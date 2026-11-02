# =============================================================================
# Brazil Solar PV Fleet: operating plants and development pipeline
# -----------------------------------------------------------------------------
# Data:  ANEEL open data (https://dadosabertos.aneel.gov.br). No API key.
#   1. SIGA, Sistema de Informações de Geração (required, updated daily).
#      siga-empreendimentos-geracao.csv: every generation project with an
#      ANEEL grant (CEG), its phase, granted/inspected kW, owners, municipality
#      and coordinates. Filtered to SigTipoGeracao = UFV (solar PV).
#        Operação                -> Operating
#        Construção              -> Under construction
#        Construção não iniciada -> Not started (authorised, no works yet)
#   2. RALIE, construction monitoring report (optional, joined by CEG core).
#        ralie-usina-atual.csv            : works status, viability, complex,
#                                           market (ACR/ACL)
#        ralie-unidade-geradora-atual.csv : per-unit commercial operation dates
#                                           (SFG forecast, else granted date),
#                                           used as the plant's expected COD
#   The app downloads all three. If ANEEL is unreachable it falls back to the
#   cache, then to copies of the CSVs saved next to app.R under the same names.
#
# Map:   OpenStreetMap tiles (dark-filtered + standard). Light use only, per
#        https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI, matching the US Solar PV Fleet app.
#
# install.packages(c("shiny","bslib","httr2","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
#
# The parsed snapshot is cached to br_solar.rds next to this file and
# re-downloaded after 24 h or from the sidebar button.
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Config -----------------------------------------------------------------

CACHE_FILE    <- "br_solar.rds"
CACHE_TTL     <- 24 * 3600
CACHE_VERSION <- 1L   # bump when the cached structure changes
CONNECT_TIMEOUT <- 15  # seconds; ANEEL is often unreachable from outside Brazil
# options(br_pv.offline = TRUE) skips the download and reads the local CSVs only

ANEEL_BASE <- "https://dadosabertos.aneel.gov.br/dataset/"
SRC <- list(
  siga = list(
    file = "siga-empreendimentos-geracao.csv",
    url  = paste0(ANEEL_BASE, "6d90b77c-c5f5-4d81-bdec-7bc619494bb9/resource/",
                  "11ec447d-698d-4ab8-977f-b424d5deee6a/download/siga-empreendimentos-geracao.csv"),
    page = paste0(ANEEL_BASE, "siga-sistema-de-informacoes-de-geracao-da-aneel")),
  ralie = list(
    file = "ralie-usina-atual.csv",
    url  = paste0(ANEEL_BASE, "57e4b8b5-a5db-40e6-9901-27ca629d0477/resource/",
                  "4a615df8-4c25-48fa-bbea-873a36a79518/download/ralie-usina-atual.csv"),
    page = paste0(ANEEL_BASE, "ralie-relatorio-de-acompanhamento-da-expansao-da-oferta-",
                  "de-geracao-de-energia-eletrica")),
  units = list(
    file = "ralie-unidade-geradora-atual.csv",
    url  = paste0(ANEEL_BASE, "57e4b8b5-a5db-40e6-9901-27ca629d0477/resource/",
                  "8a794ee4-3d41-4ce7-a80a-ae7702a16b7b/download/ralie-unidade-geradora-atual.csv"))
)

STAGES <- c("Operating", "Under construction", "Not started")
STAGE_COLS <- c(
  "Operating"          = "#5E6AD2",  # Linear indigo
  "Under construction" = "#F2994A",  # orange
  "Not started"        = "#F2C94C"   # yellow
)
PIPELINE       <- STAGES[-1]
DEFAULT_STAGES <- c("Operating", "Under construction")
CY             <- as.integer(format(Sys.Date(), "%Y"))

# Brazil bounding box, for coordinate sanity checks
BR_LAT <- c(-34.5, 5.6)
BR_LON <- c(-74.5, -28.5)

REGIME_LABELS <- c(
  "PIE" = "Independent producer (PIE)", "APE" = "Self-producer (APE)",
  "APE-COM" = "Self-producer, selling surplus (APE-COM)",
  "SP" = "Public service (SP)", "REG" = "Registered (REG)", "COM" = "Trader (COM)")

UF_NAMES <- c(
  AC = "Acre", AL = "Alagoas", AP = "Amapá", AM = "Amazonas", BA = "Bahia", CE = "Ceará",
  DF = "Distrito Federal", ES = "Espírito Santo", GO = "Goiás", MA = "Maranhão",
  MT = "Mato Grosso", MS = "Mato Grosso do Sul", MG = "Minas Gerais", PA = "Pará",
  PB = "Paraíba", PR = "Paraná", PE = "Pernambuco", PI = "Piauí", RJ = "Rio de Janeiro",
  RN = "Rio Grande do Norte", RS = "Rio Grande do Sul", RO = "Rondônia", RR = "Roraima",
  SC = "Santa Catarina", SP = "São Paulo", SE = "Sergipe", TO = "Tocantins")

# ---- Small helpers ----------------------------------------------------------

clean_names <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

norm_txt <- function(x) {
  x <- iconv(tolower(as.character(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

pick <- function(d, ...) {
  nm <- intersect(c(...), names(d))
  if (length(nm)) d[[nm[1]]] else rep(NA_character_, nrow(d))
}

# Brazilian numbers: "1.234,56" or "1234,56"; a lone dot is a decimal point
num_br <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^0-9,.-]", "", x)
  has_comma <- grepl(",", x)
  x[has_comma] <- sub(",", ".", gsub(".", "", x[has_comma], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Dates as 2019-05-30, 2019-05-30 00:00:00 or 30/05/2019
date_br <- function(x) {
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  dmy <- grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10), "%Y-%m-%d")
  out[dmy] <- as.Date(substr(x[dmy], 1, 10), "%d/%m/%Y")
  # SIGA writes 1900-01-03 when a plant has no date; nothing PV predates 1950
  out[!is.na(out) & (out < as.Date("1950-01-01") | out > as.Date("2100-01-01"))] <- NA
  out
}

na_dash <- function(x) ifelse(is.na(x) | !nzchar(trimws(x)), "-", x)

# CEG core used to join SIGA and RALIE (IdeNucleoCEG, else digits of CodCEG)
ceg_key <- function(nucleo, ceg) {
  k <- trimws(as.character(nucleo))
  alt <- sub("^.*?([0-9]{6})-?[0-9].*$", "\\1", as.character(ceg), perl = TRUE)
  alt[!grepl("^[0-9]{6}$", alt)] <- NA
  k[is.na(k) | !nzchar(k)] <- alt[is.na(k) | !nzchar(k)]
  sub("^0+(?=[0-9])", "", gsub("[^0-9]", "", k), perl = TRUE)
}

# ---- Download + read ANEEL CSVs ---------------------------------------------

# Fail fast when the host can't be reached at all (connecttimeout), but give a
# slow transfer of the larger files time to finish (timeout).
download_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  resp <- request(url) |>
    req_user_agent("shiny-br-solar-fleet-dashboard (R/httr2)") |>
    req_options(connecttimeout = CONNECT_TIMEOUT) |>
    req_timeout(300) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform(path = tmp)
  if (resp_status(resp) != 200) stop("HTTP ", resp_status(resp), " for ", url, call. = FALSE)
  if (file.size(tmp) < 1000) stop("Empty file from ", url, call. = FALSE)
  tmp
}

# ANEEL exports vary in delimiter (; or ,) and encoding (UTF-8 with BOM or
# Latin-1), so sniff both from the first bytes.
read_aneel_csv <- function(path) {
  con <- file(path, "rb"); bytes <- readBin(con, "raw", 65536); close(con)
  bom <- length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  txt <- rawToChar(bytes[bytes != as.raw(0)])
  # A cut at the buffer end can split a multibyte char; trim back to a newline
  txt <- sub("[^\n]*$", "", txt, useBytes = TRUE)
  enc <- if (bom) "UTF-8-BOM" else if (validUTF8(txt)) "UTF-8" else "latin1"
  first <- strsplit(txt, "\n", fixed = TRUE, useBytes = TRUE)[[1]][1]
  n_sep <- function(ch) sum(gregexpr(ch, first, fixed = TRUE, useBytes = TRUE)[[1]] > 0)
  sep <- if (n_sep(";") >= n_sep(",")) ";" else ","
  d <- read.csv(path, sep = sep, fileEncoding = enc, colClasses = "character",
                check.names = FALSE, quote = "\"", na.strings = c("", "NA", "NULL"),
                strip.white = TRUE, comment.char = "")
  names(d) <- make.unique(clean_names(names(d)))
  as_tibble(d)
}

# Download each source; fall back to a same-name copy in the app folder.
# All three files live on the same host, so after one connection failure the
# rest go straight to the local copies instead of timing out again.
fetch_sources <- function(optional = c("ralie", "units")) {
  out <- list(); offline <- isTRUE(getOption("br_pv.offline", FALSE)); why <- NULL
  for (k in names(SRC)) {
    s <- SRC[[k]]
    res <- if (offline) NULL else tryCatch(
      list(path = download_csv(s$url), from = "download"),
      error = function(e) {
        if (inherits(e, "httr2_failure") || grepl("connect|resolve|timed? ?out",
                                                  conditionMessage(e), ignore.case = TRUE)) {
          offline <<- TRUE
        }
        why <<- conditionMessage(e)
        NULL
      })
    if (is.null(res) && file.exists(s$file)) {
      res <- list(path = s$file, from = "local file")
      message(k, ": using local ", s$file,
              if (!is.null(why)) paste0(" (download failed: ", why, ")"))
    }
    if (is.null(res)) {
      if (k %in% optional) {
        message(k, ": not available, continuing without it")
      } else stop(
        "Could not download ", s$file, " from ANEEL",
        if (!is.null(why)) paste0(" (", why, ")"), ".\n",
        "dadosabertos.aneel.gov.br is often unreachable from outside Brazil. ",
        "Download these files from a connection that can reach it (or via a Brazilian VPN) ",
        "and save them next to app.R with the same names:\n  ",
        paste(vapply(SRC, \(x) paste0(x$file, "  <- ", x$url), ""), collapse = "\n  "),
        "\nOnly ", SRC$siga$file, " is required.", call. = FALSE)
    }
    out[k] <- list(res)
  }
  out
}

# ---- Owners -----------------------------------------------------------------
#
# DscPropriRegimePariticipacao reads like
#   "100% para Enel Green Power Brasil S.A. (PIE)"
#   "50% para Foo Energia S.A. (PIE); 50% para Bar Ltda (PIE)"
# Split before every "NN%", then strip "para" and the trailing (REGIME).

parse_owner_one <- function(x) {
  empty <- tibble(owner = character(), share = numeric(), regime = character())
  if (is.na(x) || !nzchar(trimws(x))) return(empty)
  if (grepl("^n[aã]o informad", trimws(x), ignore.case = TRUE))
    return(tibble(owner = "Not reported", share = 1, regime = NA_character_))
  pos <- gregexpr("[0-9]+(?:[.,][0-9]+)?\\s*%", x, perl = TRUE)[[1]]
  segs <- if (pos[1] == -1) x else substring(x, pos, c(pos[-1] - 1, nchar(x)))
  sh <- if (pos[1] == -1) NA_real_ else num_br(sub("%.*$", "", segs))
  rg <- regmatches(segs, regexec("\\(([A-Za-z-]{2,10})\\)\\s*[;,.]?\\s*$", segs))
  rg <- vapply(rg, \(m) if (length(m) > 1) toupper(m[2]) else NA_character_, "")
  nm <- sub("^[0-9]+(?:[.,][0-9]+)?\\s*%\\s*", "", segs, perl = TRUE)
  nm <- sub("^(para|de|do|da|-|:)\\s+", "", nm, ignore.case = TRUE)
  nm <- sub("\\s*\\([A-Za-z-]{2,10}\\)\\s*[;,.]?\\s*$", "", nm)
  nm <- trimws(sub("[;,]\\s*$", "", trimws(nm)))
  keep <- nzchar(nm)
  if (!any(keep)) return(empty)
  sh <- sh[keep]
  sh <- if (all(is.na(sh))) rep(1 / sum(keep), sum(keep)) else {
    sh[is.na(sh)] <- 0; if (sum(sh) > 0) sh / sum(sh) else rep(1 / length(sh), length(sh))
  }
  tibble(owner = nm[keep], share = sh, regime = rg[keep])
}

# Parse each distinct ownership string once, then join back to plants
owners_long <- function(key, raw) {
  raw <- coalesce(raw, "")
  u <- unique(raw)
  parsed <- bind_rows(lapply(u, \(x) {
    p <- parse_owner_one(x)
    if (!nrow(p)) p <- tibble(owner = "Unknown", share = 1, regime = NA_character_)
    mutate(p, raw = x)
  }))
  tibble(key = key, raw = raw) |>
    distinct(key, .keep_all = TRUE) |>
    inner_join(parsed, by = "raw", relationship = "many-to-many") |>
    select(key, owner, share, regime)
}

regime_label <- function(code) {
  out <- unname(REGIME_LABELS[code])
  ifelse(is.na(out), ifelse(is.na(code), NA_character_, code), out)
}

# ---- 1. SIGA ----------------------------------------------------------------

stage_from_phase <- function(x) {
  s <- norm_txt(x)
  out <- case_when(
    grepl("^operac", s)                    ~ "Operating",
    grepl("nao iniciada|nao inic", s)      ~ "Not started",
    grepl("^construc", s)                  ~ "Under construction",
    TRUE                                   ~ NA_character_   # revoked, suspended, etc.
  )
  factor(out, levels = STAGES)
}

# Fix the usual coordinate slips: missing minus signs and swapped lat/lon
fix_coords <- function(lat, lon) {
  fixed <- rep(FALSE, length(lat))
  sw <- !is.na(lat) & !is.na(lon) & abs(lat) > 28 & abs(lat) <= 74.5 & abs(lon) <= 34.5
  tmp <- lat[sw]; lat[sw] <- lon[sw]; lon[sw] <- tmp; fixed[sw] <- TRUE
  neg_lon <- !is.na(lon) & lon > 28.5 & lon < 74.5
  lon[neg_lon] <- -lon[neg_lon]; fixed[neg_lon] <- TRUE
  neg_lat <- !is.na(lat) & lat > BR_LAT[2] & lat < -BR_LAT[1]
  lat[neg_lat] <- -lat[neg_lat]; fixed[neg_lat] <- TRUE
  ok <- !is.na(lat) & !is.na(lon) & lat >= BR_LAT[1] & lat <= BR_LAT[2] &
        lon >= BR_LON[1] & lon <= BR_LON[2]
  list(lat = ifelse(ok, lat, NA_real_), lon = ifelse(ok, lon, NA_real_), fixed = fixed & ok)
}

read_siga <- function(path) {
  d <- read_aneel_csv(path)
  d <- d[toupper(trimws(pick(d, "sigtipogeracao"))) == "UFV", ]
  if (!nrow(d)) stop("SIGA file has no UFV (solar PV) rows: ", path, call. = FALSE)

  # Keep the record in force for each CEG (drop superseded versions, if any)
  fim <- date_br(pick(d, "datfimvigencia"))
  d <- d[is.na(fim) | fim >= Sys.Date(), ]
  ini <- date_br(pick(d, "datiniciovigencia"))
  d <- d[order(pick(d, "codceg"), ini, decreasing = TRUE, na.last = TRUE), ]
  d <- d[!duplicated(pick(d, "codceg")) | is.na(pick(d, "codceg")), ]

  kw_out <- num_br(pick(d, "mdapotenciaoutorgadakw"))
  kw_fis <- num_br(pick(d, "mdapotenciafiscalizadakw"))
  xy <- fix_coords(num_br(pick(d, "numcoordnempreendimento")),
                   num_br(pick(d, "numcoordeempreendimento")))

  g <- tibble(
    ceg        = trimws(pick(d, "codceg")),
    key        = ceg_key(pick(d, "idenucleoceg"), pick(d, "codceg")),
    plant      = trimws(pick(d, "nomempreendimento")),
    state      = toupper(trimws(pick(d, "sigufprincipal"))),
    municipality = trimws(pick(d, "dscmuninicpios", "dscmunicipios")),
    phase      = trimws(pick(d, "dscfaseusina")),
    grant_type = trimws(pick(d, "dsctipooutorga")),
    owners_raw = trimws(pick(d, "dscpropriregimepariticipacao", "dscpropriregimeparticipacao")),
    cod        = date_br(pick(d, "datentradaoperacao")),
    mw_granted = kw_out / 1000,
    mw_inspected = kw_fis / 1000,
    lat = xy$lat, lon = xy$lon, coord_fixed = xy$fixed,
    snapshot   = date_br(pick(d, "datgeracaoconjuntodados"))
  ) |>
    mutate(stage = stage_from_phase(phase),
           # Inspected kW once ANEEL has verified the plant, granted kW before
           mw = ifelse(!is.na(mw_inspected) & mw_inspected > 0, mw_inspected, mw_granted),
           key = coalesce(na_if(key, ""), ceg, paste0("row", row_number()))) |>
    distinct(key, .keep_all = TRUE)

  n_all <- nrow(g)
  g <- filter(g, !is.na(stage), !is.na(mw), mw > 0)
  list(g = g,
       qc = list(n_ufv = n_all, n_kept = nrow(g),
                 n_no_coords = sum(is.na(g$lat)), n_fixed = sum(g$coord_fixed)))
}

# ---- 2. RALIE (optional) ----------------------------------------------------

read_ralie <- function(path) {
  d <- read_aneel_csv(path)
  tg <- toupper(trimws(pick(d, "sigtipogeracao")))
  if (any(tg == "UFV", na.rm = TRUE)) d <- d[tg %in% "UFV", ]
  dr <- date_br(pick(d, "datralie"))
  r <- tibble(
    key          = ceg_key(pick(d, "idenucleoceg"), pick(d, "codceg")),
    ralie_date   = dr,
    works        = trimws(pick(d, "dscsituacaoobra")),
    viability    = trimws(pick(d, "dscviabilidade")),
    schedule     = trimws(pick(d, "dscsituacaocronograma")),
    market       = trimws(pick(d, "dsccomercializacaoenergia")),
    complex      = trimws(pick(d, "nomcomplexo")),
    connection   = trimws(pick(d, "nomconexao")),
    works_start  = date_br(pick(d, "datinicioobrarealizado")),
    li_status    = trimws(pick(d, "dscsituacaoli"))
  )
  r |> filter(!is.na(key), nzchar(key)) |>
    arrange(key, desc(ralie_date)) |> distinct(key, .keep_all = TRUE)
}

# Plant expected COD = last unit's commercial operation date
# (SFG forecast where given, else the date in the grant act)
read_units <- function(path) {
  d <- read_aneel_csv(path)
  tg <- toupper(trimws(pick(d, "sigtipogeracao")))
  if (any(tg == "UFV", na.rm = TRUE)) d <- d[tg %in% "UFV", ]
  tibble(
    key  = ceg_key(pick(d, "idenucleoceg"), pick(d, "codceg")),
    sfg  = date_br(pick(d, "datprevisaoopcomercialsfg")),
    act  = date_br(pick(d, "datuginicioopcomeroutorgado"))
  ) |>
    mutate(exp = coalesce(sfg, act)) |>
    filter(!is.na(key), nzchar(key), !is.na(exp)) |>
    group_by(key) |>
    summarise(expected_cod = max(exp), cod_source = if (any(!is.na(sfg))) "SFG forecast" else "grant act",
              .groups = "drop") |>
    # SFG parks hundreds of plants on one horizon date (e.g. 2031-10-12) when
    # it has no real schedule for them. Keep the date, but say what it is.
    add_count(expected_cod, name = "n_same") |>
    mutate(cod_source = ifelse(cod_source == "SFG forecast" & n_same >= 50,
                               sprintf("SFG horizon date shared by %d plants", n_same),
                               cod_source)) |>
    select(-n_same)
}

# ---- Build + cache ----------------------------------------------------------

build_data <- function(src = fetch_sources()) {
  sg <- read_siga(src$siga$path)
  g <- sg$g

  ralie <- if (!is.null(src$ralie)) tryCatch(read_ralie(src$ralie$path), error = \(e) {
    warning("RALIE plant file not read: ", conditionMessage(e), call. = FALSE); NULL })
  units <- if (!is.null(src$units)) tryCatch(read_units(src$units$path), error = \(e) {
    warning("RALIE unit file not read: ", conditionMessage(e), call. = FALSE); NULL })

  g <- if (!is.null(ralie)) left_join(g, ralie, by = "key") else
    mutate(g, ralie_date = as.Date(NA), works = NA_character_, viability = NA_character_,
           schedule = NA_character_, market = NA_character_, complex = NA_character_,
           connection = NA_character_, works_start = as.Date(NA), li_status = NA_character_)
  g <- if (!is.null(units)) left_join(g, units, by = "key") else
    mutate(g, expected_cod = as.Date(NA), cod_source = NA_character_)

  # Operating plants: actual COD. Pipeline: expected COD from RALIE.
  g <- g |>
    mutate(online = if_else(stage == "Operating", cod, expected_cod),
           year = as.integer(format(online, "%Y")),
           month = as.integer(format(online, "%m")))

  O <- owners_long(g$key, g$owners_raw)
  lead <- O |> arrange(key, desc(share)) |> distinct(key, .keep_all = TRUE)
  g <- g |>
    left_join(transmute(lead, key, owner = owner, regime_code = regime), by = "key") |>
    mutate(owner = coalesce(owner, "Unknown"),
           regime = regime_label(regime_code),
           n_owners = as.integer(table(O$key)[key]))

  list(version = CACHE_VERSION, gens = g, owners = O, qc = sg$qc,
       period = suppressWarnings(max(g$snapshot, na.rm = TRUE)),
       ralie_period = if (!is.null(ralie)) suppressWarnings(max(ralie$ralie_date, na.rm = TRUE)) else NA,
       has_ralie = !is.null(ralie), has_units = !is.null(units),
       from = vapply(src, \(s) if (is.null(s)) "missing" else s$from, ""),
       fetched = Sys.time())
}

# Cache logic:
#  - no usable cache -> download
#  - cache < 24 h    -> use it as is
#  - cache >= 24 h   -> re-download; on failure keep the cache and reset its clock
load_data <- function() {
  cached <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
  if (!is.null(cached) && !identical(cached$version, CACHE_VERSION)) cached <- NULL
  if (!is.null(cached)) {
    age <- difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs")
    if (age < CACHE_TTL) return(cached)
  }
  out <- tryCatch(build_data(), error = function(e) {
    if (is.null(cached)) stop(e)
    warning("ANEEL refresh failed, keeping cached data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(out)) {
    Sys.setFileTime(CACHE_FILE, Sys.time())
    return(cached)
  }
  saveRDS(out, CACHE_FILE)
  out
}

# Everything the app derives from a snapshot, so a refresh can swap it whole
prep_data <- function(s) {
  # Pipeline plants whose expected COD has already passed are shown as this year
  G <- s$gens |>
    mutate(year_plot = ifelse(stage != "Operating" & !is.na(year) & year < CY, CY, year))
  yrs <- G$year_plot[!is.na(G$year_plot)]
  list(
    raw          = s,
    G            = G,
    period_label = if (is.finite(s$period)) format(s$period, "%d %b %Y") else "latest",
    yr_rng       = if (length(yrs)) range(yrs) else c(CY - 10, CY + 3),
    total_op     = sum(G$mw[G$stage == "Operating"]),
    site_max     = max(1, ceiling(max(G$mw))),
    states       = sort(unique(na.omit(G$state))),
    regimes      = sort(unique(na.omit(G$regime))),
    viabilities  = sort(unique(na.omit(G$viability)))
  )
}

# Prepped snapshot, memoised on download time so sessions share one copy
current_data <- local({
  key <- NULL; val <- NULL
  function() {
    s <- load_data()
    k <- format(s$fetched, "%Y-%m-%d %H:%M:%OS3")
    if (!identical(k, key)) { val <<- prep_data(s); key <<- k }
    val
  }
})
invisible(current_data())  # fail fast at startup if ANEEL is unreachable and no cache

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

# Bubble area proportional to MW (radius ~ sqrt(MW)), pixel radius
bubble_r <- function(mw) pmin(pmax(sqrt(mw) * 0.95, 2.5), 34)

theme <- bs_theme(
  version = 5, bg = lin$bg, fg = lin$text, primary = lin$accent,
  secondary = lin$raised,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  font_scale = 0.88,
  `border-radius` = "6px", `border-color` = lin$border,
  `card-bg` = lin$surface, `card-border-color` = lin$border,
  `input-bg` = lin$raised, `input-border-color` = lin$border
) |>
  bs_add_rules(sprintf("
  body { font-feature-settings:'cv11','ss01','tnum'; letter-spacing:-0.005em; }
  .navbar { background:%1$s !important; border-bottom:1px solid %4$s; }
  .brand { display:flex; align-items:center; gap:10px; font-weight:600; font-size:0.95rem; }
  .brand .logo { width:18px; height:18px; border-radius:5px;
                 background:linear-gradient(135deg,%5$s 0%%,#A5ABF2 100%%); }
  .brand .tag { color:%2$s; font-weight:400; font-size:0.78rem;
                border:1px solid %4$s; border-radius:999px; padding:1px 8px; }
  .bslib-sidebar-layout > .sidebar { border-right:1px solid %4$s !important; }
  .sec { color:%3$s; font-size:0.7rem; font-weight:500; text-transform:uppercase;
         letter-spacing:0.06em; margin:4px 0 6px; }
  .card { box-shadow:none; }
  .card-header { background:transparent; border-bottom:1px solid %4$s; font-size:0.8rem;
                 font-weight:500; color:%6$s; padding:10px 14px; }
  .card-header .sub { color:%3$s; font-weight:400; margin-left:6px; }
  .kpi { background:%7$s; border:1px solid %4$s; border-radius:8px; padding:14px 16px; height:100%%; }
  .kpi .lbl { color:%2$s; font-size:0.72rem; margin-bottom:6px; display:flex; align-items:center; gap:6px; }
  .kpi .dot { width:7px; height:7px; border-radius:50%%; display:inline-block; }
  .kpi .val { font-size:1.55rem; font-weight:600; letter-spacing:-0.02em; line-height:1.1; }
  .kpi .foot { color:%3$s; font-size:0.72rem; margin-top:6px; white-space:nowrap;
               overflow:hidden; text-overflow:ellipsis; }
  .form-label, .control-label { color:%2$s; font-size:0.75rem; font-weight:500; }
  .form-control, .selectize-input { font-size:0.82rem; }
  .selectize-input, .selectize-dropdown { background:#141516 !important; border-color:%4$s !important; }
  .selectize-input .item { background:rgba(94,106,210,0.18) !important; color:#C9CDFB !important;
                           border:0 !important; border-radius:4px !important; }
  .form-check-input { background-color:#141516; border-color:rgba(255,255,255,0.18); }
  .form-check-input:checked { background-color:%5$s; border-color:%5$s; }
  .form-check-label { font-size:0.8rem; }
  .stage-sw { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .irs--shiny .irs-bar { background:%5$s; border-color:%5$s; }
  .irs--shiny .irs-line { background:#1C1D1F; border-color:%4$s; }
  .irs--shiny .irs-handle { background:%6$s; border:0; box-shadow:0 0 0 3px rgba(94,106,210,0.35); }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%5$s; font-size:0.7rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background:#1C1D1F; color:%2$s; font-size:0.68rem; }
  .btn-lin { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.78rem; }
  .btn-lin:hover { background:#1C1D1F; color:%6$s; }
  .meta { color:%3$s; font-size:0.72rem; line-height:1.5; }
  .meta a { color:%2$s; }
  /* OpenStreetMap standard tiles, recoloured to sit in the dark UI */
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
  .pop-k { color:%2$s; }
  .pop-stage { display:inline-block; font-size:0.68rem; padding:1px 7px; border-radius:999px;
               border:1px solid currentColor; margin-bottom:4px; }
  .info.legend, .size-legend { background:rgba(15,16,17,0.92) !important; color:%6$s !important;
       border:1px solid %4$s; border-radius:6px; font-size:0.72rem; box-shadow:none; padding:8px 10px; }
  .size-legend .t { color:%2$s; margin-bottom:4px; }
  .leaflet-bar a, .leaflet-control-layers { background:#141516; color:%6$s; border-color:%4$s; }
  .leaflet-control-layers-expanded { font-size:0.75rem; }
  .leaflet-control-attribution { background:rgba(8,9,10,0.75) !important; color:%3$s; }
  .leaflet-control-attribution a { color:%2$s; }
  table.dataTable { font-size:0.78rem; }
  table.dataTable thead th { color:%2$s; font-weight:500; border-bottom:1px solid %4$s !important; }
  table.dataTable tbody td { border-top:1px solid rgba(255,255,255,0.04) !important; }
  table.dataTable tbody tr:hover > * { background:rgba(255,255,255,0.03) !important; }
  table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(94,106,210,0.22) !important;
       color:%6$s !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_filter label { color:%3$s; font-size:0.74rem; }
  .dt-buttons .btn { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.74rem; padding:3px 10px; }
  .page-link { background:transparent; border-color:%4$s; color:%2$s; }
  .active > .page-link { background:%5$s; border-color:%5$s; }
  ", lin$bg, lin$muted, lin$faint, lin$border, lin$accent, lin$text, lin$surface))

kpi <- function(label, color, value_id, foot_id) {
  div(class = "kpi",
      div(class = "lbl", span(class = "dot", style = paste0("background:", color)), label),
      div(class = "val", textOutput(value_id, inline = TRUE)),
      div(class = "foot", textOutput(foot_id, inline = TRUE)))
}

plotly_lin <- function(p, ...) {
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif", color = lin$muted, size = 11),
    margin = list(l = 8, r = 8, t = 6, b = 6),
    hoverlabel = list(bgcolor = lin$raised, bordercolor = "#2A2B2E",
                      font = list(color = lin$text, family = "Inter")),
    xaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 linecolor = lin$border, tickfont = list(color = lin$faint)),
    yaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 tickfont = list(color = lin$faint)),
    showlegend = FALSE
  )
  do.call(layout, c(list(p), modifyList(base, list(...)))) |>
    config(displayModeBar = FALSE)
}

fmt_mw <- function(x) {
  ifelse(x >= 1000, sprintf("%.1f GW", x / 1000),
         paste(formatC(x, format = "f", digits = 1, big.mark = ","), "MW"))
}

fmt_date <- function(d) ifelse(is.na(d), "-", format(d, "%b %Y"))

stage_labels <- lapply(STAGES, \(s) HTML(sprintf(
  "<span class='stage-sw' style='background:%s'></span>%s", STAGE_COLS[[s]], s)))

size_legend_html <- function() {
  refs <- c(10, 100, 500)
  r <- bubble_r(refs); h <- 2 * max(r) + 4
  circles <- paste0(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' stroke='#8A8F98' stroke-width='1'/>
     <text x='%.1f' y='%.1f' fill='#8A8F98' font-size='10' text-anchor='middle'>%s</text>",
    max(r) + 2, h - r - 2, r, max(r) + 2, h - 2 * r - 5,
    c("10", "100", "500 MW")), collapse = "")
  sprintf("<div class='size-legend'><div class='t'>Bubble area = MW</div>
          <svg width='%d' height='%d'>%s</svg></div>", ceiling(2 * max(r) + 30), ceiling(h + 12), circles)
}

state_choices <- function(codes) {
  setNames(codes, ifelse(is.na(UF_NAMES[codes]), codes, paste0(codes, " · ", UF_NAMES[codes])))
}

# ---- UI ---------------------------------------------------------------------

# UI is a function so every new visitor gets controls built from the newest snapshot
ui <- function(req) {
d <- current_data()
yr_rng <- d$yr_rng; SITE_MW_MAX <- d$site_max

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "Brazil Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "Brazil Solar PV Fleet",
              span(class = "tag", "ANEEL SIGA ·", textOutput("period_tag", inline = TRUE),
                   if (isTRUE(d$raw$has_ralie)) " · RALIE")),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES, selected = DEFAULT_STAGES),
    div(class = "sec", "Filters"),
    selectizeInput("state", "State", choices = state_choices(d$states), multiple = TRUE,
                   options = list(placeholder = "All states")),
    if (length(d$regimes))
      selectizeInput("regime", "Regime (lead owner)", choices = d$regimes, multiple = TRUE,
                     options = list(placeholder = "All regimes")),
    if (length(d$viabilities))
      selectizeInput("viab", "Viability (RALIE, pipeline only)", choices = d$viabilities,
                     multiple = TRUE, options = list(placeholder = "All")),
    sliderInput("mw", "Plant size (MW)", min = 0, max = SITE_MW_MAX,
                value = c(1, SITE_MW_MAX), step = 1, ticks = FALSE),
    sliderInput("yr", "Online year (actual or expected)", min = yr_rng[1], max = yr_rng[2],
                value = yr_rng, step = 1, sep = "", ticks = FALSE),
    checkboxInput("no_year", "Include plants without a date", value = TRUE),
    textInput("q", "Search plant, owner or complex", placeholder = "e.g. Enel"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    actionButton("refresh", "Re-download from ANEEL", icon = icon("rotate"),
                 class = "btn-lin w-100"),
    div(style = "height:10px"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Operating",          STAGE_COLS[["Operating"]],          "k_op",  "k_op_f"),
    kpi("Under construction", STAGE_COLS[["Under construction"]], "k_uc",  "k_uc_f"),
    kpi("Authorised, not started", STAGE_COLS[["Not started"]],   "k_pl",  "k_pl_f"),
    kpi("Largest operating plant", "#D4D8FF",                     "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "640px",
    card_header("Plants", span(class = "sub",
      "one bubble per ANEEL grant (CEG) · filled = operating, dashed ring = pipeline")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "annual additions by year online · cumulative fleet if pipeline stages complete")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "420px", full_screen = TRUE,
         card_header("Capacity by state", span(class = "sub", "top 15, selected stages")),
         card_body(plotlyOutput("by_state", height = "100%"))),
    card(height = "420px", full_screen = TRUE,
         card_header("Plant size distribution", span(class = "sub", "count of plants by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(
    full_screen = TRUE,
    card_header("Capacity by owner", span(class = "sub",
      "equity MW (owner share × plant MW) from the SIGA ownership field · respects all filters")),
    card_body(DTOutput("owners"))
  ),

  card(
    full_screen = TRUE,
    card_header("Plants", span(class = "sub", "select a row to fly to it on the map")),
    card_body(DTOutput("sites_tbl"))
  )
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  D <- reactiveVal(current_data())

  output$period_tag <- renderText(D()$period_label)

  output$meta <- renderUI({
    s <- D()$raw; qc <- s$qc
    div(class = "meta",
        "Source: ANEEL, ", a("SIGA", href = SRC$siga$page, target = "_blank"),
        " (snapshot ", D()$period_label, "), solar PV (UFV) grants. ",
        sprintf("%s plants; %s without usable coordinates are left off the map, %s had a sign or lat/lon swap corrected. ",
                format(qc$n_kept, big.mark = ","), format(qc$n_no_coords, big.mark = ","),
                format(qc$n_fixed, big.mark = ",")),
        "MW is ANEEL's inspected capacity once verified, granted capacity before.",
        br(), br(),
        if (isTRUE(s$has_ralie) || isTRUE(s$has_units)) tagList(
          a("RALIE", href = SRC$ralie$page, target = "_blank"),
          if (!is.na(s$ralie_period) && is.finite(s$ralie_period))
            sprintf(" (%s)", format(s$ralie_period, "%b %Y")),
          ": works status, viability and complex for pipeline plants; expected COD from the ",
          "last unit's SFG forecast, else the grant act date. Pipeline dates already past are shown in ",
          CY, ".")
        else "RALIE not available this build: pipeline plants have no expected dates.",
        br(), br(),
        "Map data © ", a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright",
                         target = "_blank"), " contributors.",
        br(), sprintf("Downloaded %s%s", format(s$fetched, "%Y-%m-%d %H:%M"),
                      if (any(s$from == "local file")) " (partly from local CSVs)" else ""))
  })

  sync_inputs <- function(d) {
    updateSelectizeInput(session, "state", choices = state_choices(d$states),
                         selected = intersect(input$state, d$states))
    updateSelectizeInput(session, "regime", choices = d$regimes,
                         selected = intersect(input$regime, d$regimes))
    updateSelectizeInput(session, "viab", choices = d$viabilities,
                         selected = intersect(input$viab, d$viabilities))
    updateSliderInput(session, "mw", max = d$site_max,
                      value = c(min(input$mw[1], d$site_max), d$site_max))
    updateSliderInput(session, "yr", min = d$yr_rng[1], max = d$yr_rng[2], value = d$yr_rng)
  }

  observeEvent(input$refresh, {
    id <- showNotification("Downloading SIGA and RALIE from ANEEL…", duration = NULL,
                           type = "message")
    on.exit(removeNotification(id), add = TRUE)
    updateActionButton(session, "refresh", label = "Downloading…")
    on.exit(updateActionButton(session, "refresh", label = "Re-download from ANEEL"), add = TRUE)

    res <- tryCatch(build_data(), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Refresh failed:", conditionMessage(res)), type = "error",
                       duration = 8)
    } else {
      saveRDS(res, CACHE_FILE)
      new <- prep_data(res)
      D(new)
      sync_inputs(new)
      showNotification(sprintf("Updated to SIGA snapshot %s.", new$period_label),
                       type = "message", duration = 5)
    }
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages", selected = DEFAULT_STAGES)
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "regime", selected = character(0))
    updateSelectizeInput(session, "viab", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$site_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateCheckboxInput(session, "no_year", value = TRUE)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  # Plant-level rows after attribute filters (all stages)
  plants_all <- reactive({
    d <- D()$G |>
      filter((is.na(year_plot) & isTRUE(input$no_year)) |
             (!is.na(year_plot) & year_plot >= input$yr[1] & year_plot <= input$yr[2]),
             mw >= input$mw[1], mw <= input$mw[2])
    if (length(input$state))  d <- filter(d, state %in% input$state)
    if (length(input$regime)) d <- filter(d, regime %in% input$regime)
    if (length(input$viab))   d <- filter(d, stage == "Operating" | viability %in% input$viab)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) |
                                  grepl(q, owners_raw, ignore.case = TRUE) |
                                  grepl(q, coalesce(complex, ""), ignore.case = TRUE))
    d
  })

  plants <- reactive({
    plants_all() |>
      filter(stage %in% input$stages) |>
      mutate(size_class = cut(mw, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
             radius = bubble_r(mw),
             online_lbl = fmt_date(online)) |>
      arrange(desc(mw))
  })

  # KPIs (ignore the layer toggles so they always tell the full story)
  stage_sum <- function(st) {
    s <- plants_all() |> filter(stage %in% st)
    list(mw = sum(s$mw), n = nrow(s))
  }
  output$k_op   <- renderText(fmt_mw(stage_sum("Operating")$mw))
  output$k_op_f <- renderText(sprintf("%s plants · %.0f%% of BR utility PV",
    format(stage_sum("Operating")$n, big.mark = ","),
    100 * stage_sum("Operating")$mw / max(D()$total_op, 1)))
  output$k_uc   <- renderText(fmt_mw(stage_sum("Under construction")$mw))
  output$k_uc_f <- renderText({
    s <- plants_all() |> filter(stage == "Under construction")
    if (!nrow(s)) return("no plants")
    yrs <- na.omit(s$year_plot)
    if (length(yrs)) sprintf("%s plants · online %s to %s", nrow(s), min(yrs), max(yrs))
    else sprintf("%s plants", nrow(s))
  })
  output$k_pl   <- renderText(fmt_mw(stage_sum("Not started")$mw))
  output$k_pl_f <- renderText({
    s <- stage_sum("Not started"); op <- stage_sum("Operating")$mw
    sprintf("%s plants · %s of today's fleet", format(s$n, big.mark = ","),
            if (op > 0) sprintf("%.0f%%", 100 * s$mw / op) else "-")
  })
  big <- reactive(plants_all() |> filter(stage == "Operating") |>
                    slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, big()$state) else "")

  # ---- Bubble map (OpenStreetMap) ----
  output$map <- renderLeaflet({
    osm_attr <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    osm_url  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM dark",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM standard",
               options = tileOptions(maxZoom = 19)) |>
      addLayersControl(baseGroups = c("OSM dark", "OSM standard"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(-50, -14, zoom = 4) |>
      addLegend("bottomright", colors = unname(STAGE_COLS), labels = STAGES,
                title = "Stage", opacity = 1) |>
      addControl(html = HTML(size_legend_html()), position = "bottomleft",
                 className = "size-legend-wrap")
  })

  observe({
    d <- plants() |> filter(!is.na(lat), !is.na(lon)) |> mutate(stage = as.character(stage))
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[d$stage])
    is_op <- d$stage == "Operating"

    ralie_html <- ifelse(is_op | (is.na(d$works) & is.na(d$viability)), "",
      sprintf("<br><span class='pop-k'>Works</span> %s &middot; <span class='pop-k'>Viability</span> %s<br>
               <span class='pop-k'>Schedule</span> %s &middot; <span class='pop-k'>Market</span> %s",
              htmlEscape(na_dash(d$works)), htmlEscape(na_dash(d$viability)),
              htmlEscape(na_dash(d$schedule)), htmlEscape(na_dash(d$market))))
    complex_html <- ifelse(is.na(d$complex) | !nzchar(d$complex), "",
      sprintf("<br><span class='pop-k'>Complex</span> %s", htmlEscape(d$complex)))
    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b><br>
       <span class='pop-k'>Owners</span> %s<br>
       <span class='pop-k'>Location</span> %s, %s<br>
       <span class='pop-k'>Capacity</span> %s MW%s<br>
       <span class='pop-k'>%s</span> %s<br>
       <span class='pop-k'>Phase</span> %s &middot; <span class='pop-k'>Grant</span> %s<br>
       <span class='pop-k'>CEG</span> %s%s%s",
      col, d$stage, htmlEscape(d$plant), htmlEscape(na_dash(d$owners_raw)),
      htmlEscape(na_dash(d$municipality)), d$state,
      formatC(d$mw, format = "f", digits = 1, big.mark = ","),
      ifelse(!is.na(d$mw_granted) & abs(d$mw_granted - d$mw) > 0.05,
             sprintf(" (granted %s)", formatC(d$mw_granted, format = "f", digits = 1)), ""),
      ifelse(is_op, "In service", ifelse(is.na(d$cod_source), "Expected online",
                                         paste0("Expected online (", d$cod_source, ")"))),
      d$online_lbl, htmlEscape(na_dash(d$phase)), htmlEscape(na_dash(d$grant_type)),
      htmlEscape(na_dash(d$ceg)), complex_html, ralie_html)
    lab <- sprintf("%s · %s MW · %s", d$plant, formatC(d$mw, format = "f", digits = 1), d$stage)

    # Operating: solid bubbles. Pipeline: dashed rings drawn on top.
    if (any(is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[is_op], lat = d$lat[is_op], radius = d$radius[is_op],
        group = "sites", layerId = paste0(d$key[is_op], "_op"),
        stroke = TRUE, weight = 0.8, color = col[is_op], opacity = 0.9,
        fillColor = col[is_op], fillOpacity = 0.45,
        popup = pop[is_op], label = lab[is_op])
    }
    if (any(!is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[!is_op], lat = d$lat[!is_op], radius = d$radius[!is_op],
        group = "sites", layerId = paste0(d$key[!is_op], "_", d$stage[!is_op]),
        stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!is_op], opacity = 0.95,
        fillColor = col[!is_op], fillOpacity = 0.10,
        popup = pop[!is_op], label = lab[!is_op])
    }
  })

  # ---- Capacity trajectory: history + pipeline ----
  output$trajectory <- renderPlotly({
    g <- plants_all() |> filter(!is.na(year_plot), stage %in% input$stages)
    shiny::validate(shiny::need(nrow(g) > 0, "No capacity with a date for the current filters."))

    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |>
      group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))

    p <- plot_ly()
    for (s in intersect(STAGES, input$stages)) {
      a <- filter(annual, stage == s, gw > 0)
      if (!nrow(a)) next
      p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
                         marker = list(color = STAGE_COLS[[s]],
                                       opacity = if (s == "Operating") 1 else 0.85),
                         hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }

    op <- annual |> filter(stage == "Operating") |> arrange(year_plot) |>
      mutate(cum = cumsum(gw))
    hist_end <- max(c(g$year_plot[g$stage == "Operating"], CY - 1), na.rm = TRUE)
    base <- sum(op$gw)
    if ("Operating" %in% input$stages) {
      h <- filter(op, year_plot <= hist_end)
      p <- p |> add_lines(data = h, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = "Operating fleet (cumulative)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.1f} GW installed<extra></extra>")
    } else base <- 0

    fut_years <- yrs[yrs >= hist_end]
    pipe_on <- intersect(PIPELINE, input$stages)
    scen <- list(c("Under construction"), PIPELINE)
    scen_name <- c("+ under construction", "+ not started")
    dash <- c("dash", "dot")
    for (i in seq_along(scen)) {
      st <- intersect(scen[[i]], pipe_on)
      if (!length(st) || (i > 1 && setequal(st, intersect(scen[[i - 1]], pipe_on)))) next
      add <- annual |> filter(stage %in% st, year_plot >= hist_end) |>
        group_by(year_plot) |> summarise(gw = sum(gw), .groups = "drop") |>
        right_join(tibble(year_plot = fut_years), by = "year_plot") |>
        mutate(gw = coalesce(gw, 0)) |> arrange(year_plot) |>
        mutate(cum = base + cumsum(gw))
      add <- bind_rows(tibble(year_plot = hist_end, cum = base), add)
      p <- p |> add_lines(data = add, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = scen_name[i],
                          line = list(color = STAGE_COLS[[st[length(st)]]], width = 1.6,
                                      dash = dash[i]),
                          hovertemplate = paste0(scen_name[i], " · %{x}: %{y:.1f} GW<extra></extra>"))
    }

    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, tickfont = list(color = lin$faint),
                   range = c(max(min(yrs), CY - 16) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GW added per year", font = list(size = 10)),
                   gridcolor = "rgba(255,255,255,0.05)", tickfont = list(color = lin$faint)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GW", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = CY - 0.5, x1 = CY - 0.5, y0 = 0, y1 = 1,
                         line = list(color = "rgba(255,255,255,0.25)", width = 1, dash = "dot"))),
      annotations = list(list(x = CY - 0.5, y = 1, xref = "x", yref = "paper",
                              text = paste(CY, "→ pipeline"), showarrow = FALSE,
                              xanchor = "left", yanchor = "top", xshift = 4,
                              font = list(size = 10, color = lin$faint))),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_state <- renderPlotly({
    s <- plants_all() |> filter(stage %in% input$stages) |>
      group_by(state, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop")
    shiny::validate(shiny::need(nrow(s) > 0, "No plants for the current filters."))
    top <- s |> group_by(state) |> summarise(t = sum(gw)) |> slice_max(t, n = 15) |> arrange(t)
    s <- filter(s, state %in% top$state) |> mutate(state = factor(state, top$state))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage)))) {
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~state,
                         orientation = "h", name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         text = ~unname(UF_NAMES[as.character(state)]), textposition = "none",
                         hovertemplate = paste0(st, " · %{text}: %{x:.2f} GW<extra></extra>"))
    }
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GW", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- plants() |> count(size_class, stage, .drop = FALSE) |>
      filter(stage %in% input$stages)
    shiny::validate(shiny::need(sum(z$n) > 0, "No plants for the current filters."))
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages)) {
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} plants<extra></extra>"))
    }
    p |> plotly_lin(barmode = "group", bargap = 0.25, bargroupgap = 0.08,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)),
                    yaxis = list(title = ""))
  })

  # ---- Capacity by owner (equity MW) ----
  owner_tbl <- reactive({
    p <- plants_all() |> select(key, plant, state, stage, mw)
    D()$raw$owners |>
      inner_join(p, by = "key", relationship = "many-to-many") |>
      mutate(emw = mw * share) |>
      group_by(Owner = owner) |>
      summarise(
        `Operating plants`   = n_distinct(key[stage == "Operating"]),
        `Operating MW`       = sum(emw[stage == "Operating"]),
        `Under constr. MW`   = sum(emw[stage == "Under construction"]),
        `Not started MW`     = sum(emw[stage == "Not started"]),
        States = n_distinct(state),
        `Largest plant` = {
          o <- stage == "Operating"
          if (any(o)) sprintf("%s (%s MW)", plant[o][which.max(mw[o])],
                              formatC(max(mw[o]), format = "f", digits = 0))
          else "-"
        },
        .groups = "drop") |>
      mutate(`Total incl. pipeline` = `Operating MW` + `Under constr. MW` + `Not started MW`,
             `Share of operating` = `Operating MW` / max(sum(`Operating MW`), 1)) |>
      relocate(`Share of operating`, .after = `Operating MW`) |>
      arrange(desc(`Operating MW`), desc(`Total incl. pipeline`))
  })

  output$owners <- renderDT({
    d <- owner_tbl()
    datatable(
      d, rownames = FALSE, selection = "none", extensions = "Buttons",
      style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 15, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(2, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(1:6, 8))))
    ) |>
      formatRound(c("Operating MW", "Under constr. MW", "Not started MW",
                    "Total incl. pipeline"), digits = 0) |>
      formatPercentage("Share of operating", digits = 1) |>
      formatStyle("Operating MW",
                  background = styleColorBar(c(0, max(d$`Operating MW`, 1)),
                                             "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Under constr. MW",
                  background = styleColorBar(c(0, max(d$`Under constr. MW`, 1)),
                                             "rgba(242,153,74,0.30)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- Plant table ----
  output$sites_tbl <- renderDT({
    datatable(
      plants() |> transmute(Plant = plant, Stage = stage, `Lead owner` = owner,
                            Owners = n_owners, State = state, Municipality = municipality,
                            MW = round(mw, 1), Online = online_lbl, Regime = regime,
                            Grant = grant_type, Works = works, Viability = viability,
                            Complex = complex, CEG = ceg,
                            Map = ifelse(is.na(lat), "no coords", "")),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(6, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(3, 6))))
    ) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS)))
  })

  observeEvent(input$sites_tbl_rows_selected, {
    r <- plants()[input$sites_tbl_rows_selected, ]
    if (is.na(r$lat)) {
      showNotification("This plant has no usable coordinates in SIGA.", duration = 4)
      return()
    }
    leafletProxy("map") |>
      flyTo(r$lon, r$lat, zoom = 13, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = r$radius + 6, group = "focus",
                       stroke = TRUE, color = "#FFFFFF", weight = 1.5, opacity = 0.9,
                       fill = FALSE)
  })
}

shinyApp(ui, server)