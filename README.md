# Brazil Solar PV Fleet

An R/Shiny dashboard of Brazil's utility-scale solar PV fleet and development pipeline, built on ANEEL open data.

Created by Fernando ([@jfer2pi](https://github.com/jfer2pi)), with coding help from Claude (Anthropic).

It is the third app in a series, alongside the [US Solar PV Fleet](https://github.com/jfer2pi/eia_solar_pv) (EIA-860M) and the [Mexico Solar PV Fleet](https://github.com/jfer2pi/mexico_pv) (GEM tracker and CNE permits). It shares the US app's layout and Linear-style dark theme: KPI cards, a bubble map of plants by stage, the capacity trajectory, capacity by state, plant size distribution, capacity by owner and a plant table. No API keys are needed anywhere.

## Data

| Source | File | Used for |
|---|---|---|
| ANEEL SIGA (required, updated daily) | `siga-empreendimentos-geracao.csv` | Every solar PV (UFV) grant: phase, MW, owners, municipality, coordinates |
| ANEEL RALIE (optional) | `ralie-usina-atual.csv` | Works status, viability, schedule, complex, market (ACR or free market) |
| ANEEL RALIE units (optional) | `ralie-unidade-geradora-atual.csv` | Expected commercial operation date per generating unit |

All three come from [dadosabertos.aneel.gov.br](https://dadosabertos.aneel.gov.br). The app tries to download them, caches the parsed result to `br_solar.rds` for 24 h, and falls back to the copies committed in this repo when ANEEL can't be reached.

**How the data is read**

- **Stages** follow SIGA's `DscFaseUsina`: Operação is Operating, Construção is Under construction, Construção não iniciada is Not started (authorised, no works yet).
- **Capacity** is ANEEL's inspected kW once the plant has been verified, the granted kW before that.
- **Owners** come from `DscPropriRegimePariticipacao`, for example "72% para X (PIE), 18% para Y (PIE)". The owner table shows equity MW (owner share times plant MW); the map and plant table show the lead owner.
- **Expected dates** for pipeline plants are the last unit's SFG forecast in RALIE, or the date in the grant act when there is no forecast.
- **Coordinates** with an obvious slip (missing minus sign, latitude and longitude swapped) are corrected. Plants without usable coordinates are left off the map but stay in the tables.

## How it was built

1. **Choosing the country.** After the US and Mexico apps, we surveyed open datasets for other markets. Canada has no national plant registry with coordinates, only stale or provincial lists. Brazil's ANEEL SIGA is a national registry of every grant with coordinates, phase and owners, the closest equivalent to EIA-860M, so Brazil came next.
2. **Starting point.** The app was built from the US app's code rather than the Mexico app's, since the US app had the cleaner structure.
3. **Sources.** We mapped the SIGA columns and added ANEEL's RALIE construction-monitoring report, which supplies the works status, viability and expected dates that SIGA lacks for the pipeline.
4. **Access from outside Brazil.** The first run timed out: ANEEL's open-data site often doesn't answer connections from outside Brazil. The app now gives up after 15 s and reads local copies of the CSVs. The files were downloaded once over a Brazilian VPN and are committed here.
5. **Checking the real files.** Inspecting the November 2026 files confirmed the format (semicolon-separated, UTF-8, decimal commas) and turned up a few quirks the app now handles:
   - SIGA writes 1900-01-03 when a plant has no operation date; dates before 1950 are treated as missing.
   - Missing coordinates are written as 0,0 (392 plants), so they fall outside Brazil and are dropped from the map.
   - Some owners are "Não Informado"; these show as "Not reported".
   - Co-owned plants separate owners with commas and can use decimal shares such as "48.6000%".
6. **Pipeline dates.** RALIE matched 1,575 of SIGA's 1,585 pipeline plants. About 950 medium-viability plants share two dates, 12 and 14 October 2031, which looks like a default ANEEL applies when there is no real schedule. Those plants stay in 2031 on the chart, but their popups call the date a horizon date shared by N plants rather than a forecast.
7. **Publishing.** The app, data and a manifest workflow were pushed to this repo for deployment on Posit Connect Cloud.

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

From outside Brazil, set `options(br_pv.offline = TRUE)` before `runApp()` to skip the download attempt and read the committed CSVs straight away.

## Deploy on Posit Connect Cloud

1. In Connect Cloud, create a new Shiny app from this GitHub repo, branch `main`, primary file `app.R`.
2. Connect Cloud reads `manifest.json` for the R version and packages. It lists `app.R` and the three CSVs.

The *Write manifest* GitHub Action regenerates `manifest.json` on every push that changes `app.R` or a CSV, and commits it back. To write it by hand:

```r
rsconnect::writeManifest(appFiles = c("app.R", list.files(pattern = "^(siga|ralie)-.*\\.csv$")))
```

## Updating the data

Connect Cloud runs outside Brazil and probably can't reach ANEEL, so the deployed app relies on the committed CSVs. To refresh it, download new copies of the three files (over a Brazilian VPN if needed), replace them in the repo under the same names, and push. The workflow rewrites the manifest and Connect Cloud redeploys.

## Credits

Plant data: ANEEL, SIGA and RALIE open data. Map data © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors, ODbL.
