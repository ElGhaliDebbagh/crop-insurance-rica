# R scripts

Run from the repository root in RStudio. Create `data/`, `figures/` and `results/` locally before running. Install the packages tidyverse, survey, fixest, knitr, kableExtra, marginaleffects, margins and lubridate. PNG table exports may require additional rendering dependencies.

Suggested order (not execution-tested):

1. `panel_script1.R`: imports annual RICA files (2000–2024), harmonises data and saves the panel.
2. `INDUM_tests.R`: checks identifier continuity using the in-memory annual data.
3. `stats_desc_figures_probit.R`: reads the saved panel and produces descriptive and exploratory analyses.
4. `Propensity_score.R`: estimates propensity scores using `rica_data`.
5. `reg3.R`: uses `panel_ps` from the preceding script.
6. `SBVCLIM.R` and `INDAS.R`: alternative shock specifications, using `rica_data` and `table_regions`. Run separately after panel construction.

`Scrip2024.R` is an exploratory single-year script requiring the 2024 RICA and regional temperature CSV files.

These are research working scripts, not a validated replication package. The thesis is the reference for final specifications and results. Paths were adapted for repository use; models were not changed. Raw data are not included in this archive.
