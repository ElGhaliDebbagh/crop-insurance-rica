# Data documentation

## Data source

The main data source is the French Farm Accountancy Data Network (RICA — Réseau d'information comptable agricole), published by Agreste, the statistical service of the French Ministry of Agriculture.

Official source: [Agreste — RICA public anonymised microdata](https://agreste.agriculture.gouv.fr/agreste-web/servicon/I.2/listeTypeServicon/)

## Raw data

| Characteristic | Description |
|---|---|
| Available period | 2000–2024 |
| Main study period | 2002–2024 |
| Annual data files | 25 |
| Total raw data volume | Approximately 500 MB |
| File formats | TXT and CSV |
| Statistical unit | Agricultural holding |
| Processing language | R |

## Main variables

- Agricultural holding identifiers
- Geographical location
- Farm economic size
- Type of farming (OTEX)
- Agricultural production and financial indicators
- Insurance premiums and indemnities
- Climate-related losses
- Survey weights

## Data processing

The data preparation workflow includes:

1. Importing annual microdata files.
2. Selecting relevant variables.
3. Harmonising variables and geographical classifications across years.
4. Checking the consistency and stability of agricultural holding identifiers.
5. Constructing a longitudinal panel.
6. Creating indicators for descriptive and econometric analysis.
7. Accounting for survey weights and panel structure.

## Data access

Raw microdata and intermediate datasets are not included in this repository.

The original annual files should be obtained from Agreste and placed in this directory, preserving their original filenames.

Additional data requirements and processing steps are described in the R scripts and the master's thesis.

## Related resources

- [R scripts and technical documentation](../scripts/)
- [Statistical visualisations](../figures/)
- [Statistical results](../results/)
- [Full master's thesis](../report/master_thesis.pdf)

## Reproducibility

The scripts have been adapted for repository use, but full end-to-end reproducibility has not yet been verified.
