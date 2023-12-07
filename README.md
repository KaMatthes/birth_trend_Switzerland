# The ups and downs of birth rate in Switzerland 2020 to 2023 in a historical context

## Paper (preprint)

<br >
Mathilde Le Vu, Katarina L Matthes, Kaspar Staub (2023) preprint: The ups and downs of birth rate in Switzerland 2020 to 2023 in a historical context <i> medRxiv </i> 
https://doi.org/10.1101/2023.12.05.23299432

## Data

The aggregated data is public available via Zenodo:
<br >
<br >
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10277091.svg)](https://doi.org/10.5281/zenodo.10277091)

## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- output
```

### `R` folder 

This folder contains all R scripts.
  
  - `excess_birth_subgroups.R` : INLA code of estimated birth and 95 CrI
  - `birth_predict.R`: INLA code of birth prediction
  - `plot_predict.R` : code to create figure of birth prediction (Figure 3)
  - `plot_excess_1890.R` : code to create figure of estimated birth during the flu 1890
  - `plot_excess_1918.R` : code to create figure of estimated birth during the flu 1918
  - `plot_excess_1957.R` : code to create figure of estimated birth during the flu 1957
  - `plot_excess_1969.R` : code to create figure of estimated birth during the flu 1969
  - `plot_excess_2009.R` : code to create figure of estimated birth during the flu 2009
  - `plot_excess_2020.R` : code to create figure of estimated birth during the Covid-19

  - 
### `data` folder

This folder contains the aggregated birth data and the Swiss population figures for total births and subgroups.

### `output` folder

This folder contains all outputs.

### `master.R` 

This skript contains information of the used R packages, R scripts, plotting parameters

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png

