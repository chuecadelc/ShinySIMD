# Scottish Index of Multiple Deprivation (SIMD) Shiny App - Exploring SIMD

An interactive RShiny application for exploring the Scottish Index of Multiple Deprivation (SIMD), originally developed in 2017 during a Q-Step internship at the University of Glasgow under the supervision of Dr. Brian Fogarty and Dr. Niccole Pamphilis. It has been substantially updated in 2026 with a new dataset, redesigned interface and expanded functionality.

---

## Background

This was one of my first RShiny projects, originally built in 2017 as part of the Q-Step Programme at the School of Social and Political Sciences of University of Glasgow, a national initiative promoting quantitative methods training in the social sciences. The app was used by Q-step colleagues in public lectures to help general audiences understand statistical concepts and data visualisation through a real, policy-relevant dataset. 
It was also to guide the public through exploring deprivation patterns across Scotland interactively and to inspire younger generations in Glasgow City to study quantitative methods in the social sciences.

The Scottish Index of Multiple Deprivation (SIMD) is the Scottish Government's official tool for identifying areas of concentrated deprivation across the country, combining multiple domains including income, employment, 
health, education, housing, crime, and geographic access. Full details on the dataset are available from the [Scottish Government](https://www2.gov.scot/Topics/Statistics/SIMD).

---

## What's New in the 2026 Update

- **SIMD 2020 dataset added** alongside the original 2016 release, enabling direct comparison between the two most recent SIMD releases
- **Scotland-wide interactive mapping** — previously limited to Glasgow City Council area, the map now covers the whole of Scotland using Local Authority Districts (2016 and 2020 boundaries respectively)
- **Redesigned interface** using the Bootswatch "Minty" theme for a more polished, modern user experience
- **Improved variable descriptions** for clearer, more accessible interpretation of SIMD domains and indicators
- **Expanded visualisation options** for both single-variable and two-variable comparisons

---

## Features

### 📋 Information
Overview of the SIMD dataset, its domains and how to use the app's features.

### 📊 Single Variable Exploration
Summary statistics and customisable visualisation of individual SIMD variables, including:
- Histogram
- Density plot
- Boxplot

### 🔀 Two Variable Comparison
Explore relationships between pairs of SIMD variables through:
- Scatterplot
- Hexbin chart
- Optional (simple) linear regression model
- Geographic subsetting by local authority area (e.g. Greater Glasgow, Stirling, Edinburgh, Aberdeen, etc.)

### 🗺️ Interactive Map
- Scotland-wide choropleth map using Local Authority District boundaries (2016 / 2020)
- Dedicated high-resolution Glasgow City Council map, built from a custom GeoJSON file created in QGIS
- User-adjustable variable selection, colour schemes, and cartographic styling

> **Note on map density:** Some rural areas with low population density may appear visually sparse on the Scotland-wide map at default zoom. Zoom in for more clearly defined data zones.

---

## Data Sources

| Dataset | Description | Source |
|---------|-------------|--------|
| `SIMD16_Data.xlsx` | SIMD 2016 release, indicator-level data | Scottish Government |
| `SIMD20_Data.xlsx` | SIMD 2020 release, indicator-level data | Scottish Government |
| `SG_SIMD_2016_1.geojson` | Custom Glasgow City Council boundary file | Created in QGIS |
| `data_simdGla.csv` | Processed Glasgow-level dataset | Derived from SIMD source data |

Full SIMD documentation and methodology available at 
[gov.scot/Topics/Statistics/SIMD](https://www2.gov.scot/Topics/Statistics/SIMD).

---

## Repository Structure

```
SIMD-Shiny-App/
│
├── .github/
|    workflows/
│      └──r-check.yml       # Checks syntax, lint (coding conventions) & UI/Server matching
├── www/
│   ├── Q_Step_logo.png     # Q-Step Programme logo
│   └── UofG_logo.jpeg      # University of Glasgow logo
|
├── App.R                   # Main Shiny application (UI and server)
├── ShinySIMD.Rproj         # RStudio project file
│
├── SIMD16_Data.xlsx        # SIMD 2016 dataset
├── SIMD20_Data.xlsx        # SIMD 2020 dataset
├── SG_SIMD_2016_1.geojson  # Custom Glasgow boundary file (QGIS-derived)
├── data_simdGla.csv        # Processed Glasgow-level data
│
├── UI_Server-check.R       # Rscript to check all UI inputs have corresponding Server outputs
│
├── .gitignore
├── README.md
└── LICENSE
```

---

## Tools & Dependencies

```r
install.packages(c(
  "shiny", "bslib", "shinyFeedback",
  "tidyverse", "DT", "ineq", "readxl", 
  "sf", "leaflet", "RColorBrewer", "ggpubr"
))
```

| Package | Purpose |
|---------|---------|
| `shiny` | Core application framework |
| `bslib` | UI theming and layout (`page_navbar`, Bootswatch "lux" theme, Google Fonts) |
| `shinyFeedback` | Inline validation warnings (e.g. flagging when a variable isn't available in both SIMD datasets) |
| `tidyverse` | Data manipulation (`dplyr`) and all visualisations (`ggplot2`) |
| `DT` | Interactive summary statistics table |
| `ineq` | Gini coefficient calculation for inequality measures |
| `readxl` | Reading SIMD source data from Excel workbooks |
| `sf` | Spatial data handling for the Scotland boundary GeoJSON |
| `leaflet` | Interactive choropleth map |
| `RColorBrewer` | Colour palettes for the map's colour scheme selector |
| `ggpubr` | Pearson correlation annotation on scatterplots |

> **Note:** The original script also loaded `gridExtra`, `grid`, `data.table`, `scales`, `lattice`, `magrittr`, `stats`, `haven`, `Hmisc`, `rgdal`, `sp`, `raster`, `rsconnect`, `shinythemes`, `fresh`, `png`, and `shinydashboard`. These have been removed following a dependency audit as part of the 2026 rewrite. `rgdal` in particular was retired from CRAN in October 2023 and is replaced by `sf` throughout. `rsconnect` is only needed for deployment and should be installed separately in your deployment environment rather than loaded within `App.R` itself.

---

## Maintenance Notes

As part of the 2026 update, the codebase underwent a full audit and refactor:

- **Dependency audit** — reduced from 21 loaded libraries to 11 actively used ones, removing unused and deprecated packages (see Tools & Dependencies below)
- **Bug fixes** — resolved several issues including a non-functional interactive map tab, broken theme-switching logic and fragile variable-selection code
- **Code deduplication** — refactored repeated dataset-selection and variable-update logic into shared helper functions, reducing the codebase by roughly 30% while preserving all functionality
- **Deprecated package migration** — replaced `rgdal` (retired from CRAN, October 2023) with `sf` throughout
- **Github Actions** — created various actions for syntax and lint checking and UI/Server consistency, improving workflow

---

## How to Run Locally

1. Clone the repository
2. Open `ShinySIMD.Rproj` in RStudio
3. Ensure all data files (`SIMD16_Data.xlsx`, `SIMD20_Data.xlsx`, 
   `SG_SIMD_2016_1.geojson`, `data_simdGla.csv`) are in the root directory
4. Run the app:

```r
shiny::runApp()
```

---

## Acknowledgements

Originally developed during a Q-Step Programme internship at the School of Social and Political Sciences of University of Glasgow (2017) under the supervision of Dr. Brian Fogarty and Dr. Niccole Pamphilis. The Q-Step Programme is a national initiative promoting quantitative social science training, supported by the Nuffield Foundation, Economic and Social Research Council (ESRC) and the Higher Education Funding Council for England (HEFCE).

---

## Author

Dr. Cristina Chueca Del Cerro  
[Portfolio](https://chuecadelc.github.io/)

Questions, suggestions or collaboration ideas are always welcome so get in touch!

---
