# Car Price Analysis Dashboard (R Shiny)

This is an individual re-implementation of the [DSCI 532 Group 1 Car Price Analysis Dashboard](https://github.com/UBC-MDS/DSCI-532_2026_1_car_price_analysis) built in R Shiny. The original group project was implemented in Python Shiny.

## Purpose

The dashboard allows users to explore car pricing patterns across brands, fuel types, body types, and engine specifications. Users can filter vehicles by brand, body type, fuel type, and price range to compare efficiency scores, pricing trends, and performance characteristics across different segments.

## Live Application

**Deployed app:** 

## Features

- **4 input filters**: brand (multi-select), body type (multi-select), fuel type (multi-select), price range (slider)
- **1 reactive calc**: `filtered_df` — applies all sidebar filters to the dataset
- **6 output components**:
  - 3 KPI value boxes (vehicle count, average price, average efficiency)
  - Engine Size vs Efficiency scatter plot (colored by fuel type)
  - Hybrid vs Standard Fuel efficiency bar chart
  - Average Price by Fuel Type bar chart
  - Horsepower vs Price scatter plot (colored by fuel type)
- Reset Filters button

## Installation and Setup

### Prerequisites

- R >= 4.2
- RStudio (recommended)

### Install required packages

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "scales"))
```

### Run the app

1. Clone the repository:

```bash
git clone https://github.com/quandothoang/dsci532-individual-car-price-r.git
cd dsci532-individual-car-price-r
```

2. Make sure the data file is in place:

```
data/global_cars_enhanced.csv
```

3. Run the app in R or RStudio:

```r
shiny::runApp()
```

Or from terminal:

```bash
Rscript -e "shiny::runApp('.')"
```

## Data Source

Dataset: [Global Cars Enhanced](https://www.kaggle.com/datasets/tatheerabbas/car-price-classification-ready-data) — 300 vehicles with 16 attributes including brand, pricing, engine specs, and efficiency metrics.

## Author

Quan Hoang — UBC MDS 2025-26
