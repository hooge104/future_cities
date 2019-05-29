# Interactive explorer of future climates of the World's major cities

This `Shiny` app allows users to scroll, pan and click on a map showing the World's major cities. Each city included in the study is listed as a circle marker, colored by the climatic input variable of choice. Upon clicking a circle marker, results and data are shown. 

Additionally, an data explorer tab is included. 

All data and results are open access and are published in Bastin et al. 2019, Plos One, doi: 10.xxx. 

## Using the app online

Hosted on shinyapps.io: https://hooge104.shinyapps.io/css-app/

## Using the app in Rstudio:

```
runGitHub("future_cities", "hooge104")
```

Required packages:

```
library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(htmltools)
library(DT)
library(reshape2)
```
