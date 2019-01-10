#### LIBRARIES ####

## Maps
library(leaflet)
library(rgdal)
library(rmapshaper)
library(sf)
library(sp)

## Scraping
library(httr)
library(rvest)
library(tidyverse)
library(xml2)

## Data manipulation
library(data.table)
library(Hmisc)
library(lubridate)
library(tidyverse)
library(readr)
library(reshape2)

## Modeling
library(randomForest)
library(randomForestExplainer)
library(xgboost)
library(xgboostExplainer)

#### CUSTOM FUNCTIONS ####
## fread_to_tbl(): uses data.table::fread() for fast reading of data, then converts to tibble
fread_to_tbl <- function(filepath) {
  data <- fread(filepath) %>%
    as.data.frame() %>%
    as.tbl()
  return(data)
}

## logit(): takes the logit of x
logit <- function(x) {
  x <- log(x/(1-x))
  return(x)
}

## invlogit(): inverts the logit function (logit(invlogit(x) = 1)
invlogit <- function(x) {
  x <- exp(x)/(1 + exp(x))
  return(x)
}
