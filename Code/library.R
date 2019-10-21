#### LIBRARIES ####

## Maps
library(leaflet)
library(lwgeom)
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
library(caret)
library(lme4)
library(randomForest)
library(randomForestExplainer)
library(xgboost)
library(xgboostExplainer)

## Simulating
library(mvnfast)
library(Rfast)

#### PARTY COLORS ####
quebec_parties <- c("Liberal", "Conservative", "NDP", "Bloc", "Green", "People's")
quebec_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Bloc" = "#8ECEF9", "Green" = "green4", "People's" = "midnightblue")
quebec_colors_abbr <- c("LPC" = "red", "CPC" = "blue", "NDP" = "darkorange1", "BQ" = "#8ECEF9", "GPC" = "green4", "PPC" = "midnightblue")

national_parties <- c("Liberal", "Conservative", "NDP", "Green", "People's")
national_colors <- c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", "People's" = "midnightblue")
national_colors_abbr <- c("LPC" = "red", "CPC" = "blue", "NDP" = "darkorange1", "GPC" = "green4", "PPC" = "midnightblue")

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

## sum_squares(): calculates sum of squares of a vector
mean_squares <- function(x) {
  ss <- sum(x^2, na.rm = TRUE)
  n <- length(x[!is.na(x)])
  return(ss/n)
}

## cv.randomForest(): performs p-fold cross-validation for random forest model given parameters
cv.randomForest <- function(formula, data, p, ...) {
  # Identify outcome variable
  outcome_var <- as.character(formula)[2]
  
  # Initialize some values
  too_small <- FALSE
  data <- data[sample(1:nrow(data)),]
  n <- nrow(data)
  n_p <- floor(nrow(data)/p)
  if(n_p*p < n) {
    p <- p + 1
    too_small <- TRUE
  }
  sse <- rep(NA, p)
    
  # Actual cross-validation
  for(i in 1:p) {
    # Choose test indices
    if(too_small & p == p) {
      cv_idx <- ((p-1)*n_p+1):n
    }
    else {
      cv_idx <- ((p-1)*n_p+1):(p*n_p)
    }
    train <- data[-cv_idx,]
    test <- data[cv_idx,]
    
    # Fit random forest on train data
    rf <- randomForest(formula = formula, data = train, ...)
    
    # Test predictions and error
    test_preds <- predict(rf, newdata = test)
    test_error <- test_preds - test[,outcome_var]
    sse[i] <- mean_squares(test_error)*length(cv_idx)
  }
  mse <- sum(sse)/n
  rmse <- sqrt(mse)
  
  return(rmse)
}

## province_polls: print graph of polls for a particular province
province_polls <- function(province) {
  if(grepl("q", province, ignore.case = TRUE)) {
    p <- provincial_polls %>%
      rename(prov = province) %>%
      filter(prov == province) %>%
      melt(id.vars = c("pollster", "date", "age", "mode", "prov"), variable.name = "Party", value.name = "Poll") %>%
      mutate(Poll = as.numeric(Poll)) %>%
      ggplot(aes(x = date, y = Poll, col = Party)) +
      geom_point(alpha = 0.4, size = 1) +
      geom_smooth(method = "loess", span = 2/3, size = 1) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      labs(title = "2019 Canadian federal election polling",
           subtitle = province, x = "Date", y = "%") +
      scale_colour_manual(name = "Party", values = quebec_colors, labels = quebec_parties) +
      theme(axis.text.x = element_text(angle = 90))
  } else if(!grepl("q", province, ignore.case = TRUE)) {
    p <- provincial_polls %>%
      rename(prov = province) %>%
      filter(prov == province) %>%
      melt(id.vars = c("pollster", "date", "age", "mode", "prov"), variable.name = "Party", value.name = "Poll") %>%
      mutate(Poll = as.numeric(Poll)) %>%
      filter(Party != "BQ") %>%
      ggplot(aes(x = date, y = Poll, col = Party)) +
      geom_point(alpha = 0.4, size = 1) +
      geom_smooth(method = "loess", span = 2/3, size = 1) +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      labs(title = "2019 Canadian federal election polling",
           subtitle = province, x = "Date", y = "%") +
      scale_colour_manual(name = "Party", values = national_colors, labels = national_parties) +
      theme(axis.text.x = element_text(angle = 90))
  }
  return(p)
}

## Calculate provincial means
region_wtd_mean <- function(region_polls) {
  means <- region_polls %>%
    summarise(LPC = wtd.mean(LPC, weights = weight),
              CPC = wtd.mean(CPC, weights = weight),
              NDP = wtd.mean(NDP, weights = weight),
              GPC = wtd.mean(GPC, weights = weight),
              PPC = wtd.mean(PPC, weights = weight)) %>%
    t() %>%
    t() %>%
    as.vector()
  return(means)
}