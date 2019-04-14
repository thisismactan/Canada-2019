## Estimate historical error variance in polls
source("Code/library.R")

## Read in polls and compute errors
polls_2006 <- read_csv("Data/polls_2006.csv") %>%
  mutate(age = as.numeric(as.Date("2006-01-23") - date),
         weight = (age <= 30)*exp(-(age^(1/3)))/(2019-2006),
         CPC_error = CPC - 36.27,
         LPC_error = LPC - 30.23,
         NDP_error = NDP - 17.48,
         Bloc_error = BQ - 10.48,
         Green_error = Green - 4.48) %>%
  filter(weight > 0)

polls_2008 <- read_csv("Data/polls_2008.csv") %>%
  mutate(age = as.numeric(as.Date("2008-10-14") - date),
         weight = (age <= 30)*exp(-(age^(1/3)))/(2019-2008),
         CPC_error = CPC - 37.65,
         LPC_error = LPC - 26.26,
         NDP_error = NDP - 18.18,
         Bloc_error = BQ - 9.98,
         Green_error = Green - 6.78) %>%
  filter(weight > 0)

polls_2011 <- read_csv("Data/polls_2011.csv") %>%
  mutate(age = as.numeric(as.Date("2011-05-02") - date),
         weight = (age <= 30)*exp(-(age^(1/3)))/(2019-2011),
         CPC_error = CPC - 39.62,
         LPC_error = LPC - 18.91,
         NDP_error = NDP - 30.63,
         Bloc_error = BQ - 6.04,
         Green_error = Green - 3.91) %>%
  filter(weight > 0)

polls_2015 <- read_csv("Data/polls_2015.csv") %>%
  mutate(age = as.numeric(as.Date("2015-10-19") - date),
         weight = (age <= 30)*exp(-(age^(1/3)))/(2019-2015),
         CPC_error = CPC - 31.89,
         LPC_error = LPC - 39.47,
         NDP_error = NDP - 19.71,
         Bloc_error = BQ - 4.66,
         Green_error = GPC - 3.45) %>%
  filter(weight > 0)

## Compute weighted covariance matrix
poll_errors <- bind_rows(polls_2006, polls_2008, polls_2011, polls_2015) %>%
  dplyr::select(weight, LPC_error, CPC_error, NDP_error, Bloc_error, Green_error) %>%
  na.omit()

poll_error_covariance <- cov.wt(poll_errors %>% dplyr::select(-weight), wt = poll_errors$weight)$cov 
