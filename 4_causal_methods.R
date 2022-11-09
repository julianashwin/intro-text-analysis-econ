####
# This file illustrates double ML on FOMC minutes
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(tidyverse)


############### Import data ############### 
# GDP growth 
gdp_df <- read.csv("data/GDPC1.csv", stringsAsFactors = FALSE) %>%
  rename(quarter = DATE, gdp = GDPC1) %>%
  mutate(gdp_lag = lag(gdp), 
         growth = 100*(log(gdp) - log(gdp_lag)),
         quarter = as.Date(quarter))
# Minutes
fedminutes_df <- read.csv("data/fedminutes_clean.csv", stringsAsFactors = FALSE) %>%
  mutate(quarter = floor_date(as.Date(date), unit = "quarter")) %>%
  left_join(gdp_df) %>%
  filter(quarter > "2000-01-01")
