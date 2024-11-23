### Objective: Clean Covariates for H_CHARLS ###

### Notes: only the first 3 waves are included ###

rm(list = ls())

### Import necessary libraries ###

library(haven)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(broom)

### Set working directory ###

setwd("/Users/bruce/Desktop/GHRC Yan Lab/Hearing impairment & PF 2023")

### Import health status DTA files ###
dfraw <- read_dta("rawdata/H_CHARLS.dta")

### Columns to keep ###
var_list <- c("ID",
              "inw1","inw2","inw3","inw4", # in wave
              "r1smokev","r2smokev","r3smokev", # ever smoked
              "r1drinkev","r2drinkev","r3drinkev", #ever drank
              "r1mbmicata","r2mbmicata","r3mbmicata", # BMI category
              "r1hibpe","r2hibpe","r3hibpe",
              "r1diabe","r2diabe","r3diabe",
              "r1cancre","r2cancre","r3cancre",
              "r1lunge","r2lunge","r3lunge",
              "r1hearte","r2hearte","r3hearte",
              "r1stroke","r2stroke","r3stroke",
              "r1arthre","r2arthre","r3arthre",
              "r1dyslipe","r2dyslipe","r3dyslipe",
              "r1livere","r2livere","r3livere",
              "r1kidneye","r2kidneye","r3kidneye",
              "r1asthmae","r2asthmae","r3asthmae")
              
dfCov <- dfraw[var_list]

### wave clearning ###

wave11 <- dfCov %>%
  filter(inw1==1) %>%
  mutate(wave="11") %>%
  dplyr::select(c("ID", "wave",
                  "r1smokev", "r1drinkev", "r1mbmicata",
                  "r1hibpe", "r1diabe", "r1cancre", "r1lunge", "r1hearte", "r1stroke",
                  "r1arthre", "r1dyslipe", "r1livere", "r1kidneye", "r1asthmae"))

wave13 <- dfCov %>%
  filter(inw2==1) %>%
  mutate(wave="13") %>%
  dplyr::select(c("ID", "wave",
                  "r2smokev", "r2drinkev", "r2mbmicata",
                  "r2hibpe", "r2diabe", "r2cancre", "r2lunge", "r2hearte", "r2stroke",
                  "r2arthre", "r2dyslipe", "r2livere", "r2kidneye", "r2asthmae"))

wave15 <- dfCov %>%
  filter(inw3==1) %>%
  mutate(wave="15") %>%
  dplyr::select(c("ID", "wave",
                  "r3smokev", "r3drinkev", "r3mbmicata",
                  "r3hibpe", "r3diabe", "r3cancre", "r3lunge", "r3hearte", "r3stroke",
                  "r3arthre", "r3dyslipe", "r3livere", "r3kidneye", "r3asthmae"))

### rename the columns ###

var_names <- c("id","wave",
               "smokev", "drinkev", "mbmi",
               "hibpe", "diabe", "cancer", "lunge", "hearte", "stroke",
               "arthre", "dyslipe", "livere", "kidneye", "asthmae")

colnames(wave11) <- var_names
colnames(wave13) <- var_names
colnames(wave15) <- var_names

### pool together waves ###
pooled_11_15_Cov <- bind_rows(wave11, wave13, wave15)

### pre-processing covariates ###
pooled_11_15_Cov <- pooled_11_15_Cov %>%
  mutate(smoke = case_when(
    is.na(smokev) ~ NA,
    smokev == 0 ~ 0,
    smokev == 1 ~ 1,
    .default = NA
  )) %>%
  mutate(drink = case_when(
    is.na(drinkev) ~ NA,
    drinkev == 0 ~ 0,
    drinkev == 1 ~ 1,
    .default = NA
  ))

pooled_11_15_Cov <- pooled_11_15_Cov %>%
  select(-c(smokev, drinkev))

save(pooled_11_15_Cov, file = "outputs/pooled_11_15_covariates.RData")