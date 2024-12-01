### Objective: Clean Harmonized CHARLS With Hearing Impairment (Dose-Response) ###

### Notes: Only the first 3 waves have physical measures ###

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

### Import the cross-sectional health status DTA files ###

dfraw <- read_dta("rawdata/H_CHARLS.dta")
wave11raw <- read_dta("rawdata/CHARLS_health_2011.dta")
wave13raw <- read_dta("rawdata/CHARLS_health_2013.dta")
wave15raw <- read_dta("rawdata/CHARLS_health_2015.dta")

### Extract relevant columns ###

demoCols <- c("ID",
              "inw1","inw2","inw3","inw4", # whether in wave
              "r1agey","r2agey","r3agey","r4agey", # curated age
              "ragender", #gender
              "raeducl", # education
              "r1mstat","r2mstat","r3mstat", # marital status
              "h1rural","h2rural","h3rural", # residence
              "hh1cperc", "hh2cperc", "hh3cperc") # household consumption per capita
              

dfdemo <- dfraw[demoCols]

### Wave11 Cleaning ###

wave11 <- dfdemo %>%
  filter(inw1==1) %>%
  mutate(wave="11") %>%
  dplyr::select(c("ID", "wave",
                  "r1agey", "ragender", "raeducl", "r1mstat", "h1rural", "hh1cperc"))

### Wave13 Cleaning ###

wave13 <- dfdemo %>%
  filter(inw2==1) %>%
  mutate(wave="13") %>%
  dplyr::select(c("ID", "wave",
                  "r2agey", "ragender", "raeducl", "r2mstat", "h2rural", "hh2cperc"))

### Wave15 Cleaning ###

wave15 <- dfdemo %>%
  filter(inw3==1) %>%
  mutate(wave="15") %>%
  dplyr::select(c("ID", "wave",
                  "r3agey", "ragender", "raeducl", "r3mstat", "h3rural", "hh3cperc"))


# Define the column names
var_names <- c("id", "wave",
               "age", "sex", "education", "marital", "residence", "hhconsump")

# Assign column names to all datasets
colnames(wave11) <- var_names
colnames(wave13) <- var_names
colnames(wave15) <- var_names

### Pull in original CHARLS data ###
process_wave <- function(wave_raw, wave_data) {
  wave_raw <- wave_raw %>%
    select(ID, da005_4_, da038, da039) %>%
    rename(id = ID, srhear = da005_4_, hraid = da038, hr = da039)
  
  wave_data <- left_join(wave_data, wave_raw, by = "id")
  return(wave_data)
}
# Modify wave 2011 ID
wave11raw <- wave11raw %>%
  mutate(householdID=paste(householdID,"0",sep="")) %>%
  mutate(ID=paste(householdID,substr(ID,10,11),sep=""))

# Process each wave
wave11 <- process_wave(wave11raw, wave11)
wave13 <- process_wave(wave13raw, wave13)
wave15 <- process_wave(wave15raw, wave15)

# Pool waves together
pooled_11_15 <- bind_rows(wave11, wave13, wave15)

### Data post-processing ###
pooled_11_15 <- pooled_11_15 %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(visit = row_number() - 1) %>%
  ungroup() %>%
  filter(age >= 60)

# Hearing impairment and hearing aid
pooled_11_15 <- pooled_11_15 %>%
  mutate(hr = case_when(
    is.na(hr) ~ NA,
    hr == 4 | hr == 5 ~ 2, # severe impairment
    hr == 3 ~ 1, # moderate impairment
    hr %in% c(1, 2) ~ 0, # normal
    .default = NA
  )) %>%
  mutate(hraid = case_when(
    is.na(hraid) ~ NA,
    hraid == 1 ~ 1, # ever used hearing aid
    hraid == 2 ~ 0, # never used hearing aid
    .default = NA
  ))

# Demographics
pooled_11_15 <- pooled_11_15 %>%
  mutate(sex = case_when(
    is.na(sex) ~ NA,
    sex == 1 ~ 0, # male
    sex == 2 ~ 1, # female
    .default = NA
  )) %>%
  mutate(marital = case_when(
    is.na(marital) ~ NA,
    marital == 1 | marital == 3 ~ 0, # partnered
    .default = 1 # alone
  )) %>%
  mutate(residence = case_when(
    is.na(residence) ~ NA,
    residence == 0 ~ 0, # urban
    residence == 1 ~ 1, # rural
    .default = NA
  ))

# Household consumption per capita assignment

for (waveID in c("11", "13", "15")) {
  
  quartiles <- quantile(pooled_11_15[pooled_11_15$wave == waveID, "hhconsump"], probs = seq(0, 1, 0.25), na.rm = TRUE)
  
  pooled_11_15_consum <- pooled_11_15 %>%
    filter(wave == waveID) %>%
    mutate(hhconsumpLevel = case_when(
      is.na(hhconsump) ~ NA,
      hhconsump <= quartiles[["25%"]] ~ 1,
      hhconsump > quartiles[["25%"]] & hhconsump <= quartiles[["50%"]] ~ 2,
      hhconsump > quartiles[["50%"]] & hhconsump <= quartiles[["75%"]] ~ 3,
      .default = 4
    )) %>%
    pull(hhconsumpLevel)
  
  pooled_11_15[pooled_11_15$wave == waveID, "hhconsumpLevel"] <- pooled_11_15_consum
  
}

# Drop original consumption variable
pooled_11_15 <- pooled_11_15 %>%
  select(-hhconsump)

save(pooled_11_15, file = "outputs/pooled_11_15_hearing_dose.RData")