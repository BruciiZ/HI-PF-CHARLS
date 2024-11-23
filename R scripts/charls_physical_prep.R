### Objective: Clean Physical Functioning Measures for H_CHARLS ###

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
              "inw1","inw2","inw3","inw4",
              "r1wspeed1","r2wspeed1","r3wspeed1", # gait speed 1
              "r1wspeed2","r2wspeed2","r3wspeed2", # gait speed 2
              "r1wspeed", "r2wspeed", "r3wspeed", # average time for gait speed
              "r1walkcomp", "r2walkcomp", "r3walkcomp", # willing and able to complete gait
              "r1sbstan", "r2sbstan", "r3sbstan", # side-by-side test result
              "r1sbsdone", "r2sbsdone", "r3sbsdone", # completed side-by-side
              "r1sbscomp", "r2sbscomp", "r3sbscomp", # willing and able to do side by side
              "r1semitan", "r2semitan", "r3semitan", # semi-tandem test result
              "r1semicomp", "r2semicomp", "r3semicomp", # semi-tandem willing and able
              "r1fulltan", "r2fulltan", "r3fulltan", # full-tandem results
              "r1balance", "r2balance", "r3balance", # balance summary score
              "r1chr5sec","r2chr5sec","r3chr5sec", # chair stand time
              "r1chr5comp", "r2chr5comp", "r3chr5comp", # willing or able to do chair stands
              "r1lgrip","r2lgrip","r3lgrip", # max left hand grip
              "r1rgrip", "r2rgrip", "r3rgrip") # max right hand grip

dfPF <- dfraw[var_list]

### wave cleaning ###

wave11 <- dfPF %>%
  filter(inw1==1) %>%
  mutate(wave="11") %>%
  dplyr::select(c(ID, wave,
                  r1wspeed1, r1wspeed2, r1wspeed, r1walkcomp,
                  r1sbstan, r1sbsdone, r1sbscomp, r1semitan, r1semicomp, r1fulltan, r1balance,
                  r1chr5sec, r1chr5comp,
                  r1lgrip, r1rgrip))

wave13 <- dfPF %>%
  filter(inw2==1) %>%
  mutate(wave="13") %>%
  dplyr::select(c(ID, wave,
                  r2wspeed1, r2wspeed2, r2wspeed, r2walkcomp,
                  r2sbstan, r2sbsdone, r2sbscomp, r2semitan, r2semicomp, r2fulltan, r2balance,
                  r2chr5sec, r2chr5comp,
                  r2lgrip, r2rgrip))

wave15 <- dfPF %>%
  filter(inw3==1) %>%
  mutate(wave="15") %>%
  dplyr::select(c(ID, wave,
                  r3wspeed1, r3wspeed2, r3wspeed, r3walkcomp,
                  r3sbstan, r3sbsdone, r3sbscomp, r3semitan, r3semicomp, r3fulltan, r3balance,
                  r3chr5sec, r3chr5comp,
                  r3lgrip, r3rgrip))

### rename the columns ###

var_names <- c("id", "wave",
               "gait1", "gait2", "avg_gait", "gait_comp",
               "sbs", "sbs_done", "sbs_comp", "semitan", "semitan_comp", "fulltan", "balance",
               "cstand", "cstand_comp",
               "lgrip", "rgrip")

colnames(wave11) <- var_names
colnames(wave13) <- var_names
colnames(wave15) <- var_names

### pool together waves ###
pooled_11_15_PF <- bind_rows(wave11, wave13, wave15)

### pre-processing PF covariates ###
pooled_11_15_PF <- pooled_11_15_PF %>%
  mutate(gait = case_when(
    is.na(gait1) & is.na(gait2) ~ NA,
    is.na(gait1) & !is.na(gait2) ~ gait2,
    is.na(gait2) & !is.na(gait1) ~ gait1,
    gait1 >= gait2 ~ gait2,
    .default = gait1
  )) %>%
  mutate(handgrip = case_when(
    is.na(lgrip) & is.na(rgrip) ~ NA,
    is.na(lgrip) ~ rgrip,
    is.na(rgrip) ~ lgrip,
    rgrip >= lgrip ~ rgrip,
    .default = lgrip
  ))

# exclude outliers for gait and chair stand time

#upperGait <- quantile(pooled_11_15_PF$gait, probs = c(0.995), na.rm = TRUE)
lowerGait <- quantile(pooled_11_15_PF$gait, probs = c(0.005), na.rm = TRUE)

#upperChair <- quantile(pooled_11_15_PF$cstand, probs = c(0.995), na.rm = TRUE)
lowerChair <- quantile(pooled_11_15_PF$cstand, probs = c(0.005), na.rm = TRUE)

pooled_11_15_PF <- pooled_11_15_PF %>%
  mutate(gait = case_when(
    is.na(gait) ~ NA,
    gait <= lowerGait ~ NA,
    .default = 2.5 / gait # meters per second
  )) %>%
  mutate(cstand = case_when(
    is.na(cstand) ~ NA,
    cstand <= lowerChair ~ NA,
    .default = cstand
  ))

# calculate the SPPB score

gait_quartiles <- quantile(pooled_11_15_PF$gait, probs = seq(0, 1, 0.25), na.rm = T)
cstand_quartiles <- quantile(pooled_11_15_PF$cstand, probs = seq(0, 1, 0.25), na.rm = T)
pooled_11_15_PF <- pooled_11_15_PF %>%
  mutate(gaitScore = case_when(
    is.na(gait) ~ NA,
    gait <= gait_quartiles[["25%"]] ~ 1,
    gait > gait_quartiles[["25%"]] & gait <= gait_quartiles[["50%"]] ~ 2,
    gait > gait_quartiles[["50%"]] & gait <= gait_quartiles[["75%"]] ~ 3,
    gait > gait_quartiles[["75%"]] ~ 4,
    .default = NA
  )) %>%
  mutate(standScore = case_when(
    is.na(cstand) ~ NA,
    cstand <= cstand_quartiles[["25%"]] ~ 4,
    cstand > cstand_quartiles[["25%"]] & cstand <= cstand_quartiles[["50%"]] ~ 3,
    cstand > cstand_quartiles[["50%"]] & cstand <= cstand_quartiles[["75%"]] ~ 2,
    cstand > cstand_quartiles[["75%"]] ~ 1,
    .default = NA
  ))

pooled_11_15_PF <- pooled_11_15_PF %>%
  mutate(SPPB = case_when(
    rowSums(across(c(gaitScore, standScore, balance), ~is.na(.))) == 3 ~ NA,
    .default = rowSums(across(c(gaitScore, standScore, balance)), na.rm = TRUE)
  ))

pooled_11_15_PF <- pooled_11_15_PF %>%
  dplyr::select(c(id, wave, gait, cstand, balance, gaitScore, standScore, SPPB))

save(pooled_11_15_PF, file = "outputs/pooled_11_15_PF.RData")