### Objective: Generate RData for Sensitivity Analysis ###

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

### Load data ###
load("outputs/pooled_11_15_hearing_dose.RData")
load("outputs/pooled_11_15_covariates.RData")
load("outputs/pooled_11_15_PF.RData")

pooled_11_15 <- pooled_11_15 %>%
  dplyr::select(-c(srhear, hraid))

### Merge datafarmes ###
pooled_11_15 <- left_join(pooled_11_15, pooled_11_15_Cov, by = c("id", "wave"))
pooled_11_15 <- left_join(pooled_11_15, pooled_11_15_PF, by = c("id", "wave"))

pooled_11_15 <- pooled_11_15 %>%
  filter(age >= 60) # limit to 60 or above

### Substitute the IDs ###

# replace ids with integers
set.seed(123)
random_sequence <- sample(1:length(unique(pooled_11_15$id)), length(unique(pooled_11_15$id)), replace = FALSE)
unique_id <- unique(pooled_11_15$id)

for (i in 1:length(unique_id)) {
  pooled_11_15$id[pooled_11_15$id == unique_id[i]] <- random_sequence[i]
}

pooled_11_15$id <- as.integer(pooled_11_15$id)

# stratify the visit
pooled_11_15 <- pooled_11_15 %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(visit = row_number() - 1) %>%
  ungroup()

save(pooled_11_15, file = "outputs/pooled_11_15_dose_full.RData")