# Multiple Imputation with Chained Equations
# Author: Bruce Zhou
# Date: `r Sys.Date()`

# Load required libraries
library(haven)
library(dplyr)
library(tidyverse)
library(mice)
library(ggmice)
library(miceadds)
library(micemd)

# Clear workspace
rm(list = ls())

# Load data
setwd("/Users/bruce/Desktop/GHRC Yan Lab/Hearing impairment & PF 2023")
load('outputs/pooled_11_15_full.RData')

pooled_11_15 <- pooled_11_15 %>%
  dplyr::select(-c(gaitScore, standScore, SPPB))

# Adjust column sequence
var_list <- c("id", "visit", "hibpe", "diabe", "cancer", "lunge",
              "hearte", "stroke", "arthre", "dyslipe", "livere", 
              "kidneye", "asthmae", "marital", "smoke", "drink", 
              "hr", "hhconsumpLevel", "mbmi", "education", "gait",
              "cstand", "balance", "sex", "residence", "wave", "age")

pooled_11_15_MI <- pooled_11_15 %>%
  dplyr::select(all_of(var_list))

pooled_11_15_MI <- pooled_11_15_MI %>%
  mutate(across(all_of(c(3:20)), factor)) %>%
  mutate(across(all_of(c(21:23)), as.numeric))

# MICE setup
ini <- mice(pooled_11_15_MI, maxit = 0)
meth <- ini$meth

# Specify imputation methods
meth[3:17] <- "2l.bin" # Level-1 categorical variables
meth[c("gait", "cstand", "hhconsumpLevel", "mbmi", "balance")] <- "2l.lmer" # Level-1 continuous/nominal variables
meth[c("education")] <- "2lonly.pmm" # Level-2 variable

# Setup predictor matrix
imputeMatrix <- ini$predictorMatrix
imputeMatrix[, "id"] <- -2 # Clustering variable
imputeMatrix[imputeMatrix == 1] <- 2
imputeMatrix[, "wave"] <- 0
imputeMatrix[c("hhconsumpLevel", "mbmi", "balance"), ] <- 1
imputeMatrix[c("hhconsumpLevel", "mbmi", "balance"), "id"] <- 0
diag(imputeMatrix) <- 0

# Imputation
imp <- mice.par(pooled_11_15_MI, m = 30, maxit = 50, method = meth, 
                predictorMatrix = imputeMatrix, seed = 2024)

# Save the imputed dataset
write.mice.imputation(mi.res = imp, name = "pooled_11_15_imputed", mids2spss = FALSE)

# End of script