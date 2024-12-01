# HI-PF-CHARLS

Repo for R codes with the project: Hearing Impairment and Physical Functioning

Supervisor: **Prof. Lijing Yan**, **Zhengting He, ScM**.

Project leader: **Bruce Zhou, BS**.

# Instructions

## Data Cleaning

**charls_hearing_prep:** This cleans hearing + demographic covariates.
**charls_physical_prep:** This cleans the SPPB and sub-items.
**charls_covariates_prep:** This cleans the potential confounders.

The order of these scripts does not matter as they are self-contained.

## Data Merging

**charls_stata:** This merges the 3 dataframes above to form the full\
dataset. It exports to both R and Stata. Some of the first regressions\
were done using Stata.

## Multiple Imputation

**multiple_imputation:** This implements a paralleled multiple imputation\
with chained equations in R. The random seed has been set to 2024.

## Statistical Analyses

The statistical analyses scripts have been stored in each correspoding fol\
der, namely `main analysis`, `secondary analysis`, `sensitivity analysis`,\
and `subgroup analysis`.
