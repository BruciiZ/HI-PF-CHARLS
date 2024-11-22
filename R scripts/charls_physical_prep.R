### Objective: Clean Harmonized CHARLS With Physical Function Measures and Sensory Impairments ###

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
              "inw1","inw2","inw3","inw4", #whether in wave
              "ragender", #gender
              "rabyear","rabmonth","rabday", #birth year, month, and day
              "r1agey","r2agey","r3agey","r4agey", #curated age
              "h1rural","h2rural","h3rural", #urban or rural
              "r1dressa","r2dressa","r3dressa","r4dressa",
              "r1batha","r2batha","r3batha","r4batha",
              "r1eata","r2eata","r3eata","r4eata",
              "r1beda","r2beda","r3beda","r4beda",
              "r1toilta","r2toilta","r3toilta","r4toilta",
              "r1urina","r2urina","r3urina","r4urina",
              "r1moneya","r2moneya","r3moneya","r4moneya",
              "r1medsa","r2medsa","r3medsa","r4medsa",
              "r1mealsa","r2mealsa","r3mealsa","r4mealsa",
              "r1housewka","r2housewka","r3housewka","r4housewka",
              "r1shopa","r2shopa","r3shopa","r4shopa",
              "r1adlab_c","r2adlab_c","r3adlab_c","r4adlab_c",
              "r1wspeed","r2wspeed","r3wspeed", #gait speed
              "r1balance", "r2balance","r3balance", #balance score
              "r1chr5sec","r2chr5sec","r3chr5sec", #chair stand time
              "r1gripsum","r2gripsum","r3gripsum") #grip strength

dfdemo <- dfraw[demoCols]

### Wave11 Cleaning ###

wave11 <- dfdemo %>%
  filter(inw1==1) %>%
  mutate(wave="11") %>%
  dplyr::select(c("ID","wave","rabyear","rabmonth","rabday","r1agey","ragender","h1rural",
           "r1dressa","r1batha","r1eata","r1beda","r1toilta","r1urina",
           "r1moneya","r1medsa","r1mealsa","r1housewka","r1shopa","r1adlab_c",
           "r1wspeed","r1gripsum","r1balance","r1chr5sec"))

### Wave13 Cleaning ###

wave13 <- dfdemo %>%
  filter(inw2==1) %>%
  mutate(wave="13") %>%
  dplyr::select(c("ID","wave","rabyear","rabmonth","rabday","r2agey","ragender","h2rural",
           "r2dressa","r2batha","r2eata","r2beda","r2toilta","r2urina",
           "r2moneya","r2medsa","r2mealsa","r2housewka","r2shopa","r2adlab_c",
           "r2wspeed","r2gripsum","r2balance","r2chr5sec"))

### Wave15 Cleaning ###

wave15 <- dfdemo %>%
  filter(inw3==1) %>%
  mutate(wave="15") %>%
  dplyr::select(c("ID","wave","rabyear","rabmonth","rabday","r3agey","ragender","h3rural",
           "r3dressa","r3batha","r3eata","r3beda","r3toilta","r3urina",
           "r3moneya","r3medsa","r3mealsa","r3housewka","r3shopa","r3adlab_c",
           "r3wspeed","r3gripsum","r3balance","r3chr5sec"))


# Define the column names
column_names <- c("id", "wave", "byear", "bmonth", "bday", "age", "sex", "residence",
                  "e1", "e2", "e3", "e4", "e5", "e6",
                  "e7", "e8", "e9", "e10", "e11", "ADLScore",
                  "gait", "grip", "balance", "cstand")

# Assign column names to all datasets
for (wave in list(wave11, wave13, wave15)) {
  colnames(wave) <- column_names
}

### Pull in original CHARLS data ###
wave11raw <- wave11raw %>%
  mutate(householdID=paste(householdID,"0",sep="")) %>%
  mutate(ID=paste(householdID,substr(ID,10,11),sep=""))

wave11raw <- wave11raw[c("ID","da005_4_","da032","da033","da034","da038","da039")]
colnames(wave11raw) <- c("id","srhear","wrglass","vdis","vclo","hraid","hr")
wave11 <- left_join(wave11,wave11raw,by="id")

wave13raw <- wave13raw[c("ID","da005_4_","da032","da033","da034","da038","da039")]
colnames(wave13raw) <- c("id","srhear","wrglass","vdis","vclo","hraid","hr")
wave13 <- left_join(wave13,wave13raw,by="id")

wave15raw <- wave15raw[c("ID","da005_4_","da032","da033","da034","da038","da039")]
colnames(wave15raw) <- c("id","srhear","wrglass","vdis","vclo","hraid","hr")
wave15 <- left_join(wave15,wave15raw,by="id")

waveALL <- full_join(wave11,wave13)
waveALL <- full_join(waveALL,wave15)

### Data post-processing ###
df <- waveALL %>%
  arrange(id,wave) %>%
  group_by(id) %>%
  mutate(visit=row_number() - 1) %>%
  ungroup()

# Reorder the columns
df <- df %>%
  dplyr::select(c("id","wave","byear","bmonth","bday","age","sex","residence",
                  "e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e11",
                  "srhear", "wrglass","vdis","vclo","hraid","hr","visit",
                  "gait","balance","cstand","grip","ADLScore"))

# ADL/IADL Coding
df[,9:25] <- replace(df[,9:25], is.na(df[,9:25]), 9)


# If any of the ADL items is 1, ADL is impaired; same with IADL
df <- df %>%
  mutate(ADL=ifelse(if_any(c(e1, e2, e3, e4, e5, e6), ~. == 1), 1,
                    ifelse(if_all(c(e1, e2, e3, e4, e5, e6), ~. == 9), NA, 0))) %>%
  mutate(IADL=ifelse(if_any(c(e7, e8, e9, e10, e11), ~. == 1), 1,
                     ifelse(if_all(c(e7, e8, e9, e10, e11), ~. == 9), NA, 0)))

# Calculate IADL Score and the summary score of ADL and IADL
df[,15:19] <- replace(df[,15:19], df[,15:19]==9, NA)

df <- df %>%
  mutate(IADLScore=ifelse(is.na(IADL), 
                          NA, rowSums(df[,15:19],na.rm = TRUE)))

df <- df %>%
  mutate(SumScore=ifelse(is.na(ADL)&is.na(IADL), 
                         NA, rowSums(df[,c("ADLScore","IADLScore")],na.rm = TRUE)))

# Hearing impairment and hearing aid coding
df <- df %>%
  mutate(hearloss = case_when(
    is.na(hr) ~ NA,
    hr == 4 | hr == 5 ~ 1,
    hr %in% c(1, 2, 3) ~ 0,
    .default = NA
  )) %>%
  mutate(hraid = case_when(
    is.na(hraid) ~ NA,
    hraid == 2 ~ 0,
    hraid == 1 ~ 1,
    .default = NA
  ))

# Drop unwanted columns
df <- df %>%
  dplyr::select(-c("e1","e2","e3","e4","e5",
            "e6","e7","e8","e9","e10","e11",
            "wrglass","vdis","vclo","hr",
            "srhear"))

save(df, file = "outputs/H_CHARLSPhysical.RData")