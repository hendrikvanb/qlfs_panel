# METADATA ====
# Description: Process QLFS panel data 
# Created: 2018-01-26 (Hendrik van Broekhuizen)
# Last edit: 2018-02-05 (Hendrik van Broekhuizen)
# Last review: NA

# SUMMARY: This script creates `data/processed/qlfs_panel.RDS` by processing
# `data/temp/qlfs_panel.RDS`. This processing includes data wrangling,
# subsetting, and feature engineering tasks and is focussed on creating a panel
# dataset that can be used to model specific labour market outcomes.  Users
# wishing to use the qlfs_panel for other purposes may wish to amend the
# processing in this script.


# INITIALISE ====

#> Load libraries ----
library(tidyverse)
library(zoo)
library(lubridate)

#> Set global options ----

# display 13 digits for doubles
options(digits = 13)


# FUNCTIONS ====



# LOAD DATA & WRANGLE ====

# read qlfs_panel into df
df <- readRDS('data/temp/qlfs_panel.RDS')

#> Select rows ----

# keep only recrods where respondents are 15 or older
df <- df %>% filter(q14age >= 15)

# sort data frame %>% create column to show how many waves each respondent was
# surveyed %>% create column to count each wave
df <- df %>%
  arrange(uqno, personno, survey_date) %>% 
  group_by(uqno, personno) %>% 
  mutate(respondent_waves = n(), 
         respondent_wave = row_number()) %>% 
  ungroup

# keep only records where respondents were captured in multiple waves
df <- df %>% filter(respondent_waves > 1)

#> Select columns ----

# feature selection
df <- df %>% 
  select(survey_date, uqno, personno, respondent_waves, respondent_wave,
         province, hhsize, children_15, children_18, q13gender:q20selfrespond,  
         education_status, status, status_exp, unempl_status, cpi_2010, cpi_deflator_current,
         indus:hrswrk, q415typebusns, q412bmemunion, job_start_date, tenure, 
         earnings:hh_imputed_median_earnings, q18field, q110edui, 
         q52salaryinterval, q56salaryinterval, q58salarycategory, q411contracttype, q412contrduration)

# rename columns (to be more user friendly)
df <- df %>% 
  rename(gender = q13gender,
         age = q14age,
         race = q15population,
         marital_status = q16maritalstatus,
         education = q17education,
         self_respond = q20selfrespond,
         union = q412bmemunion,
         field_of_study = q18field,
         educ_institution = q110edui,
         sector = q415typebusns,
         hours = hrswrk,
         written_contract = q411contracttype,
         contract_duration = q412contrduration
         )


# FEATURE ENGINEERING ====

# add binary derived employment indicator
df <- df %>% 
  mutate(empl = if_else(status == 'Employed', 1, 0))

# add numeric quarter of year column
df <- df %>% 
  mutate(quarter = quarter(survey_date) %>% unclass())


# EXPORT DATA ====

# save .rds version of df
df %>% saveRDS('data/processed/qlfs_panel.RDS')