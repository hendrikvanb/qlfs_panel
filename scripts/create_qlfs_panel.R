# METADATA ====
# Description: Create QLFS panel data 
# Created: 2018-01-21 (Hendrik van Broekhuizen)
# Last edit: 2018-01-26 (Hendrik van Broekhuizen)
# Last review: NA

# SUMMARY: This script creates `data/temp/qlfs_panel.RDS` by joining
# `data/temp/qlfs_pooled.RDS`, `data/temp/qlfs_panel.RDS`, and
# `data/processed/cpi.RDS`. Some processing and feature engineering is also
# performed.


# INITIALISE ====

#> Load libraries ----
library(tidyverse)
library(zoo)
library(lubridate)

#> Set global options ----

# display 13 digits for doubles
options(digits = 13)


# FUNCTIONS ====

# function to impute mean and median earnings for persons who only provided
# bracket income responses.  imputation is based on the mean/median earnings for
# all indivduals reporting actual earnings within the given earnings bracket
# range in the survey wave in question. imputation is done for each unique
# combination of survey_date, q17education, and age_grp1. Note that this
# function is dependent on the structure of the df and the earnings_bracket
# syntax as well as the existence of an earnings variable in the df. function
# returns a df with imputed values for each bracket-survey combination
my_impute_earnings <- function(earnings_bracket) {
  lower_limit <- earnings_bracket %>% 
    gsub(' ', '', .) %>% 
    gsub('^R([0-9]{1,})(-R)([0-9]{1,})$', '\\1', .) %>% 
    as.numeric()
  
  upper_limit <- earnings_bracket %>% 
    gsub(' ', '', .) %>% 
    gsub('^R([0-9]{1,})(-R)([0-9]{1,})$', '\\3', .) %>% 
    as.numeric()
  
  result <- df %>% 
    filter(between(earnings, lower_limit, upper_limit)) %>% 
    group_by(survey_date, q17education, age_grp1) %>% 
    summarise(imputed_median_earnings = median(earnings, na.rm = T),
              imputed_mean_earnings = mean(earnings, na.rm = T)) %>% 
    add_column(q58salarycategory = earnings_bracket) %>% 
    ungroup()
  
  return(result)
}


# LOAD DATA & JOIN ====

# load qlfs_pooled into df
df <- readRDS('data/temp/qlfs_pooled.RDS')

# join in selected variables from lmd_pooled by reading directly from rds file
df <- df %>% 
  left_join(readRDS('data/temp/lmd_pooled.RDS') %>% 
              select(survey_date, uqno, personno, q14age, q52salaryinterval, 
                     q54a_monthly, q56salaryinterval, q57a_monthly, q58salarycategory),
            by = c('survey_date', 'uqno', 'personno', 'q14age'))

# join in selected variables from cpi by reading directly from rds file
x <- df %>% 
  left_join(readRDS('data/processed/cpi.RDS'), 
            by = c('survey_date'))


# FEATURE ENGINEERING ====

#> Create derived household composition features ----

# create derived household size derived variables
df <- df %>% 
  group_by(uqno, survey_date) %>% 
  mutate(hhsize = n(),
         children_15 = sum(q14age < 15),
         children_18 = sum(q14age < 18)) %>% 
  ungroup()

#> Create derived earnings variables ----

# create monthly earnings column equal to q54a_monthly + q57a_monthly
df <- df %>% 
  mutate(earnings = select(., q54a_monthly, q57a_monthly) %>% 
           rowSums(na.rm = T),
         earnings =  ifelse(earnings == 0, NA, earnings)) 

# created a df of imputed earnings using the my_impute_earnings function
imputed_values <- map_dfr(df %>% 
                            pull(q58salarycategory) %>% 
                            levels() %>% 
                            plyr::mapvalues(c('NONE', 'R83 301 OR MORE'), 
                                            c('R0 - R0', 'R83 301 - R500 000')), 
                          my_impute_earnings) %>% 
  mutate(q58salarycategory = q58salarycategory %>% 
           plyr::mapvalues(c('R0 - R0', 'R83 301 - R500 000'),
                           c('NONE', 'R83 301 OR MORE')) %>% 
           factor())

# join imputed earnings values into df %>% coerce q58salarycategory back to
# factor if needed
df <- df %>% 
  left_join(imputed_values, by = c('survey_date', 'q58salarycategory', 'q17education', 'age_grp1')) %>% 
  mutate(q58salarycategory = q58salarycategory %>% as.factor())

# create household earnings and imputed household earnings columns
df <- df %>% 
  group_by(survey_date, uqno) %>% 
  mutate(hh_earnings = sum(earnings, na.rm = T),
         hh_imputed_mean_earnings = sum(imputed_mean_earnings, na.rm = T),
         hh_imputed_median_earnings = sum(imputed_median_earnings, na.rm = T)) %>% 
  ungroup()

# set all missing earnings variables to zero (on the assumption that NA implies
# 0)
df <- df %>% 
  mutate_at(vars(contains('earnings')), function(x) x = ifelse(x == 0, NA, x))

#> Create derived labour market status features ----

# create variable indicating date when respondent started current job
df <- df %>% 
  arrange(uqno, personno, survey_date) %>% 
  mutate(lag_year = lag(q44yearstart),
         lead_year = lead(q44yearstart),
         condition1 = !is.na(q44monthstart) & is.na(q44yearstart) & lag_year == lead_year,
         condition2 = !is.na(q44monthstart) & is.na(q44yearstart) & lag_year != lead_year,
         condition3 = !is.na(q44monthstart) & !is.na(q44yearstart),
         job_start_date = case_when(
           condition1 ~ paste(lag_year, q44monthstart),
           condition2 ~ paste(year(survey_date), q44monthstart),
           condition3 ~ paste(q44yearstart, q44monthstart)
         ) %>% 
           as.yearmon(format = '%Y %m') %>% 
           as.Date()
  ) %>% 
  select(-contains('condition'), -lead_year, -lag_year)

# add a tenure (in months) column to df %>% set tenure to NA if <0
df <- df %>% 
  mutate(tenure = interval(job_start_date, survey_date) %/% months(1),
         tenure =  ifelse(tenure < 0, NA, tenure))


# EXPORT DATA ====

# arrange df %>% export to disk in .RDS format
df %>% 
  arrange(uqno, personno, survey_date) %>% 
  saveRDS('data/temp/qlfs_panel.RDS')
