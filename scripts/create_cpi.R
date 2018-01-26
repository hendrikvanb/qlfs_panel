# METADATA ====
# Description: Extract and create monthly CPI data 
# Created: 2018-01-26 (Hendrik van Broekhuizen)
# Last edit: 2018-01-26 (Hendrik van Broekhuizen)
# Last review: NA

# SUMMARY: This script creates `data/processed/cpi.RDS`. It uses the _OECD
# stats_ api to extract the quarterly CPI for South Africa with the express
# intention of merging this data into the `qlfs_panel.RDS` dataset. The primary
# reason for using _OECD stats_ rather than "official" _StatsSA_ or _SARB_
# publications is that the former not only provides a consistent api for easy
# data extraction, but also provides monhtly, quarterly, and yearly CPI and CPI
# inflation values accurate to four decimal places.  See
# [OECD](http://stats.oecd.org/viewhtml.aspx?datasetcode=MEI_PRICES&lang=en) for
# more details.


# INITIALISE ====

#> Load libraries ----
library(tidyverse)
library(OECD)
library(zoo)
library(lubridate)

#> Set global options ----

# display 13 digits for doubles
options(digits = 13)


# FUNCTIONS ====


# LOAD DATA & WRANGLE ----

# get info on oecd database structure
info <- get_data_structure('MEI_PRICES')

# download quarterly cpi index for south africa from oecd stats using api and R
# package and put into df
df <- get_dataset('MEI_PRICES',
                  filter = 'CPALTT01.ZAF.IXOB.Q',
                  start_time = 2008, end_time = Sys.Date() %>% 
                    year(), pre_formatted = T)

# select columns to keep %>%  rename columns %>% convert survey_date to date
# object consistent with the survey_date column in qlfs_pooled.RDS
df <- df %>% 
  select(obsTime, obsValue) %>%
  rename(survey_date = obsTime, cpi_2010 = obsValue) %>% 
  mutate(survey_date = survey_date %>% 
           as.yearqtr(format = '%Y-Q%q') %>% 
           as.Date() + months(2)) 

# create a cpi_deflator_current column which can be used as multiplier to
# express rand amounts in current (or most recent) quarter's prices
df <- df %>% 
  mutate(cpi_deflator_current = cpi_2010[survey_date == max(survey_date)]/cpi_2010 
  )
  

# EXPORT DATA ====

# export df to .RDS format
df %>% saveRDS('data/processed/cpi.RDS')
