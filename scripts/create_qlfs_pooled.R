# METADATA ====
# Description: Create pooled QLFS data 
# Created: 2018-01-21 (Hendrik van Broekhuizen)
# Last edit: 2018-01-24 (Hendrik van Broekhuizen)
# Last review: NA

# SUMMARY: This script creates data/temp/qlfs_pooled.RDS by importing and
# appending various waves of qlfs data (stored in .dta Stata format) contained
# in data/raw.  Minimal value cleaning and variable type conversion processing
# is applied to reduce the size of the resultant .RDS file.


# INITIALISE ====

#> Load libraries ----
library(tidyverse)
library(zoo)
library(lubridate)

#> Set global options ----

# display 13 digits for doubles
options(digits = 13)


# FUNCTIONS ====

# function to read qlfs .dta files directly from their zip file containers %>%
# perform minimal processing (conversion to factors, lowercase variable names,
# etc) %>% then export each qlfs df in .RDS format to data/temp
my_stataqlfs_to_rdsqlfs <- function(zipfile, dtafile) {
  df <- unz(zipfile, dtafile) %>% 
    haven::read_dta() %>% 
    haven::as_factor(only_labelled = T, levels = 'label') %>% 
    rename_all(stringr::str_to_lower) %>% 
    add_column(survey_date = zipfile %>% 
                 gsub('^.*qlfs_([0-9]{4})_q([0-9]{1})\\.zip$', '\\1/\\2', .) %>% 
                 as.yearqtr(format = '%Y/%q') %>% 
                 as.Date() + months(2)) %>% 
    select(survey_date, everything()) %>% 
    mutate_if(is.factor, function(x) replace(x, x %in% c('Not applicable', 'Not  applicable'), NA))
  
  newfile <- zipfile %>% 
    gsub('^.*qlfs_([0-9]{4})_q([0-9]{1})\\.zip$', 'qlfs_\\1_q\\2', .) %>% 
    paste0('data/temp/', ., '.RDS')
  df %>% saveRDS(newfile)
}

# function to convert specific qlfs columns to numeric after applying minimal
# processing
my_makenumeric <- function(column) {
  misslist <- c('Unspecified', 'Do not know', 'Do not  know', 'Not applicable', 'Not  applicable')
  if (is.integer(column)) {
    column = column
  } else {
    column = column %>% 
      replace(., . %in% misslist, NA) %>% 
      type.convert(numerals = 'no.loss', as.is = T)
  }
}

# function to convert specific qlfs columns to factors after applying minimal
# processing
my_makefactor <- function(column) {
  misslist <- c('Unspecified', 'Do not know', 'Do not  know', 'Not applicable', 'Not  applicable')
  if (is.integer(column)) {
    column = column
  } else {
    column = case_when(
      column %in% misslist ~ NA_character_,
      TRUE ~ as.character(column)
    ) %>% 
      as.factor()
  }
}

# LOAD DATA & WRANGLE ====

# get list of zip files to be read into dfs
zipfiles <- list.files(path = 'data/raw', 
                       pattern = 'qlfs_[0-9]{4}_q[0-9]{1}\\.zip',
                       full.names = T)

# get list of .dta files in each .zip file to unpack
dtafiles <- map(zipfiles, function(x) 
                  unzip(x, list = T)$Name %>% 
                  grep('\\.dta', ., ignore.case = T, value = T))

# apply the my_stataqlfs_to_rdsqlfs to all zipfiles-dtafiles pairs
map2(zipfiles, dtafiles, my_stataqlfs_to_rdsqlfs)


# APPEND FILES ====

# get list of qlfs .rds files
rdsfiles <- list.files(path = 'data/temp', 
                       pattern = 'qlfs_[0-9]{4}_q[0-9]{1}\\.RDS',
                       full.names = T)

# read each df into memory %>% convert numeric variables to factors (needed to
# avoid type conflicts when row binding in the next step) %>% then row bind
# together into a single df
df <- map_dfr(rdsfiles, function(x) 
  x %>% 
    readRDS() %>% 
    mutate_if(is.numeric, as.factor)
)

backup <- df
df <- backup


# WRANGLE DATA ====

# some question numbers changed between qlfs waves. merge old question number
# columns with newer question number columns where applicable
df <- df %>%
  mutate(q59afarmwrk = factor(q59afarmwrk) %>% if_else(is.na(.), factor(q26afarmwrk), .),
         q59bfetchwater = factor(q59bfetchwater) %>% if_else(is.na(.), factor(q26bfetchwater), .),
         q59cprodhhgoods = factor(q59cprodhhgoods) %>% if_else(is.na(.), factor(q26cprodhhgoods), .),
         q59dconstruc = factor(q59dconstruc) %>% if_else(is.na(.), factor(q26dconstruc), .),
         q59ecatchfood = factor(q59ecatchfood) %>% if_else(is.na(.), factor(q26ecatchfood), .),
         q59atime = as.integer(q59atime) %>% ifelse(is.na(.), as.integer(q26atime), .),
         q59btime = as.integer(q59btime) %>% ifelse(is.na(.), as.integer(q26btime), .),
         q59ctime = as.integer(q59ctime) %>% ifelse(is.na(.), as.integer(q26ctime), .),
         q59dtime = as.integer(q59dtime) %>% ifelse(is.na(.), as.integer(q26dtime), .),
         q59etime = as.integer(q59etime) %>% ifelse(is.na(.), as.integer(q26etime), .)
         )  %>% 
  select(-q26afarmwrk, -q26bfetchwater, -q26cprodhhgoods, -q26dconstruc, -q26ecatchfood,
         -q26atime, -q26btime, -q26ctime, -q26dtime, -q26etime)

# convert specific columns that should be numeric to numeric
df <- df %>% 
  mutate_at(vars(personno, 
                 q14age, 
                 ends_with('hrs'), 
                 ends_with('hrswrk'), 
                 ends_with('time'), 
                 -one_of('q27atime')),
            my_makenumeric)

# consolidate q16maritalstatus levels %>% convert to factor
df <- df %>% 
  mutate(q16maritalstatus = q16maritalstatus %>% 
           replace(., . == 'Divorce or separated', 'Divorced or separated') %>% 
           as.factor())

# consolidate q17education levels. The principle employed here
# is to update older levels to newer levels whenever it is possible to do so
# without loss of information %>% then convert to factor
df <- df %>% 
  mutate(q17education = case_when(
    q17education %in% c('Do not know', 'Other', 'Unspecified') ~ NA_character_,
    q17education == 'Bachelors Degree and Diploma' ~ 'Bachelors Degree and Post Graduate Diploma',
    q17education == 'Grade 3/Standard 1' ~ 'Grade 3/Standard 1/AET 1 (KHARI RI GUDE ,SANLI)',
    q17education == 'Grade 5/Standard 3' ~ 'Grade 5/Standard 3/AET 2',
    q17education == 'Grade 7/Standard 5' ~ 'Grade 7/Standard 5/AET 3',
    q17education == 'Grade 9/Standard 7/Form 2' ~ 'Grade 9/Standard 7/Form 2/AET 4',
    q17education == 'Higher Degree (Masters, Doctorate)' ~ 'Higher Degree (Masters/Phd)',
    q17education == 'NTC l' ~ 'NTC l/N1/NIC/(v) Level 2',
    q17education == 'NTC II' ~ 'NTC II/N2/NIC/(v) Level 3',
    q17education == 'NTC III' ~ 'NTC III/N3/NIC/(v) Level 4',
    TRUE ~ as.character(q17education)
    ) %>% 
      as.factor()
  )

# there are a large number of elementary categorical variables (few categories)
# currently encoded as strings. use my_makefactor() to do some minimal value
# cleaning and convert these to factors
df <- df %>% 
  mutate_at(vars(q20selfrespond:q43industry, q45wrk4whom:q410incometax,
                 q413vat, q414tax, q416nrworkers, q422morehrs, q424wrkxhrs,
                 q425startxwrk, q47b1pdsick:q412csalincrement,
                 indus:status, long_term_unempl:status_exp, metro_code,
                 inactreason:infempl, q18field, q110edui, q311bwhnstart, 
                 q47cleave:q414abprotect, neet, q33awhnstart), 
            my_makefactor)

# convert q44yearstart and q44monthstart to their numeric representations
df <- df %>% 
  mutate(q44yearstart = q44yearstart %>% 
           type.convert(numerals = 'allow.loss', as.is = T, na.strings = c('', 'Unspecified')), 
         q44monthstart = q44monthstart %>% replace(., . == 'Sepember', 'September') %>% 
           match(month.name))
    
# consolidate q45work4whom levels
df <- df %>% 
  mutate(q45wrk4whom = q45wrk4whom %>% 
           replace(., . == 'Own account worker ( not employing any employees)', 'Own account worker (not employing any employees)') %>% 
           as.factor())

# consolidate q411contrcttype levels
df <- df %>% 
  mutate(q411contracttype = case_when(
    q411contracttype == 'A verbal agreement' ~ 'No',
    q411contracttype == 'A written contract' ~ 'Yes',
    TRUE ~ as.character(q411contracttype)
  ) %>% 
    as.factor())

# consolidate q412contrduration levels
df <- df %>%
  mutate(q412contrduration = q412contrduration %>% 
           replace(., . == 'Unspecified duration', NA) %>% 
           as.factor())
                    
# consolidate q415typebusns levels
df <- df %>% 
  mutate(q415typebusns = case_when(
    q415typebusns %in% c('Do not know', 'Unspecified') ~ NA_character_,
    q415typebusns == 'Government controlled business (e.g. Eskom, Telkom)' ~ 'Government controlled business (e.g. Eskom; Telkom)',
    TRUE ~ as.character(q415typebusns)
  ) %>% 
    as.factor()
  )

# convert stratum and weight columns to numeric
df <- df %>% 
  mutate(stratum = stratum %>% as.numeric(),
         weight = weight %>% as.double())

# drop redundant q12nights variable (it is always 'yes')
df <- df %>% 
  select(-q12nights)


# EXPORT DATA ====

# save .rds version of pooled qlfs
df %>% saveRDS('data/temp/qlfs_pooled.RDS')

