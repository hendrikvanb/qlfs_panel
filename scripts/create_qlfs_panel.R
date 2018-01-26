# METADATA ====
# Description: Create pooled QLFS data 
# Created: 2018-01-21 (Hendrik van Broekhuizen)
# Last edit: 2018-01-24 (Hendrik van Broekhuizen)
# Last review: NA

# NOTES: This script creates `data/temp/qlfs_panel.RDS` by joining
# `data/temp/qlfs_pooled.RDS` and `data/temp/qlfs_panel.RDS`


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

#> Create derived labour market status features

# create job start_date column %>% create tenure column
x <- df %>% 
  select(uqno, personno, survey_date, q44yearstart, q44monthstart) %>% 
  arrange(uqno, personno, survey_date) %>% 
  group_by(uqno, personno, survey_date) %>% 
  mutate(start_date = case_when(
    (!is.na(q44monthstart) & is.na(q44yearstart) & lag(q44yearstart) == lead(q44yearstart)) ~ paste(lag(q44yearstart), q44monthstart),
    (!is.na(q44monthstart) & is.na(q44yearstart) & lag(q44yearstart) != lead(q44yearstart)) ~ paste(year(survey_date), q44monthstart),
    TRUE ~ paste(q44yearstart, q44monthstart)
  )) %>% 
  ungroup() %>% 
  mutate(start_date = start_date %>% 
           as.yearmon(format = '%Y %m') %>% 
           as.Date()
  )
         
         
         if_else(is.na(q44yearstart), 
                        paste(year(survey_date), q44monthstart),
                        paste(q44yearstart, q44monthstart)) %>% 
           as.yearmon(format = '%Y %m') %>% 
           as.Date())

# WRANGLE DATA ====

#> Select rows ----

# keep only recrods where respondents were 15 or older
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



df %>% filter(respondent_waves > 4) %>% select(survey_date, uqno, personno, respondent_waves, respondent_wave, q14age, q15population) %>% View()


df %>% filter(waves > 5) %>% View()

df %>% group_by(waves) %>% tally()

df %>% slice(1:100000) %>% View()

x %>% summarise_all(class) %>% gather() %>% View()

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

