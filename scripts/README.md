Scripts
================
Hendrik van Broekhuizen
29 January 2018

-   [1.`create_qlfs_pooled.R`](#create_qlfs_pooled.r)
-   [2.`create_lmd_pooled.R`](#create_lmd_pooled.r)
-   [3.`create_cpi.R`](#create_cpi.r)
-   [4.`create_qlfs_panel.R`](#create_qlfs_panel.r)
-   [5.`process_qlfs_panel.R`](#process_qlfs_panel.r)

Users can create the processed version of the *QLFS panel data* contained in `data/processed/qlfs_panel.RDS` by executing the `.R` scripts in this folder in the following order[1]:

### 1.`create_qlfs_pooled.R`

-   **Description:** Create pooled QLFS data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-24 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates data/temp/qlfs\_pooled.RDS by importing and appending various waves of qlfs data (stored in .dta Stata format) contained in data/raw. Minimal value cleaning and variable type conversion processing is applied to reduce the size of the resultant .RDS file.

### 2.`create_lmd_pooled.R`

-   **Description:** Create pooled LMD data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-24 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates data/temp/lmd\_pooled.RDS by importing and appending various waves of LMD data (stored in .dta Stata format) contained in data/raw. Minimal value cleaning and variable type conversion processing is applied to reduce the size of the resultant .RDS file.

### 3.`create_cpi.R`

-   **Description:** Extract and create monthly CPI data
-   **Created:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates `data/processed/cpi.RDS`. It uses the *OECD stats* api to extract the quarterly CPI for South Africa with the express intention of merging this data into the `qlfs_panel.RDS` dataset. The primary reason for using *OECD stats* rather than "official" *StatsSA* or *SARB* publications is that the former not only provides a consistent api for easy data extraction, but also provides monhtly, quarterly, and yearly CPI and CPI inflation values accurate to four decimal places. See [OECD](http://stats.oecd.org/viewhtml.aspx?datasetcode=MEI_PRICES&lang=en) for more details.

### 4.`create_qlfs_panel.R`

-   **Description:** Create QLFS panel data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates `data/temp/qlfs_panel.RDS` by joining `data/temp/qlfs_pooled.RDS`, `data/temp/qlfs_panel.RDS`, and `data/processed/cpi.RDS`. Some processing and feature engineering is also performed.

### 5.`process_qlfs_panel.R`

-   **Description:** Process QLFS panel data
-   **Created:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-29 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates `data/processed/qlfs_panel.RDS` by processing `data/temp/qlfs_panel.RDS`. This processing includes data wrangling, subsetting, and feature engineering tasks and is focussed on creating a panel dataset that can be used to model specific labour market outcomes. Users wishing to use the qlfs\_panel for other purposes may wish to amend the processing in this script.

[1] Note that `data/processed/qlfs_panel.RDS` contains only a subset of the variables from the *QLFS* data. Users wishing to expand/alter this subset will need to edit the `scripts/process_qlfs_panel.R` script.
