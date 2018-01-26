Scripts
================
Hendrik van Broekhuizen
26 January 2018

-   [`create_qlfs_pooled.R`](#create_qlfs_pooled.r)
-   [`create_lmd_pooled.R`](#create_lmd_pooled.r)
-   [`create_cpi.R`](#create_cpi.r)
-   [`create_qlfs_panel.R`](#create_qlfs_panel.r)

The `data/processed/qlfs_panel.RDS` dataset can be created/updated by executing the following scripts in the indicated order:

### `create_qlfs_pooled.R`

-   **Description:** Create pooled QLFS data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-24 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   \*\*<Notes:**> This script creates data/temp/qlfs\_pooled.RDS by importing and appending various waves of qlfs data (stored in .dta Stata format) contained in data/raw. Minimal value cleaning and variable type conversion processing is applied to reduce the size of the resultant .RDS file.

### `create_lmd_pooled.R`

-   **Description:** Create pooled LMD data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-24 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   \*\*<Notes:**> This script creates data/temp/lmd\_pooled.RDS by importing and appending various waves of LMD data (stored in .dta Stata format) contained in data/raw. Minimal value cleaning and variable type conversion processing is applied to reduce the size of the resultant .RDS file.

### `create_cpi.R`

-   **Description:** Extract and create monthly CPI data
-   **Created:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-26 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   **Summary:** This script creates `data/processed/cpi.RDS`. It uses the *OECD stats* api to extract the quarterly CPI for South Africa with the express intention of merging this data into the `qlfs_panel.RDS` dataset. The primary reason for using *OECD stats* rather than "official" *StatsSA* or *SARB* publications is that the former not only provides a consistent api for easy data extraction, but also provides monhtly, quarterly, and yearly CPI and CPI inflation values accurate to four decimal places. See [OECD](http://stats.oecd.org/viewhtml.aspx?datasetcode=MEI_PRICES&lang=en) for more details.

### `create_qlfs_panel.R`

-   **Description:** Create pooled QLFS data
-   **Created:** 2018-01-21 (Hendrik van Broekhuizen)
-   **Last Edit:** 2018-01-24 (Hendrik van Broekhuizen)
-   **Last Review:** NA
-   \*\*<Notes:**> This script creates `data/temp/qlfs_panel.RDS` by joining `data/temp/qlfs_pooled.RDS` and `data/temp/qlfs_panel.RDS`
