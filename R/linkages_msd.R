##   Project:  
##   Autors:   J.Davis - USAID
##   Purpose:  
##   Date:     
##   Updated:  


# Dependencies ------------------------------------------------------------

library(tidyverse)
library("devtools")
library(writexl)
#install_github("ICPI/ICPIutilities")
library(ICPIutilities)


# read in linkages IMs ----------------------------------------------------

linkages <- read_csv("C:/Users/GHFP/Documents/GitHub/misc_scripts/linkages_ims.csv", col_names = FALSE, col_types = "c") %>% 
  pull()

# read in site by IM MSD and subset to linkages IMs -----------------------
data <- file.path("C:/Users/GHFP/Documents/data/3.23 refresh")

linkages_msd <- read_rds(file.path(data, "ICPI_MER_Structured_Dataset_PSNU_IM_20180323_v2_1.Rds")) %>% 
  filter(mechanismid %in% linkages) %>%
 
     write_xlsx(linkages_msd, path = file.path(data, "linkages_msd_q1.xlsx"))

