##   Project:  JSI structured dataset
##   Autors:   J.Davis - USAID
##   Purpose:  create JSI - specific OU by IM structured dataset
##   Date:     2018-04-10
##   Updated:  2018-04-11


# Dependencies ------------------------------------------------------------

  library(tidyverse)
  library("devtools")
  library(writexl)
  #install_github("ICPI/ICPIutilities")
  library(ICPIutilities)


# Import ------------------------------------------------------------------


  # set file path
    filepath <- "~/data/3.23 refresh"

  # read in psnu by im txt file, and save as .rds
    df <- read_msd("ICPI_MER_Structured_Dataset_PSNU_IM_20180323_v2_1.txt")

    
# Inspect partners --------------------------------------------------------

    
  # next create distict list of prime partners to locate which ones to filter for(JSI, etc..)
    prime_partner <- df %>% 
      distinct(primepartner) %>% 
      arrange(primepartner)
  
  # list of all JSI partner names
    jsi_lst <- c("John Snow Inc (JSI)", "John Snow, Inc.")

    
# Subset to just JSI ------------------------------------------------------


  # filter only primerpartners of interest
    jsi <- filter(df, primepartner %in% jsi_lst) %>% 
      # write both files to excel
      write_xlsx(path = "3.23.2018_jsi_PSNU_IM.xlsx", col_names = TRUE)
    
    
  # create list of IMs to JSI
    jsi_mechs <- jsi %>%
      distinct(implementingmechanismname) %>%
      arrange(implementingmechanismname) %>% 
      #write to excel
      write_xlsx(path = "3.23.2018_jsi_IMs.xlsx", col_names = TRUE)
    

    






