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
    filepath <- "C:\\Users\\GHFP\\Documents\\data\\9_22_release"
    results <- "C:\\Users\\GHFP\\Documents\\data\\9_22_release\\IP_data"

  # read in psnu by im txt file, and save as .rds
    mer_df <- read_msd("ICPI_MER_Structured_Dataset_PSNU_IM_20180323_v2_1.txt")

  #only if you've already read it in and saved as rds 
     setwd(filepath)
     mer_df <- read_rds(file.path(filepath, "MER_Structured_Dataset_PSNU_IM_FY17-18_20180921_v2_1.rds"))
    
# Inspect partners --------------------------------------------------------

    
  # next create distict list of prime partners to locate which ones to filter for(JSI, etc..)
    prime_partner <- mer_df %>% 
      distinct(primepartner) %>% 
      arrange(primepartner)
  
  # list of all partner names of interest
    jsi_lst <- c("John Snow Inc (JSI)", "John Snow, Inc.")
    fhi_lst <- c("FHI 360")
    psi_lst <- c("Population Services International")

    
# Subset to just JSI ------------------------------------------------------


  # filter only primerpartners of interest
    jsi <- filter(mer_df, primepartner %in% jsi_lst) %>% 
      # write both files to excel
      write_xlsx(file.path(results, "9.22.2018_jsi_PSNU_IM.xlsx"), col_names = TRUE)
    
    
  # create list of IMs to JSI
    jsi_mechs <- jsi %>%
      distinct(implementingmechanismname, mechanismid) %>%
      arrange(implementingmechanismname) %>% 
      #write to excel
      write_xlsx(file.path(results, "9.22.2018_jsi_IMs.xlsx"), col_names = TRUE)
    

# FHI360 ------------------------------------------------------------------

#filter to partner of interest
    fhi <- filter(mer_df, primepartner %in% fhi_lst) %>% 
      #write to excel
      write_xlsx(file.path(results, "9.22.2018_fhi_PSNU_IM.xlsx"), col_names = TRUE)
    
    # create list of IMs to FHI
    fhi_mechs <- fhi %>%
      distinct(implementingmechanismname, mechanismid) %>%
      arrange(implementingmechanismname) %>% 
      #write to excel
      write_xlsx(file.path(results, "9.22.2018_fhi_IMs.xlsx"), col_names = TRUE)
    

# PSI ---------------------------------------------------------------------

#filter to partner of interest
    psi <- filter(mer_df, primepartner %in% psi_lst)%>% 
      #write to excel
      write_xlsx(file.path(results, "9.22.2018_psi_PSNU_IM.xlsx"), col_names = TRUE)
    
    # create list of IMs to PSI
    psi_mechs <- psi %>%
      distinct(implementingmechanismname, mechanismid) %>%
      arrange(implementingmechanismname) %>% 
      #write to excel
      write_xlsx(file.path(results, "9.22.2018_psi_IMs.xlsx", col_names = TRUE))





