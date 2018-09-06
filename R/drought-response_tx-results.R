# Drought Response Site Results
# A.Chafetz
# Purpose: Pull results data for sites related to drought support
# Date: 2018-05-24

# Dependencies ------------------------------------------------------------

  library(fs)
  library(tidyverse)
  library(ICPIutilities)
  library(readxl)


# Folder Setup ------------------------------------------------------------

  #create a temporary directory
    dir_create("tmp_prj")
  #create a raw data folder
    dir_create("tmp_prj/RawData")
    ### Store MER Structured Datasets here ###
  #create a folder for support files
    dir_create("tmp_prj/SupportFiles")
    ### Store "list of sites.csv" here ###
  #create output folder
    dir_create("tmp_prj/Output")


# Function ----------------------------------------------------------------
  
    
  extract_sitetx <- function(file){
    
    #identify site to keep from provided list
      site_uids <- read_xlsx("~/tmp_prj/SupportFiles/list of sites.xlsx") %>% 
          filter(!is.na(`DATIM Site UID`)) %>% 
          pull(`DATIM Site UID`)
      
    #unzip MSD file
      unzip(file.path("~/tmp_prj/RawData", paste0(file, ".zip")), 
            exdir = "~/tmp_prj/RawData")
      
    #import site MSD, keeping just TX values
      df <- read_msd(file.path("~/tmp_prj/RawData", file), 
                     save_rds = FALSE, remove_txt = FALSE) %>% 
      filter(indicator %in% c("TX_NEW", "TX_CURR"), 
             orgunituid %in% site_uids)
    
    return(df)
}
    

# Run ---------------------------------------------------------------------

    #identify all MSD files saved in folder
      msd_files <- list.files("~/tmp_prj/RawData") %>% 
        path_ext_remove()
    
    #run above function to import and subset data
      keysites <- map_dfr(.x = msd_files, .f = ~ extract_sitetx(.x))
      
    #export
      write_tsv(keysites, "~/tmp_prj/Output/drought_response_sites_tx.txt", na = "")

# Clean up ----------------------------------------------------------------
  
  #remove direcotry
    unlink("~/tmp_prj")

