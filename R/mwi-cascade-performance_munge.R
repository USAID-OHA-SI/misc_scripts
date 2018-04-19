##   IM performance across cascade indicators
##   A.Chafetz
##   Purpose: pull together a table with mechanism performance for cascade indicators
##   Date: 2018.04.19
##   Updated:

# Dependencies ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(writexl)
  library(fs)


# Folders -----------------------------------------------------------------
  #mer data & COP matrix (mech/partner names) location
    datafldr <- "~/ICPI/Data/"
  #output folder
    dir_create("Output")

# Import ------------------------------------------------------------------
 
  #import FY18Q1 dataset
    df_mer <- read_rds(Sys.glob(file.path(datafldr, "ICPI_MER_Structured_Dataset_OU_IM_2*.Rds"))) 

    
# Munge -------------------------------------------------------------------

  #subset to MWI and cascade indicators
    df_mwi <- df_mer %>% 
      filter(operatingunit == "Malawi",
             indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"), 
             standardizeddisaggregate == "Total Numerator",
             fundingagency != "Dedup")
    
  #aggregate to mech level
    df_mwi <- df_mwi %>% 
      select(operatingunit, fundingagency, mechanismid, primepartner, implementingmechanismname, 
             indicator, fy2016apr, fy2016_targets, fy2017apr, fy2017_targets, fy2018q1, fy2018_targets) %>% 
      rename_official(datafldr) %>% #replace mech and partner names with offical names
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    
  #reshape
    df_mwi <- df_mwi %>%
      #make long so each period is a line
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>%
      #rename apr/q1 with cumulative for standardization (it will be a variable name in reshape)
      mutate(pd = str_replace(pd, "apr", "_cum"), 
             pd = str_replace(pd, "q1", "_cum")) %>% 
      #seperate out year and cum/apr
      separate(pd, c("pd", "type")) %>% 
      #spread type so you have a line for each ind + year and cols for results and targets
      spread(type, val) %>% 
      #create achievement ind and write it with a %
      mutate(achievement = round(cum / targets, 2),
             achievement = ifelse(!is.finite(achievement), NA, as.character(paste0((achievement * 100), "%")))) %>% 
      arrange(mechanismid, pd, indicator)

# Export ------------------------------------------------------------------
    #remove blank rows
      df_mwi <- df_mwi %>% 
        mutate(rowtot = cum + targets) %>% 
        filter(rowtot !=0) %>% 
        select(-rowtot)
    
    #export
      write_xlsx(df_mwi, "Output/MWI_CascadeTargetAchievement.xlsx")
  
         