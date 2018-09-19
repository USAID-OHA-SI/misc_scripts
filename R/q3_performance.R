# Q3 performance
# A.Chafetz
# Purpose: Pull results/target data to populate visuals
# Date: 2018-09-19

# SETUP -------------------------------------------------------------------

  #dependencies
    library(tidyverse)
    library(fs)
    library(ICPIutilities)
  
  #folderpath to MSD Dataset
    folderpath_input <- "~/ICPI/Data/"
    
  #folderpath csv output
    tmp <- dir_create(file_temp()) #create a temp folder to delete afterward
    folderpath_output <- tmp

# IMPORT & CLEAN ----------------------------------------------------------
  
  #read in OUxIMfile
    df_mer <- read_rds(file.path(folderpath_input, "MER_Structured_Dataset_OU_FY17-18_20180815_v1_1.Rds"))
  
  #identify Q3 indicators
    ind_q3 <- df_mer %>%
      filter(standardizeddisaggregate == "Total Numerator") %>%
      group_by(indicator) %>%
      summarise_at(vars(fy2018q3), ~ sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      filter(fy2018q3!=0,
             !indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID_Two_Twelve_Months",
                               "PMTCT_STAT_KnownatEntry_POSITIVE", "PMTCT_STAT_NewlyIdentified_Negative",
                               "PMTCT_STAT_NewlyIdentified_POSITIVE", "PMTCT_HEI_POS", "HTS_TST_NEG")) %>%
      pull(indicator)

  #filter to q3 indicators and munge
    df_focus <- df_mer %>% 
      filter(indicator %in% ind_q3, standardizeddisaggregate == "Total Numerator") %>% 
      rename_official() %>% 
      combine_netnew("~/ICPI/Data") %>%
      filter(standardizeddisaggregate == "Total Numerator") %>% #need to clear out MCAD brought in with combine_netnew()
      add_cumulative()


# AGGREGATE DFS -----------------------------------------------------------
  
  #going to aggregate at multiple levels and combine to make pivoting easier in tool
    
  #global rackup  
    gbl <- df_focus %>%
      group_by(indicator) %>%
      summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
      filter(is.finite(achievement)) %>%
      mutate(operatingunit = "GLOBAL",
             fundingagency = "PEPFAR") %>% 
      select(operatingunit, fundingagency, everything())
  
  #global by agency
    gbl_agency <- df_focus %>%
      group_by(fundingagency, indicator) %>%
      summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
      filter(is.finite(achievement)) %>%
      mutate(operatingunit = "GLOBAL") %>% 
      select(operatingunit, fundingagency, everything())
  
  #country total
    ctry <- df_focus %>%
      group_by(operatingunit, indicator) %>%
      summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
      filter(is.finite(achievement)) %>%
      mutate(fundingagency = "PEPFAR") %>% 
      select(operatingunit, fundingagency, everything())
  
  #country by agency total
    agency <- df_focus %>%
      group_by(operatingunit, fundingagency, indicator) %>%
      summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(achievement = round(fy2018cum/fy2018_targets, 2)) %>%
      filter(is.finite(achievement)) %>%
      select(operatingunit, fundingagency, everything())

# APPEND & EXPORT ---------------------------------------------------------

  #combine dataframes
    combo <- bind_rows(gbl, gbl_agency, ctry, agency)
    
  #export
    write_csv(combo, file.path(folderpath_output, "q3_merperformance.csv"), na = "NA")

  #print save location
    folderpath_output
  #remove temp folder
    #dir_delete(tmp)