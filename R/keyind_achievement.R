##   Indicator achievement tables
##   A.Chafetz
##   Purpose: generate achievement tables for Country Fact Sheets
##   Date: 2018.09.06
##   Updated: 

#dependencies
  library(tidyverse)
  library(ICPIutilities)

#file path to MSD (.rds)
  msd_filepath <- "~/ICPI/Data/MER_Structured_Dataset_OU_FY17-18_20180815_v1_1.Rds"
  
#folder path for saving
  save_folderpath <-"C:/Users/achafetz/Downloads/"
  
#open and filter to select indicators, adding in a cumulative value for current FY
  df_mer_filter <- read_rds(msd_filepath)  %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    add_cumulative() 

#Calculate achievement portion of the table
  ach <- df_mer_filter %>% 
    #filter to just USAID & CDC
    filter(fundingagency %in% c("USAID", "HHS/CDC")) %>%
    #aggregate mech totals to OU level
    group_by(operatingunit, fundingagency, indicator) %>% 
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>% 
    ungroup() %>% 
    #calculate achievement
    mutate(ach = round(fy2018cum/fy2018_targets*100, 0)) %>% 
    #limit to just the columns needed
    select(operatingunit, fundingagency, indicator, ach) %>% 
    #rename agencies for table names
    mutate(fundingagency = case_when(fundingagency == "HHS/CDC" ~ "cdc_achievement_pct",
                                     fundingagency == "USAID"   ~ "usaid_achievement_pct")) %>% 
    #create wide, spreading agency from row to column
    spread(fundingagency, ach) %>% 
    #reorder columns
    select(operatingunit, indicator, usaid_achievement_pct, cdc_achievement_pct)

#create USAID Target share portion of the table
  share <- df_mer_filter %>% 
    #aggregate mech totals to OU level
    group_by(operatingunit, fundingagency, indicator) %>% 
    summarise_at(vars(ends_with("targets")), ~ sum(., na.rm = TRUE)) %>% 
    ungroup() %>% 
    #calculate USAID share of ou targets by indicator
    group_by(operatingunit, indicator) %>% 
    mutate(usaid_fy18_target_share = round(fy2018_targets/sum(fy2018_targets)*100, 0),
           usaid_fy19_target_share = round(fy2019_targets/sum(fy2019_targets)*100, 0)) %>% 
    ungroup() %>% 
    #keep just USAID
    filter(fundingagency == "USAID") %>%
    #keep only necessary columns
    select(operatingunit, indicator, ends_with("share"))

#merge two pieces together
  table <- full_join(ach, share) %>%
    #reorder indicators to cascade rather than alpabetical
    mutate(indicator = factor(indicator, c("HTS_TST_POS", "TX_NEW", "TX_CURR"))) %>% 
    arrange(operatingunit, indicator)

#export
  table %>% 
    rename_all(~ c("Operating Unit", "Indicator", "USAID Target Achievement (%)", 
                 "CDC Target Achievement (%)", "USAID Share of FY18 OU Targets (%)",
                 "USAID Share of FY19 OU Targets (%)")) %>% 
    write_csv(file.path(save_folderpath, "CountryProfile_MERTables.csv"), na = "")
  