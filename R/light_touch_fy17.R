##   MER "Light touch" mechanisms
##   A.Chafetz
##   Purpose: find out the #/% of USAID-funded PEPFAR partners that report on 1 or less PEPFAR indicators
##   Date: 2018.04.11
##   Updated: 2018.04.12


# Dependencies ------------------------------------------------------------

  library(tidyverse)

# Import ------------------------------------------------------------------
  
  #data folder
    folder <- "~/ICPI/Data/"
    
  #import OU_IM MER Structured dataset
    df_mer <- read_rds(Sys.glob("ICPI_MER_Structured_Dataset_OU_IM_2*.Rds"))
  
  #import EA file
    df_ea <- read_rds("ICPI_EA_Structured_Dataset_PSNU_IM_20180110.Rds")


# Inspect and Define Indicators -------------------------------------------

  #look at full ind list to determine which are "original" vs "derivative"
  #df_mer %>%  
  #  filter(fy2017apr !=0, !is.na(fy2017apr)) %>%         #keep only mech reporting >=1 value on any indicator
  #  distinct(indicator) %>%                              #distinct list of mechanisms reporting on any indicator
  #  arrange(indicator) %>%                               #sort alphabetically
  #  write_csv("~/tmp/distinct_ind.csv")                  #export for inspections/documentation
      

  ind_lst <- c("EMR_SITE", "FPINT_SITE", "GEND_GBV", "HRH_CURR", "HRH_PRE", "HRH_STAFF", "HTS_TST", 
               "KP_MAT", "KP_PREV", "LAB_PTCQI", "OVC_HIVSTAT", "OVC_SERV", "PMTCT_ART", "PMTCT_EID", 
               "PMTCT_EID_POS", "PMTCT_FO", "PMTCT_STAT", "PP_PREV", "PrEP_NEW", "SC_STOCK", "TB_ART", 
               "TB_PREV", "TB_STAT", "TX_CURR", "TX_NEW", "TX_PVLS", "TX_RET", "TX_TB", "VMMC_CIRC") 
  


# Total Mechanism Count from EA -------------------------------------------

  #denominator
    (df_denom <- df_ea %>% 
      filter(mechanismid != "0", mech_agency != "PEPFAR",                             #filter out dedups
             type == "Expenditure", fy17 !=0, !is.na(fy17)) %>%                       #keep only mech reporting expenditure >$0 on any indicator
      distinct(mech_agency, mechanismid) %>%                                          #distinct list of mechanisms reporting expenditure
      count(mech_agency, sort = TRUE)  %>%                                            #frequency table
      rename(mech_count_ea = n,                                                       #rename for clarity/merge
             fundingagency = mech_agency) %>%                                         #clean agency names to match MSD for merge
      mutate(fundingagency = case_when(fundingagency == "DoD"         ~ "DOD",        
                                       fundingagency == "CDC"         ~ "HHS/CDC",
                                       fundingagency == "Peace Corps" ~ "PC",
                                       fundingagency == "HRSA"        ~ "HHS/HRSA",
                                       fundingagency == "SAMHSA"      ~ "HHS/SAMHSA",
                                       fundingagency == "State_AF"    ~ "State/AF",
                                       fundingagency == "State_PRM"   ~ "State/PRM",
                                       TRUE                           ~ fundingagency
                                       )))
      

# Munge MER ---------------------------------------------------------------


  #comparison table (to denom)
    df_mer %>%  
      filter(fundingagency != "Dedup",                      #filter out dedups
             fy2017apr !=0, !is.na(fy2017apr),              #keep only mech reporting >=1 value on any indicator
             indicator %in% ind_lst) %>%                    #keep just "original", not "derived indicators
      distinct(fundingagency, mechanismid) %>%              #distinct list of mechanisms reporting on any indicator
      count(fundingagency, sort = TRUE)
      
  #group into freq table by # of mechs reported on  
    (df_freq <- df_mer %>%  
      filter(fundingagency != "Dedup",                      #filter out dedups
             fy2017apr !=0, !is.na(fy2017apr),              #keep only mech reporting >=1 value on any indicator
             indicator %in% ind_lst) %>%                    #keep just "original", not "derived indicators
      distinct(fundingagency, mechanismid, indicator) %>%   #distinct list of mechanisms reporting on any indicator
      count(fundingagency, mechanismid) %>%                 #freq table
      rename(ind_reported = n) %>%                          #rename for freq grouping before creating a new count
      mutate(ind_reported = case_when(                      #simply output - does mech report on one ind or more?
                        ind_reported == 1 ~ "mech_one_ind_mer",
                        TRUE              ~ "mech_multi_ind_mer"
                                      )) %>%                
      count(fundingagency, ind_reported) %>%                #2nd freq table with simplified output
      spread(ind_reported, n, fill = 0L))                   #reshape for merging with denom
    
  rm(ind_lst, df_mer, df_ea)
  
# Build Table -------------------------------------------------------------

  #filter to mechanisms reporting on multiple indicators 
    (df_full <- full_join(df_freq, df_denom,  by = "fundingagency") %>%                 #merge mech counts (MER) and total mech count (EA)
        mutate(mech_ind_non_multi = mech_count_ea - mech_multi_ind_mer,                 #gen "non-multi" for ind reporting on 0 and 1 inds
               mech_ind_non_multi_share = round(mech_ind_non_multi/mech_count_ea, 3)))  #gen share this represents of mechs spending PEPFAR $
    
  rm(df_denom, df_freq)
      
    