##   MER "Light touch" mechanisms
##   A.Chafetz
##   Purpose: find out the #/% of USAID-funded PEPFAR partners that report on 1 or less PEPFAR indicators
##   Date: 2018.04.11
##   Updated: 2018.04.14


# Dependencies ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)

# Directories -------------------------------------------------------------
  
  #data folder - folder with MER and EA data
    datafolder <- "~/ICPI/Data/"

  #export folder
    exportfolder <- "~/tmp/"

# Import ------------------------------------------------------------------


  #import OU_IM MER Structured dataset
    df_mer <- read_rds(Sys.glob(file.path(datafolder, "ICPI_MER_Structured_Dataset_OU_IM_2*.Rds")))
  
  #import EA file
    df_ea <- read_rds(file.path(datafolder, "ICPI_EA_Structured_Dataset_PSNU_IM_20180110.Rds"))


# Total Mechanism Count from EA -------------------------------------------

  #denominator
    df_denom <- df_ea %>% 
      #subset dataset to just each mechanism's total
      filter(mechanismid != "0",                                           #filter out dedups
             data_type == "DIRECT",                                        #mech level = dup, rather than deduping at snu/ou level
             variable == "GRAND_TOT",                                      #just want total expenditure
             fy17 !=0, !is.na(fy17)) %>%                                   #keep only mech reporting expenditure >$0 on any indicator
      #adjustments to match MER data
      rename(operatingunit = ou,
             fundingagency = mech_agency) %>%                                         
      mutate(fundingagency = case_when(fundingagency == "DoD"               ~ "DOD",        
                                       fundingagency == "CDC"               ~ "HHS/CDC",
                                       fundingagency == "Peace Corps"       ~ "PC",
                                       fundingagency == "HRSA"              ~ "HHS/HRSA",
                                       fundingagency == "SAMHSA"            ~ "HHS/SAMHSA",
                                       fundingagency == "State_AF"          ~ "State/AF",
                                       fundingagency == "State_PRM"         ~ "State/PRM",
                                       TRUE                                 ~ fundingagency),
             operatingunit = case_when(operatingunit == "CotedIvoire"       ~ "Cote d'Ivoire",
                                       operatingunit == "DominicanRepublic" ~ "Dominican Republic",
                                       operatingunit == "DRC"               ~ "Democratic Republic of the Congo",
                                       operatingunit == "Guyana"            ~ "Caribbean Region",
                                       operatingunit == "PapuaNewGuinea"    ~ "Papua New Guinea",
                                       operatingunit == "SouthAfrica"       ~ "South Africa",
                                       operatingunit == "SouthSudan"        ~ "South Sudan",
                                       TRUE                                 ~ operatingunit)
      )
      rm(df_ea)
  #aggregate up to mech total (currently one line per PSNU)
    df_denom <- df_denom %>% 
      group_by(operatingunit, fundingagency, mechanismid) %>% 
      summarise(fy2017_expenditure = sum(fy17)) %>% 
      ungroup() %>% 
      mutate(fy2017_expenditure = round(fy2017_expenditure,0))


# Inspect and Define Indicators -------------------------------------------

  #look at full ind list to determine which are "original" vs "derivative"
    #df_mer %>%  
      #  filter(fy2017apr !=0, !is.na(fy2017apr)) %>%         #keep only mech reporting >=1 value on any indicator
      #  distinct(indicator) %>%                              #distinct list of mechanisms reporting on any indicator
      #  arrange(indicator) %>%                               #sort alphabetically
      #  write_csv("~/tmp/distinct_ind.csv")                  #export for inspections/documentation

  #list of "original (non-derivative) indicator
    ind_lst <- c("EMR_SITE", "FPINT_SITE", "GEND_GBV", "HRH_CURR", "HRH_PRE", "HRH_STAFF", "HTS_TST", 
                 "KP_MAT", "KP_PREV", "LAB_PTCQI", "OVC_HIVSTAT", "OVC_SERV", "PMTCT_ART", "PMTCT_EID", 
                 "PMTCT_EID_POS", "PMTCT_FO", "PMTCT_STAT", "PP_PREV", "PrEP_NEW", "SC_STOCK", "TB_ART", 
                 "TB_PREV", "TB_STAT", "TX_CURR", "TX_NEW", "TX_PVLS", "TX_RET", "TX_TB", "VMMC_CIRC") 

# Munge MER ---------------------------------------------------------------

  #comparison table (to denom)
    df_num <- df_mer %>%  
      filter(fundingagency != "Dedup",                                      #filter out dedups
             fy2017apr !=0, !is.na(fy2017apr),                              #keep only mech reporting >=1 value on any indicator
             indicator %in% ind_lst) %>%                                    #keep just "original", not "derived indicators
      distinct(operatingunit, fundingagency, mechanismid, indicator) %>%    #distinct list of mechanisms reporting on any indicator
      count(operatingunit, fundingagency, mechanismid) %>%                  #create freq table
      rename(fy2017_ind_count = n)                                          #rename for clarity
    rm(df_mer)

# Merge -------------------------------------------------------------------

  df_combo <- full_join(df_num, df_denom, by = c("operatingunit", "fundingagency", "mechanismid")) %>% 
    mutate(fy2017_ind_count = ifelse(is.na(fy2017_ind_count), 0, fy2017_ind_count)) %>% 
    arrange(operatingunit, fy2017_ind_count, mechanismid) %>% 
    mutate(implementingmechanismname = "[Not in FACTSInfo COP Matrix Report, COP 16-18]",
           primepartner = "[Not in FACTSInfo COP Matrix Report, COP 16-18]") %>% 
    rename_official("C:/Users/achafetz/Documents/GitHub/PartnerProgress/RawData") %>% 
    select(operatingunit, fundingagency, mechanismid, implementingmechanismname, primepartner, fy2017_ind_count, fy2017_expenditure)
  rm(df_denom, df_num)


# Light Touch -------------------------------------------------------------  
#light touch is defined as a mechanism that only reports on 0 or 1 MER indicators
#universe of indicators (total indicators) comes from any mechanism spending PEPFAR dollars in FY17 (EA)

  #count of light touch mechanisms by agency
    df_light <- df_combo %>% 
      filter(fy2017_ind_count < 2) %>% 
      count(fundingagency, sort = TRUE) %>% 
      rename(ind_lighttouch = n)
  
  #number of total mechanisms by agency  
    df_total <- df_combo %>% 
      count(fundingagency, sort = TRUE) %>% 
      rename(ind_total = n)
  
  #join final dataset of total mechs, light touch and share by agency 
    (df_ind <- full_join(df_total, df_light) %>% 
        arrange(desc(ind_lighttouch)) %>% 
        mutate(ind_lighttouch = ifelse(is.na(ind_lighttouch), 0L, ind_lighttouch),
               share = round(ind_lighttouch/ind_total*100, 0) %>% as.integer(),
               ind_lighttouch_share = paste0(share, "%")) %>%
        select(-share))
    rm(df_light, df_total)
  
  #look at the distro of zero vs one indicator mechanisms
    df_combo %>% 
      filter(fy2017_ind_count < 2) %>% 
      count(fundingagency, fy2017_ind_count) %>% 
      spread(fy2017_ind_count, n)
  
  #number of mechanisms by OU
    df_combo %>% 
      count(operatingunit)
    
  #number of light touch mechanism by OU and Agency
    df_combo %>% 
      filter(fy2017_ind_count < 2) %>%
      mutate(fundingagency = case_when(fundingagency == "USAID" ~ "USAID",
                                       fundingagency == "HHS/CDC" ~ "HHS/CDC",
                                       TRUE                       ~ "All Other"),
             fundingagency = factor(fundingagency, 
                                    levels = c("USAID", "HHS/CDC", "All Other"))) %>% 
      count(operatingunit, fundingagency) %>%
      spread(fundingagency, n, fill = 0) 

# Export ------------------------------------------------------------------
    
  write_csv(df_combo, file.path(exportfolder, "FY17 Ind Count and Budget by Mech.csv"))
    