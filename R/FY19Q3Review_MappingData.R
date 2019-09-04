# PROJECT: FY19Q3 OHA REVIEW
## PURPOSE: provide a sample dataset for mapping
## AUTHOR:  A.CHAFETZ | USAID
## DATE:    2019-08-20
## UPDATED: 2019-08-26


# DEPENDENCIES ------------------------------------------------------------

  library(ICPIutilities)
  library(tidyverse)


# IMPORT DATA -------------------------------------------------------------

  path_msd <- "~/ICPI/Data/MER_Structured_Datasets_PSNU_IM_FY17-19_20190815_v1_1.rds"
  path_nat <- "~/ICPI/Data/MER_Structured_Datasets_NAT_SUBNAT_FY15-20_20190815.rds"

  #Genie export
    df <- read_rds(path_msd)  %>% 
      filter(fundingagency == "USAID")
    
  #NAT_SUBNAT from PEPFAR.net
    df_nat <- read_rds(path_nat) %>% 
      filter(operatingunit %in% unique(df$operatingunit))


# MUNGE -------------------------------------------------------------------

  #append datasets
    df_combo <- bind_rows(df, df_nat)
  
  #filter data & adjust variable names
    df_combo <- df_combo %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_PVLS", "PLHIV", "TX_CURR_SUBNAT"), 
             disaggregate %in% c("Total Numerator", "Total Denominator"),
             fiscal_year == 2019) %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator))
  
  #aggregate
    df_combo <- df_combo %>% 
      group_by(operatingunit, psnu, psnuuid, mech_code, mech_name, primepartner, fiscal_year, indicator) %>% 
      summarise_at(vars(cumulative, starts_with("qtr"), targets), sum, na.rm = TRUE) %>% 
      ungroup()
  
  #create VL
    df_coverage <- df_combo %>% 
      filter(indicator %in% c("TX_CURR", "TX_PVLS_D")) %>% 
      select(-cumulative, -targets) %>% 
      gather(qtr, val, starts_with("qtr")) %>% 
      spread(indicator, val) %>% 
      unite(fiscal_year, qtr, col = "pd", sep = "", remove = FALSE) %>%
      arrange(operatingunit, psnu, mech_code, pd) %>% 
      group_by(psnuuid, mech_code) %>% 
      mutate(VL_Coverage = TX_PVLS_D/lag(TX_CURR, 2)) %>%
      filter(pd == "2019qtr3") %>% 
      select(-TX_CURR, -TX_PVLS_D, -pd, -qtr) %>% 
      gather(indicator, cumulative, VL_Coverage) %>% 
      filter(cumulative >= 0, !is.infinite(cumulative)) 
  
  #create achievement
    df_ach <- df_combo %>% 
      select(-starts_with("qtr")) %>% 
      mutate(achievement = round(cumulative/targets, 3))
  
  #reshape so vars are wide and clean up
    df_ach <- df_ach %>% 
      bind_rows(df_coverage) %>% 
      gather(type, val, cumulative, targets, achievement, na.rm = TRUE) %>% 
      mutate(val = na_if(val, 0)) %>% 
      filter(!is.na(val), psnuuid != "?") %>% 
      spread(indicator, val) %>% 
      mutate(VL_Suppression_Doc = VL_Coverage * (TX_PVLS/TX_PVLS_D))


# EXPORT ------------------------------------------------------------------

  write_csv(df_ach, paste0("../Downloads/FY19Q3_OHA_Review_Map_Data_",Sys.Date() %>% str_remove_all("-"),".csv"), na = "")