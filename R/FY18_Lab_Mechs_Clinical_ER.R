##  Project:  LAB PARTNERS IN THE CLINICAL SPACE
##  Author:   Aaron Chafetz, USAID
##  Purpose:  identify the clinic work & expenditure for Lab IPs
##  Date:     2019-08-29
##  Updated:  2019-09-04


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)


# PATHS -------------------------------------------------------------------

  path_msd <- "~/ICPI/Data/MER_Structured_Datasets_OU_IM_FY17-19_20190815_v1_1.rds"
  path_fast <- "~/ICPI/Data/COP19_FAST_Consolidated_20190606_PRE_POST_FINAL.csv"
  folderpath_output <- "../Downloads"
  

# MER MUNGING -------------------------------------------------------------

  df <- path_msd %>% 
    read_rds() %>% 
    rename_official()

  df_output <- df %>% 
    filter(fiscal_year == 2018,
           str_detect(indicator, "LAB|HTS_TST$|TX_NEW")) %>% 
    mutate(indicator = case_when(str_detect(indicator, "LAB") ~ "LAB",
                                 TRUE ~ indicator)) %>% 
    group_by(operatingunit, fundingagency, mech_code, mech_name, primepartner, indicator) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(cumulative > 0) %>% 
    #mutate(cumulative = 1) %>% 
    spread(indicator, cumulative) %>% #spread(indicator, cumulative, fill = 0) %>% 
    filter(LAB > 0) %>%  #filter(LAB == 1) %>% 
    mutate(LAB = "X") %>%  #mutate_at(vars(HTS_TST, LAB, TX_NEW), ~ case_when(. == 1 ~ "X")) %>% 
    select(-HTS_TST, -TX_NEW, everything()) %>% 
    rename_at(vars(HTS_TST, LAB, TX_NEW), ~ paste0("FY18MER_", .))

# FAST MUNGING ------------------------------------------------------------

  df_fast <- read_csv(path_fast, col_types = c(.default = "c")) %>% 
    mutate(amt = as.numeric(amt))

  lab_mechs <- unique(df_output$mech_code) 

  df_fast_out <- df_fast %>% 
    filter(mechanismid %in% lab_mechs,
           amt_type == "Expenditure",
           ((program == "ASP" &  programarea == "Laboratory Systems Stregthening") |
              program %in% c("HTS", "C&T"))) %>%
    mutate(programarea = case_when(str_detect(programarea, "Lab") ~ "Lab",
                                   program == "HTS" ~ "",
                                   TRUE ~ "Non-Lab")) %>% 
    unite("program_type", c("program", "programarea"), sep = "/") %>% 
    mutate(program_type = str_remove(program_type, "/$"),
           program_type = paste0("FY18ER_", program_type)) %>% 
    count(mechanismid, program_type, wt = amt) %>% 
    spread(program_type, n) 


# JOIN & EXPORT -----------------------------------------------------------

  df_output %>% 
    left_join(df_fast_out, by = c("mech_code" = "mechanismid")) %>% 
    write_csv(file.path(folderpath_output, "FY18LabClinicalMechanismsbyAgency.csv"), na = "")
