## Project: MWI Treatment Sites
## Author:  A.Chafetz
## Purpose: Determine which partners are working at which size sites
## Date:    2019-02-02

# ASK
# In a table, by district, how many sites per partner. It would be great 
# if you could indicate how many sites at 2,000 patients or more, 
# how many at 1,000 or more, and how many at 500 or more and below 500


#dependencies

library(tidyverse)
library(ICPIutilities)

#import data
  df_mwi <- read_msd("~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20181221_v2_1_Malawi.txt")

#filter to just treatment
  df_mwi_tx <- df_mwi %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator")

#clean up partner names
  df_mwi_tx <- rename_official(df_mwi_tx)

#summarize to site and partner
  df_mwi_tx <- df_mwi_tx %>% 
    group_by(orgunituid, sitename, psnu, fundingagency, mechanismid, primepartner, implementingmechanismname) %>% 
    summarize_at(vars(fy2018apr, fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup()

#clean up partner names
  names <- tribble(
    ~mechanismid,                                         ~name,
           "70185",                                      "Baylor",
           "18025", "Lighthouse (Achieving HIV Epidemic Control)",
           "18544",                                       "EGPAF",
           "18234",                                       "EQUIP",
           "18244",                                     "JHPIEGO",
           "14441",           "Lighthouse (Center of Excellence)",
           "17097",                                         "PCI")
  
  df_mwi_tx <- df_mwi_tx %>% 
    left_join(., names, by = "mechanismid") %>% 
    select(-mechanismid:-implementingmechanismname)
    

#groupings
  df_mwi_tx <- df_mwi_tx %>%
    arrange(orgunituid) %>% 
    group_by(orgunituid) %>% 
    mutate(site_tot_18 = sum(fy2018apr),
           site_tot_19 = sum(fy2019_targets),
           patient_grp_18 = case_when(site_tot_18 >= 2000 ~ "2k+",
                                      site_tot_18 >= 1000 ~ "1k+",
                                      site_tot_18 >=  500 ~ "500+",
                                      TRUE             ~ "<500"),
           patient_grp_19 = case_when(site_tot_19 >= 2000 ~ "2k+",
                                      site_tot_19 >= 1000 ~ "1k+",
                                      site_tot_19 >=  500 ~ "500+",
                                      TRUE             ~ "<500"),
           patient_grp_18 = factor(patient_grp_18, c("2k+", "1k+", "500+", "<500")),
           patient_grp_19 = factor(patient_grp_19, c("2k+", "1k+", "500+", "<500")))
  
  
#FY18 APR
  
#site counts 
  df_mwi_tx %>% 
    filter(fy2018apr != 0) %>% 
    group_by(psnu, name, patient_grp_18) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    spread(patient_grp_18, n) %>% 
    mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = `2k+` + `1k+` + `500+` + `<500`) %>% 
    arrange(psnu, total)
  
    
#site counts total
  df_mwi_tx %>% 
    filter(fy2018apr != 0) %>% 
    group_by(name, patient_grp_18) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    spread(patient_grp_18, n) %>% 
    mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = `2k+` + `1k+` + `500+` + `<500`) %>% 
    arrange(total)
  

#FY19 Targets
  
  #site counts 
  df_mwi_tx %>% 
    filter(fy2019_targets != 0) %>% 
    group_by(psnu, name, patient_grp_19) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    spread(patient_grp_19, n) %>% 
    mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = `2k+` + `1k+` + `500+` + `<500`) %>% 
    arrange(psnu, total)
  
  
  #site counts total
  df_mwi_tx %>% 
    filter(fy2019_targets != 0) %>% 
    group_by(name, patient_grp_19) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    spread(patient_grp_19, n) %>% 
    mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = `2k+` + `1k+` + `500+` + `<500`) %>% 
    arrange(total)
  
  