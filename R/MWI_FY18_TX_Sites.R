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
    summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(fy2018apr != 0)

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
    mutate(site_tot = sum(fy2018apr),
           patient_grp = case_when(site_tot >= 2000 ~ "2k+",
                                   site_tot >= 1000 ~ "1k+",
                                   site_tot >=  500 ~ "500+",
                                   TRUE             ~ "<500"),
           patient_grp = factor(patient_grp, c("2k+", "1k+", "500+", "<500")))
          
#site counts 
  df_mwi_tx %>% 
    group_by(psnu, name, patient_grp) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    spread(patient_grp, n) %>% 
    mutate_if(is.integer, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = `2k+` + `1k+` + `500+` + `<500`) %>% 
    arrange(psnu, total)
  
    
  