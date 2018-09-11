#dependencies
  library(tidyverse)
  library(ICPIutilities)

#read in data
  df <- read_msd("~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20180815_v1_1_Caribbean_Region.txt")

#filter to just Jamacia & Barbados
  df <- df %>% 
    filter(countryname %in% c("Jamaica", "Barbados"),
           indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Total Numerator", "KeyPop/Result", "KeyPop/HIVStatus")) 
#add FY18 cumulative
  df <- add_cumulative(df)

#create a disagg combining key pops
  df <- df %>% 
    mutate(disagg = case_when(standardizeddisaggregate == "Total Numerator"                         ~ "Total",
                              otherdisaggregate == "People in prisons and other enclosed settings"  ~ "In Prison",
                              TRUE                                                                  ~ otherdisaggregate))
#one row per site
  df <- df %>%
    group_by(countryname, indicator, sitename, indicator, disagg) %>% 
    summarise_at(vars(starts_with("fy")), ~ sum(., na.rm = TRUE)) %>% 
    ungroup()
  
#linkage
  df_link <- df %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW"), disagg == "Total") %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(PROXY_LINK = round(TX_NEW/HTS_TST_POS,2)*100) %>% 
    filter(is.finite(PROXY_LINK), PROXY_LINK != 0) %>% 
    select(-HTS_TST_POS, -TX_NEW) %>% 
    spread(pd, PROXY_LINK) %>% 
    mutate(indicator = "PROXY_LINK")

#append
  df <- bind_rows(df, df_link)
  
  df <- df %>% 
    unite(ind_disagg, indicator, disagg) %>% 
    mutate(ind_disagg = factor(ind_disagg, 
                             levels = c("HTS_TST_Total", "HTS_TST_FSW", 
                                        "HTS_TST_MSM", "HTS_TST_TG", 
                                        "HTS_TST_POS_Total", "HTS_TST_POS_FSW", 
                                        "HTS_TST_POS_MSM", "HTS_TST_POS_TG",
                                        "TX_NEW_Total", "TX_NEW_FSW", "TX_NEW_MSM",
                                        "TX_NEW_TG", "TX_NEW_PWID", "TX_NEW_In Prison",
                                        "PROXY_LINK_Total","TX_CURR_Total"))) %>% 
    arrange(countryname, sitename, ind_disagg) %>% 
    select(-fy2019_targets)

  
# site <- unique(df$sitename)
# 
# data_frame(site = site, rep(ind_disagg = c("HTS_TST_Total", "HTS_TST_FSW", 
#                                        "HTS_TST_MSM", "HTS_TST_TG", 
#                                        "HTS_TST_POS_Total", "HTS_TST_POS_FSW", 
#                                        "HTS_TST_POS_MSM", "HTS_TST_POS_TG",
#                                        "TX_NEW_Total", "TX_NEW_FSW", "TX_NEW_MSM",
#                                        "TX_NEW_TG", "TX_NEW_PWID", "TX_NEW_In Prison",
#                                        "PROXY_LINK_Total","TX_CURR_Total"), 27)
#export
  write_csv(df, "C:/Users/achafetz/Downloads/Q3_CascadeAndLinkage_JAM_BRB.csv", na = "")
  
  
  