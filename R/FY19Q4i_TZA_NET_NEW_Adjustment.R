## PROJECT:  TZA FY19 NET NEW ADJUSTMENTS
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Adjustments to Net New given regional shifts
## DATE:     2019-11-07
## UPDATED:  2019-11-27

#dependencies
  library(tidyverse)
  library(ICPIutilities)

#import
  path <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE)
  df <- read_rds(path)

#filter
  df <- df %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator")
  
#rename Baylor
  df <- df %>% 
    mutate(primepartner = ifelse(str_detect(primepartner, "Baylor|BAYLOR"), "Baylor", primepartner))

#identify the partner working in region currently
  partners <- df %>% 
    reshape_msd("long") %>% 
    filter(indicator == "TX_CURR",
           period == "fy2019q4",
           !mech_code %in% c("16787", "17103", "70356")) %>% 
    group_by(fundingagency, snu1, period, primepartner) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    select(snu1, primepartner, fundingagency)
  
#snu trends
  df_adj <- df %>% 
    reshape_msd("long") %>% 
    filter(indicator == "TX_CURR",
           !mech_code %in% c("16787", "17103", "70356"),
           !period %in% c("fy2017_targets", "fy2017cumulative", 
                          "fy2018_targets", "fy2018cumulative",
                          "fy2020_targets")) %>% 
    group_by(snu1, period) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(period, val) %>% 
    left_join(partners, by = "snu1") %>% 
    select(snu1, fundingagency, primepartner, everything()) %>% 
    select(-fy2019cumulative, -fy2019_targets, everything())

  
#partner adjust
    partner_map <- tibble::tribble(
      ~primepartner, ~partner_short,
      "Elizabeth Glaser Pediatric Aids Foundation",        "EGPAF",
      "MANAGEMENT AND DEVELOPMENT FO R HEALTH",          "MDH",
      "DELOITTE CONSULTING LIMITED",     "Deloitte",
      "AMREF HEALTH AFRICA HQ",        "Amref",
      "ARIEL GLASER PEDIATRIC AIDS H EALTHCARE INITIATIVE", "APAHI",
      "Henry M. Jackson Foundation For The Advancement Of Military Medicine, Inc., The", "Henry Jackson")
  
#adjust partner names and reorder variables
  df_adj <- df_adj %>% 
    left_join(partner_map) %>% 
    mutate(primepartner = ifelse(is.na(partner_short), primepartner, partner_short),
           fundingagency = str_remove(fundingagency, "HHS/")) %>% 
    select(-partner_short) 
  
#TX_CURR achievement
  df_adj_tx <- df_adj %>% 
    select(-fy2019cumulative, -fy2019_targets, everything()) %>% 
    group_by(fundingagency, primepartner) %>% 
    summarise_at(vars(starts_with("fy")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    select(-fy2019_targets, everything()) %>% 
    mutate(fy2019achv = fy2019q4/fy2019_targets) %>% 
    add_column(indicator = "TX_CURR", .after = "primepartner")
  
#TX_NET_NEW calculation
  df_adj_nn_targets <- df_adj %>% 
    mutate(fy2019_targets = fy2019_targets - fy2018q4) %>% 
    select(snu1, fundingagency, primepartner, fy2019_targets)
  
#TX_NET_NEW achievement
  df_adj_nn <- df_adj %>% 
    select(-fy2019_targets, -fy2019cumulative) %>% 
    gather(period, val, starts_with("fy")) %>% 
    arrange(snu1, period) %>% 
    group_by(snu1) %>% 
    mutate(nn = val - lag(val)) %>% 
    ungroup() %>% 
    select(-val) %>% 
    spread(period, nn) %>% 
    mutate(fy2019cumulative = fy2019q1 + fy2019q2 + fy2019q3 + fy2019q4) %>% 
    left_join(df_adj_nn_targets) %>% 
    group_by(fundingagency, primepartner) %>% 
    summarise_at(vars(starts_with("fy")), sum, na.rm = TRUE) %>%
    ungroup() %>% 
    mutate(fy2019achv = fy2019cumulative / fy2019_targets) %>% 
    add_column(indicator = "TX_NET_NEW", .after = "primepartner") %>% 
    bind_rows(df_adj_tx, .) %>% 
    select(-starts_with("fy2017"))
  
  write_csv(df_adj_nn, "../Downloads/TZA_Q4_TX_Adj.csv", na = "") 
  
  
  
#partners unadjusted trends
  df_unadj <- df %>% 
    reshape_msd("long") %>% 
    filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
           !period %in% c("fy2017_targets", "fy2017cumulative", 
                          "fy2018_targets", "fy2018cumulative",
                          "fy2020_targets")) %>% 
    left_join(partner_map) %>% 
    mutate(primepartner = ifelse(is.na(partner_short),primepartner, partner_short),
           fundingagency = str_remove(fundingagency, "HHS/")) %>% 
    select(-partner_short) %>% 
    group_by(fundingagency, primepartner, indicator, period) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(period, val) %>% 
    select(-fy2019cumulative, everything()) %>% 
    select(-fy2019_targets, everything()) %>%
    mutate(fy2019achv = fy2019cumulative / fy2019_targets) %>% 
    arrange(indicator) %>% 
    select(-starts_with("fy2017"))
  
  write_csv(df_unadj, "../Downloads/TZA_Q4_TX_unAdj.csv", na = "") 

  
  
