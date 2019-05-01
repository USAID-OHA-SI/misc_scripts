##  PROJECT:  
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  Review Tanzania portfolio
##  DATE:     2019-02-11


# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(scales)
library(extrafont)
library(ICPIutilities)


# IMPORT DATA -------------------------------------------------------------

  #GENIE PULL 
  #  - Indicators: HTS_TST, HTS_TST_POS, TX_NEW, TX_CURR
  #  - Date: 2019-02-11

  #site data
   df_tza <- match_msd("~/ICPI/Data/PEPFAR-Data-Genie-SiteByIMs-2019-02-11 (2).zip")


# PLOT THEME --------------------------------------------------------------

   plot_theme <- function() {
     theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
           axis.ticks = element_blank(),
           legend.position = "none",
           panel.background = element_blank(),
           strip.background = element_blank(),
           strip.text = element_text(face = "bold", size = 14, color = "#595959"),
           panel.grid = element_line(color = "#ebebeb"),
           plot.title = element_text(size = 15, face = "bold", color = "black"),
           plot.caption = element_text(size = 11,  color = "#595959")
     )
   }


# MUNGE -------------------------------------------------------------------

   
  df_tza <- df_tza %>% 
    filter(fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    mutate(fundingagency = str_remove(fundingagency, "HHS/"))
   
  grps <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "TX_CURR"),
          !fy2018apr %in% c(0, NA))%>% 
    group_by(orgunituid, fundingagency, mechanismid, indicator) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    group_by(fundingagency, indicator) %>% 
    mutate(qtile = ntile(fy2018apr, 4)) %>% 
    ungroup() %>% 
    mutate(ind_grp = str_remove(indicator, "_TST|_CURR") %>% tolower(.),
           grp = case_when(qtile == 4 ~ "High",
                           qtile == 1 ~ "Low",
                           TRUE       ~ "Mid")) %>% 
    select(-fy2018apr, -qtile, -indicator)
  
  
  df_tza <- df_tza %>%
    mutate(ind_grp = case_when(str_detect(indicator, "HTS") ~ "hts",
                               str_detect(indicator, "TX") ~ "tx")) %>% 
    left_join(grps, by = c("orgunituid", "orgunituid", "fundingagency", "mechanismid", "ind_grp"))
     
  df_tza <- df_tza %>% 
    mutate(fundingagency = factor(fundingagency, c("USAID", "CDC")),
           grp = factor(grp, c("Low", "Mid", "High")))
   
  
#largest sites
  
  top <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "TX_CURR")) %>% 
    group_by(orgunituid, sitename, fundingagency, mechanismid, indicator) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(fundingagency, indicator, desc(fy2018apr)) %>% 
    group_by(fundingagency, indicator) %>% 
    top_n(10, wt = fy2018apr) %>% 
    ungroup()
  
  top_hts <- top %>% 
    filter(indicator == "HTS_TST") %>% 
    select(-indicator, -fy2018apr)
  
  top_tx <- top %>% 
    filter(indicator == "TX_CURR") %>% 
    select(-indicator, -fy2018apr)
    
  
# VISUALIZE ---------------------------------------------------------------

  
  
## TX_NEW
  
#Site Achievement
  
  df_ach <- df_tza %>% 
    filter(indicator == "TX_NEW",
           !is.na(grp)) %>% 
    group_by(fundingagency, mechanismid, grp) %>% 
    summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(!fy2018_targets %in% c(0, NA)) %>% 
    mutate(fy2018ach = fy2018apr/fy2018_targets,
           lab = case_when(mechanismid == "17986" ~ grp))
  
  df_ach %>% 
    ggplot(aes(mechanismid, fy2018ach, color = grp)) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab),
               vjust = -1.2, family = "Gill Sans MT",
               na.rm = TRUE) +
    coord_flip() +
    scale_y_continuous(label = percent) + 
    scale_color_manual(values = c("#3498db", "#9b59b6", "#34495e")) +
    facet_grid(fundingagency ~ ., scales = "free_y") +
    expand_limits(y = 0) +
    labs(y = "FY18 Target Achievement", x = "",
         title = "TX_NEW Partner Site Achievement",
         caption = "Note: Achievement capped at 200%") +
    plot_theme() 
  
#Trends
  df_trend <- df_tza %>% 
    filter(indicator == "TX_NEW",
           !is.na(grp)) %>% 
    group_by(fundingagency, grp) %>% 
    summarise_at(vars(contains("q")), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    arrange(fundingagency, grp, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           linegrp = paste(fundingagency, grp),
           lab = case_when(fundingagency == "USAID" & pd == "FY18Q1" ~ grp))
  
  df_trend %>% 
    ggplot(aes(pd, val, group = linegrp, color = grp)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_text(aes(label = lab),
              vjust = -1.2, family = "Gill Sans MT",
              na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#9b59b6", "#34495e"))  +
    facet_grid(. ~ fundingagency) +
    labs(x = "", y = "TX_NEW (avg)") + 
    plot_theme()
  

  df_trend <- df_tza %>% 
    filter(indicator == "TX_NEW",
           !is.na(grp)) %>% 
    group_by(fundingagency, grp) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    arrange(fundingagency, grp, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           linegrp = paste(fundingagency, grp))
  
  df_trend %>% 
    ggplot(aes(pd, val, group = linegrp, fill = grp)) +
    geom_col() +
    scale_fill_manual(values = c("#3498db", "#9b59b6", "#34495e"))  +
    scale_y_continuous(labels = comma) +
    facet_grid(grp ~ fundingagency) +
    labs(x = "", y = "TX_NEW") + 
    plot_theme()
  
  
  
  df_trend <- df_tza %>% 
    filter(indicator == "TX_NEW",
           !is.na(grp)) %>% 
    group_by(fundingagency, grp) %>% 
    summarise_at(vars(contains("q")), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    arrange(fundingagency, grp, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           linegrp = paste(fundingagency, grp),
           lab = case_when(fundingagency == "USAID" & pd == "FY18Q1" ~ grp))  
  
  
  
  df_trend <- df_tza %>% 
    filter(indicator == "TX_NEW") %>% 
    inner_join(top_tx, by = c("orgunituid", "sitename", "fundingagency", "mechanismid")) %>% 
    group_by(sitename, fundingagency, mechanismid) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    unite(name, c(sitename, fundingagency, mechanismid), sep = " ", remove = FALSE) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.)) %>% 
    arrange(name, pd)
  
  
  df_trend %>% 
    ggplot(aes(pd, val, group = name, color = fundingagency)) +
    geom_point(size = 3) + 
    geom_line(size = 1) +
    scale_color_manual(values = c("#16a085", "#34495e")) +
    facet_wrap(. ~ fundingagency) +
    labs(x = "", y = "TX_NEW", 
         title = "Trend in TX_NEW for 10 largest treatment sites") +
    plot_theme()
  
  # #Partner/Site Achievement
  #    
  #   df_ach <- df_tza %>% 
  #      filter(indicator == "TX_NEW") %>% 
  #      group_by(orgunituid, sitename, fundingagency, mechanismid, primepartner, implementingmechanismname) %>% 
  #      summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  #      ungroup() %>% 
  #      filter(!fy2018_targets %in% c(0, NA)) %>% 
  #      mutate(fy2018ach = fy2018apr/fy2018_targets,
  #             #fy2018ach = ifelse(is.finite(fy2018ach),fy2018ach, 0),
  #             fy2018ach = ifelse(fy2018ach > 2, 2, fy2018ach)) 
  
  # #histogram of site achievement by agency
  # df_ach %>% 
  #   ggplot(aes(fy2018ach)) +
  #   geom_histogram() +
  #   scale_x_continuous(label = percent) + 
  #   facet_grid(fundingagency ~ .) +
  #   labs(x = "FY18 Target Achievement", y = "# of sites",
  #        title = "TX_NEW Partner Site Achievement",
  #        caption = "Note: Achievement capped at 200%") +
  #   plot_theme()
  # 
  # #partner achievement
  #   df_ach %>% 
  #     ggplot(aes(mechanismid, fy2018ach)) +
  #     geom_boxplot() +
  #     geom_point(position = "jitter", alpha = .2, na.rm = TRUE) +
  #     scale_y_continuous(label = percent) + 
  #     facet_wrap(. ~ fundingagency, scales = "free_x") +
  #     labs(y = "FY18 Target Achievement", x = "",
  #          title = "TX_NEW Partner Site Achievement",
  #          caption = "Note: Achievement capped at 200%") +
  #     plot_theme()

  #trends
    df_trends <- df_tza %>% 
      filter(indicator == "TX_NEW") %>% 
      group_by(orgunituid, sitename, fundingagency, mechanismid, primepartner, implementingmechanismname) %>% 
      summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, val, contains("q")) %>% 
      mutate(pd = str_remove(pd, "20") %>% toupper(.)) %>% 
      arrange(fundingagency, mechanismid, orgunituid, pd) %>% 
      group_by(mechanismid, orgunituid) %>% 
      mutate(delta = ifelse(pd == "FY18Q1", 0, val - lag(val))) %>% 
      ungroup()

    df_trends %>% 
      filter(mechanismid == "16787") %>% 
      ggplot(aes(pd, val)) +
      geom_col()
    
    df_trends %>% 
      filter(mechanismid == "16787") %>% 
      ggplot(aes(pd, val, group = orgunituid)) +
      geom_line(size = 1) +
      geom_point()
    
    df_trends %>% 
      filter(mechanismid == "16787") %>%
      ggplot(aes(pd, delta, group = orgunituid)) +
      geom_line(size = 1) +
      geom_point()
    
    df_trends %>% 
      filter(mechanismid == "16787") %>%
      group_by(mechanismid, pd) %>% 
      summarise_at(vars(delta), mean, na.rm = TRUE) %>% 
      ungroup() %>% 
      ggplot(aes(pd, delta, group = mechanismid)) +
      geom_line(size = 1) +
      geom_point()
    
  # Linkage by Agency
    
    df_link <- df_tza %>%
      filter(fundingagency!= "Dedup",
             indicator %in% c("HTS_TST_POS", "TX_NEW")) %>% 
      group_by(fundingagency, orgunituid, indicator) %>% 
      summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, fy2018apr, fill = 0)

    df_link %>%
      filter(TX_NEW < 3000) %>% 
      ggplot(aes(HTS_TST_POS, TX_NEW, color = fundingagency)) +
      geom_point(size = 3, alpha = .4) +
      geom_abline(slope = 1, intercept = 0, color = "#595959") +
      scale_y_continuous(label = comma) +
      scale_x_continuous(label = comma) +
      facet_grid(. ~ fundingagency) +
      plot_theme()
      theme(legend.position = "none")
      
  #linakge by partner
    df_link_im <- df_tza %>%
      filter(fundingagency!= "Dedup",
             indicator %in% c("HTS_TST_POS", "TX_NEW")) %>% 
      group_by(fundingagency, orgunituid, mechanismid, indicator) %>% 
      summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, fy2018apr, fill = 0) %>% 
      mutate(linkage = TX_NEW/HTS_TST_POS,
             linkage = ifelse(linkage > 5, 5, linkage)) %>% 
      filter_at(vars(HTS_TST_POS, TX_NEW), any_vars(.!=0))
  
    df_link_im %>% 
      ggplot(aes(mechanismid, linkage)) +
      geom_boxplot() +
      geom_point(position = "jitter", alpha = .2, na.rm = TRUE) +
      scale_y_continuous(label = percent) + 
      facet_wrap(. ~ fundingagency, scales = "free_x") +
      labs(y = "FY18 Proxy Linkage", x = "",
           title = "Proxy Linkage",
           caption = "Note: Achievement capped at 500%") +
      plot_theme()
    
    df_link_im %>%
      filter(HTS_TST_POS <2000) %>% 
      ggplot(aes(HTS_TST_POS, linkage, color = fundingagency)) +
      geom_point(size = 3, alpha = .4) +
      geom_hline(yintercept = 1, color = "#595959") +
      scale_y_continuous(label = percent) +
      scale_x_continuous(label = comma) +
      facet_grid(. ~ fundingagency) +
      labs(y = "Proxy Linkage", x = "HTS_TST_POS",
           title = "FY18 HTS_TST_POS v Proxy Linkage",
           caption = "Note: HTS_TST_POS capped at 2,000 & achievement capped at 500%") +
      plot_theme()
    theme(legend.position = "none") 
    