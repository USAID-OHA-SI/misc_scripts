##  PROJECT:  
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  Review Tanzania portfolio
##  DATE:     2019-02-11


# DEPENDENCIES ------------------------------------------------------------


  library(tidyverse)
  library(scales)
  library(extrafont)
  library(ICPIutilities)
  library(ggrepel)

# IMPORT DATA -------------------------------------------------------------


  #OUxIM data
  df_tza <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.rds") %>% 
    filter(operatingunit == "Tanzania")
  
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
  



# VISUALIZE ---------------------------------------------------------------

  
#TX_NET_NEW v TX_NEW diff - PSNU
  top_tx <- df_tza %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator",
             psnu!= "_Military Tanzania") %>% 
      group_by(psnu) %>%
      summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      arrange(desc(fy2018apr)) %>% 
      top_n(16) %>% 
      pull(psnu)
  

  df_nn <- df_tza %>% 
    filter(indicator %in% c("TX_NET_NEW", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(psnu, indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    arrange(psnu, indicator, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           yr = str_sub(pd, 3,4),
           grp = paste(indicator, yr))
  
  df_nn %>%
    filter(psnu %in% top_tx) %>% 
    ggplot(aes(pd, val, group = grp, indicator, color = indicator)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_wrap(. ~ psnu) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY17Q1", rep("", 3), "FY18Q1", rep("", 3))) +
    theme(legend.position = "top") +
    plot_theme()
  
#INDEX SHARE - PNSU
  
  top_hts <- df_tza %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Total Numerator",
           psnu!= "_Military Tanzania") %>% 
    group_by(psnu) %>%
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(fy2018apr)) %>% 
    top_n(16) %>% 
    pull(psnu)
  
  df_index <- df_tza %>% 
    filter(psnu != "_Military Tanzania",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
    mutate(modality = ifelse(modality == "IndexMod", "Index", modality)) %>% 
    group_by(psnu, modality) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    group_by(psnu, pd) %>% 
    mutate(share = val/sum(val)) %>% 
    ungroup() %>% 
    filter(modality == "Index") %>% 
    arrange(psnu, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           yr = str_sub(pd, 3,4),
           grp = paste(psnu, yr))
  
  df_index %>% 
    filter(psnu %in% top_hts) %>% 
    ggplot(aes(pd, share, group = grp, color = modality)) +
    geom_hline(yintercept = 0, color = "#595959") +
    geom_path(size = 1) +
    geom_point(size = 3) +
    facet_wrap(. ~ psnu) +
    scale_color_manual(values = "#16a085") +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY17Q1", rep("", 3), "FY18Q1", rep("", 3))) +
    labs(title = "FY17-18 Index Share",
         x = "", y = "index pos. testing as a share of all pos. testing") +
    plot_theme()
  
  
#MALE SHARE - PSNU
  df_male <- df_tza %>% 
    filter(psnu != "_Military Tanzania",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
    mutate(sex = ifelse(agecoarse == "<15", "Peds", sex)) %>% 
    filter(!is.na(sex)) %>% 
    group_by(psnu, sex) %>% 
    summarise_at(vars(contains("fy2018q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    group_by(psnu, pd) %>% 
    mutate(share = val/sum(val)) %>% 
    ungroup() %>% 
    filter(sex == "Male") %>% 
    arrange(psnu, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           yr = str_sub(pd, 3,4),
           grp = paste(psnu, yr))
  
  df_male %>% 
    filter(psnu %in% top_hts) %>% 
    ggplot(aes(pd, share, group = grp, color = sex)) +
    geom_hline(yintercept = 0, color = "#595959") +
    geom_path(size = 1) +
    geom_point(size = 3) +
    facet_wrap(. ~ psnu) +
    scale_color_manual(values = "#16a085") +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY18Q1", rep("", 3))) +
    labs(title = "FY18 Male Share",
         x = "", y = "male pos. testing as a share of all pos. testing") +
    plot_theme()
  
#LINKAGE - PSNU
  
  df_link <- df_tza %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(psnu, indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    spread(indicator, val) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS) %>% 
    arrange(psnu, pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           yr = str_sub(pd, 3,4),
           grp = paste(psnu, yr),
           operatingunit = "Tanzania")

  df_link %>% 
    filter(psnu %in% top_hts) %>% 
    ggplot(aes(pd, proxylink, group = grp, color = operatingunit)) +
    geom_hline(yintercept = 0, color = "#595959") +
    geom_hline(yintercept = 1, color = "#595959", linetype = 2) +
    geom_path(size = 1) +
    geom_point(size = 3) +
    facet_wrap(. ~ psnu) +
    scale_color_manual(values = "#16a085") +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY17Q1", rep("", 3), "FY18Q1", rep("", 3))) +
    labs(title = "FY17-18 Index Share",
         x = "", y = "proxy linkage (TX_NEW/HTS_POS)") +
    plot_theme()
  
#Average Linkage - PSNU
  df_link_avg <- df_tza %>% 
    filter(psnu %in% top_hts,
           indicator %in% c("HTS_TST_POS", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    spread(indicator, val) %>% 
    mutate(proxylink = TX_NEW / HTS_TST_POS) %>% 
    arrange(pd) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           yr = str_sub(pd, 3,4),
           grp = paste(yr),
           operatingunit = "Tanzania")
  
  df_link %>% 
    filter(psnu %in% top_hts) %>% 
    ggplot(aes(pd, proxylink, group = grp, color = operatingunit)) +
    geom_hline(yintercept = 0, color = "#595959") +
    geom_hline(yintercept = 1, color = "#595959", linetype = 2) +
    geom_point(size = 3, alpha = .2) +
    geom_point(data = df_link_avg, aes(pd, proxylink), size = 4, color = "#16a085") +
    geom_line(data = df_link_avg, aes(pd, proxylink), size = 1, color = "#16a085") + 
    scale_color_manual(values = "#7f8c8d") +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(labels = c("FY17Q1", rep("", 3), "FY18Q1", rep("", 3))) +
    labs(title = "FY17-18 Index Share",
         x = "", y = "proxy linkage (TX_NEW/HTS_POS)") +
    plot_theme()
  
  
#Slope
  df_tx <- df_tza %>% 
    filter(indicator == "TX_NEW",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(psnu, indicator) %>% 
    summarise_at(vars(fy2017apr, fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(growth = fy2018apr > fy2017apr,
           lab17 = case_when(growth == TRUE ~paste0(psnu, ", ", comma(fy2017apr))),
           lab18 =  case_when(growth == TRUE ~paste0(comma(fy2018apr))))

  df_tx %>% 
    filter(psnu %in% top_tx) %>% 
    ggplot() +
    geom_segment(aes(x = 1, xend = 2, y = fy2017apr, yend = fy2018apr, color = growth), 
                 size = .75) +
    geom_vline(xintercept = c(1, 2), linetype = "dashed", size = .1) +
    scale_color_manual(labels = c("Up", "Down"), 
                       values = c("#7f8c8d", "#16a085")) +
    labs(x = "", y = "", title = "Change in TX_NEW") +
    xlim(.5, 2.5) + 
    ylim(0,(1.1*(max(df_tx$fy2017apr, df_tx$fy2018apr)))) +
    geom_text(aes(x = 1, y = fy2017apr, label = lab17), 
              hjust=1.1, size=3.5, family = "Gill Sans MT",
              check_overlap = TRUE,
              na.rm = TRUE) +
    geom_text(aes(x = 2, y = fy2018apr, label = lab18), 
              hjust=-0.1, size=3.5, family = "Gill Sans MT",
              check_overlap = TRUE,
              na.rm = TRUE) +
    geom_text(label="FY17", x=1, y=1.1*(max(df_tx$fy2017apr, df_tx$fy2018apr)), 
              hjust=1.2, size=5, family = "Gill Sans MT") +
    geom_text(label="FY18", x=2, y=1.1*(max(df_tx$fy2017apr, df_tx$fy2018apr)), 
              hjust=-0.1, size=5, family = "Gill Sans MT") +
    plot_theme() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank())
  
  
  
  
  
  
  
  