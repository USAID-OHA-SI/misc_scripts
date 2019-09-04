##  Project:  FY19Q3 IP Meeting
##  Author:   Aaron Chafetz, USAID
##  Purpose:  identify the clinic work & expenditure for Lab IPs
##  Date:     2019-09-03

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)


# VARIABLES ---------------------------------------------------------------

  #path
    path_msd <- "~/ICPI/Data/MER_Structured_Datasets_OU_IM_FY17-19_20190815_v1_1.rds"
    folderpath_output <- "../Downloads/"
  #colors
    lblue <- "#A7C6ED"  
    blue <- "#002F6C"


# MUNGE -------------------------------------------------------------------

  #import
    df_mer <- read_rds(path_msd)

  #limit to NET_NEW and TX_CURR
    df_fltr <- df_mer %>% 
      filter(fundingagency %in% c("USAID", "HHS/CDC"),
             indicator %in% c("TX_NET_NEW", "TX_CURR"),
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(operatingunit, fiscal_year, indicator, fundingagency) %>% 
      summarise_at(vars(targets, contains("qtr"), cumulative), sum, na.rm = TRUE) %>% 
      ungroup()

  #net new targets
    df_nn <- df_fltr %>% 
      dplyr::filter(indicator == "TX_CURR") %>% 
      reshape_msd("wide") %>% 
      dplyr::group_by_if(is.character) %>% 
      dplyr::summarise_at(vars(fy2018q4,fy2019_targets), sum, na.rm = TRUE) %>% 
      dplyr::ungroup()
    
    df_nn <- df_nn %>% 
      dplyr::mutate(indicator = "TX_NET_NEW") %>% 
      dplyr::mutate(fy2019_targets = fy2019_targets - fy2018q4) %>% 
      dplyr::filter_if(is.numeric, dplyr::any_vars(. != 0)) %>% 
      dplyr::select(-fy2018q4) %>% 
      tidyr::gather(period, val, fy2019_targets)

  #reshape and add net new targets
    df_long <- df_fltr %>% 
      reshape_msd("long") %>% 
      bind_rows(df_nn)
    
  #create an orderign for the ous
    ou_order <- df_long %>% 
      filter(period == "fy2019cumulative",
             indicator == "TX_CURR") %>% 
      spread(fundingagency, val) %>% 
      arrange(desc(USAID), desc(`HHS/CDC`)) %>% 
      pull(operatingunit)

  #tweak for visual df
    df_viz <- df_long %>% 
      filter(period %in% c("fy2018q3", "fy2018q4", "fy2019q1", 
                           "fy2019q2", "fy2019q3", 
                           "fy2019cumulative", "fy2019_targets")) %>% 
      spread(indicator, val) %>% 
      mutate(period = str_remove(period, "20") %>% toupper,
             fy = str_sub(period, end = 4),
             placeholder = TX_NET_NEW * 1.9,
             operatingunit = factor(operatingunit, ou_order)) %>% #spacing for labels
      filter(str_detect(period, "Q"),
             fundingagency == "USAID")


# VISUAL ------------------------------------------------------------------

  #NET NEW small multiples by OU
    df_viz %>% 
      ggplot(aes(period, TX_NET_NEW, fill = fy)) +
      geom_hline(yintercept = 0, size = .8) + 
      geom_col(na.rm = TRUE) +
      geom_text(aes(label = comma(TX_NET_NEW)), 
                size = 2.5, color = "gray30",
                vjust = ifelse(df_viz$TX_NET_NEW <0, .9, -.7),
                family = "Gill Sans MT",
                na.rm = TRUE) + 
      geom_blank(aes(y = placeholder)) + 
      facet_wrap(. ~ operatingunit, scales = "free_y") +
      scale_fill_manual(values = c(lblue, blue)) +
      scale_y_continuous(labels = comma) +
      scale_x_discrete(labels = c("FY18Q3", "", "FY19Q1", "", "FY19Q3")) + 
      labs(x = NULL, y = NULL,
           title = "USAID TRENDS IN NET_NEW",
           caption = "Note: Sorted by cumulative TX_CURR
           Source: FY19Q3i MSD") +
      theme_light() + 
      theme(panel.border = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            axis.text.x = element_text(size = 9),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(family = "Gill Sans MT"),
            plot.caption = element_text(color = "gray30"))
  
  #export
    ggsave(file.path(folderpath_output, "FY19Q3_IP_Review_NET_NEW_USAID.png"), dpi = 300,
           width = 10, height = 5.63, units = "in")
