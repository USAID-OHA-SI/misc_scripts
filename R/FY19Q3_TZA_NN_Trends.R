

# SETUP -------------------------------------------------------------------

#load dependencies
  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(nationalparkcolors)

#load data
  df_tza <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE)[1] %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")
  

#define color palette
  pal <- park_palette("ArcticGates")
  clr_pos <- pal[2]
  clr_neg <- pal[5]
  
#partner short names
  partner_map <- tibble::tribble(
                                                                    ~primepartner, ~partner_short,
                                                            "John Snow Inc (JSI)",          "JSI",
                                       "Tanzania Health Promotion Support (THPS)",         "THPS",
    "Baylor College of Medicine International Pediatric AIDS Initiative/Tanzania",       "Baylor",
                                              "Management development for Health",          "MDH",
                              "Ariel Glaser Pediatric AIDS Healthcare Initiative",        "AGPAF",
                                     "Elizabeth Glaser Pediatric AIDS Foundation",        "EGPAF",
                                                    "Deloitte Consulting Limited",     "Deloitte",
                                                            "Amref Health Africa",        "AMREF",
                                                              "Baylor University",       "Baylor"
    )
  
# ADD NET_NEW TARGETS -----------------------------------------------------
  
  df_tza_nn_targets <- df_tza %>% 
    filter(indicator == "TX_CURR",
           fiscal_year %in% c(2018, 2019)) %>% 
    select(-starts_with("qtr")) %>% 
    reshape_msd() %>% 
    select(-fy2018_targets, -fy2019cumulative) %>% 
    mutate_if(is.double, ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(targets = fy2019_targets - fy2018cumulative,
           targets = na_if(targets, 0),
           fiscal_year = 2019,
           indicator = "TX_NET_NEW") %>% 
    select(-fy2019_targets, -fy2018cumulative) %>% 
    select(-targets, everything()) %>% 
    filter(!is.na(targets))
  
  df_tza <- bind_rows(df_tza, df_tza_nn_targets)
  
  rm(df_tza_nn_targets)
  
    
# EXPLORE -----------------------------------------------------------------

  df_tza %>% 
    filter(!mech_code %in% c("16874", "17293"),
           indicator %in% c("TX_NET_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2019) %>% 
    group_by(fundingagency, mech_code, indicator) %>% 
    summarise_at(vars(qtr1, qtr2, qtr3, cumulative), sum, na.rm = TRUE) %>%
    ungroup() %>%
    arrange(indicator, fundingagency, mech_code) %>% 
    knitr::kable(format.args = list(big.mark = ","))
    
  
  df_tza %>% 
    filter(fundingagency == "HHS/CDC",
           !mech_code %in% c("16874", "17293"),
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year > 2017) %>% 
    group_by(fundingagency, indicator, fiscal_year) %>% 
    summarise_at(vars(qtr4, targets), sum, na.rm = TRUE) %>%
    ungroup() 
  
  df_tza %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           fiscal_year == 2019) %>% 
    group_by(fundingagency, indicator, standardizeddisaggregate) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>%
    ungroup()
  


  
  
# VISUALS -----------------------------------------------------------------

  #structure data
    df_tza_tx <- df_tza %>% 
      filter(fundingagency %in% c("USAID", "HHS/CDC"),
            mech_name != "AGPAHI", #closed out in FY17
            fiscal_year %in% c(2018, 2019),
            indicator %in% c("TX_NET_NEW", "TX_CURR"),
            standardizeddisaggregate == "Total Numerator") %>% 
      left_join(partner_map, by = "primepartner") %>% 
      mutate(primepartner = partner_short,
             mech_name = str_remove_all(mech_name, " -| \\(.*$")) %>% 
      group_by(fundingagency, snu1, mech_code, mech_name, primepartner, fiscal_year, indicator) %>% 
      summarise_if(is.double, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd("long") %>% 
      #filter(!str_detect(period, "cumulative|targets")) %>% 
      mutate(period = str_remove(period, "20") %>% toupper,
             fundingagency = str_remove(fundingagency, "HHS/"),
             fundingagency = factor(fundingagency, c("USAID", "CDC")),
             neg = val < 0) %>% 
      arrange(mech_code, period)

  #AGENCY TRENDS
    df_tza_tx %>% 
      filter(str_detect(period, "Q")) %>% 
      group_by(fundingagency, period, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(neg = val <0) %>% 
      ggplot(aes(period, val, fill = neg)) +
      geom_col() +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = comma, position = "right") +
      scale_fill_manual(values = c(clr_pos, clr_neg)) + 
      facet_grid(indicator ~ fundingagency, scales = "free_y",
                 switch = "y") +
      scale_x_discrete(breaks = c("FY18Q1", "", "FY18Q3", "", "FY19Q1", "", "FY19Q3")) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle ="TZA Treatment Trends") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            plot.caption = element_text(color = "gray30"),
            text = element_text(family = "Gill Sans MT"))
    
    ggsave("TZA_Tx_AgencyTrends.png",
           path = "../Downloads/",
           dpi = 300, width = 6.66, height = 5.63, units = "in")

    
  #CUMULATIVE NET NEW
    df_nn_targets <- df_tza_tx %>% 
      filter(indicator == "TX_NET_NEW",
             period == "FY19_TARGETS") %>% 
      group_by(fundingagency) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup()
    
    df_tza_tx %>% 
      filter(str_detect(period, "FY19Q"),
             indicator == "TX_NET_NEW") %>% 
      group_by(fundingagency, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      group_by(fundingagency) %>% 
      mutate(cum = cumsum(val)) %>% 
      ungroup() %>% 
      mutate(neg = cum <0) %>% 
      ggplot(aes(period, cum, fill = neg)) +
      geom_hline(data = df_nn_targets, aes(yintercept = val), 
                 color = pal[4], linetype =  "dashed", size = 1.25) +
      geom_col() +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c(clr_pos, clr_neg)) + 
      facet_wrap(. ~ fundingagency) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle ="FY19 TZA Cumulative TX_NET_NEW") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            plot.caption = element_text(color = "gray30"),
            text = element_text(family = "Gill Sans MT"))
    
    ggsave("TZA_Cum_Tx_AgencyTrends.png",
           path = "../Downloads/",
           dpi = 300, width = 6.66, height = 5.63, units = "in")
  
    rm(df_nn_targets)
    
  #partner achievement
    df_tza_tx %>% 
      filter(indicator == "TX_NET_NEW",
             str_detect(period, "19(_TARGETS|CUMULATIVE)")) %>% 
      group_by(fundingagency, primepartner, mech_code, mech_name, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(period, val) %>% 
      rename(targets = FY19_TARGETS,
             results = FY19CUMULATIVE) %>% 
      mutate(achievement = results/targets) %>% 
      arrange(fundingagency, desc(targets))
      
  #partner trends
    viz_agency <- function(df, agency){
      df %>% 
        filter(fundingagency == agency,
               str_detect(period, "Q")) %>% 
        group_by(primepartner, mech_name, indicator, period) %>% 
        summarise_at(vars(val), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(neg = val < 0) %>% 
        ggplot(aes(period, val, fill = neg)) +
        geom_hline(yintercept = 0) +
        geom_col() +
        scale_y_continuous(labels = comma, position = "right") +
        scale_fill_manual(values = c(clr_pos, clr_neg)) + 
        facet_grid(indicator ~ primepartner + mech_name, scales = "free_y",
                   switch = "y") +
        scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
        labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
             subtitle = paste0("TZA Treatment Trends | ", agency)) +
        theme_light() +
        theme(legend.position = "none",
              axis.ticks = element_blank(),
              plot.caption = element_text(color = "gray30"),
              text = element_text(family = "Gill Sans MT"))
    }
    
    viz_agency(df_tza_tx, "USAID") 
    
    ggsave("../Downloads/TZA_Tx_Trends_USAID.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)
    
    viz_agency(df_tza_tx, "CDC")  
    
    ggsave("../Downloads/TZA_Tx_Trends_CDC.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)
    
    
  #IM CUMULATIVE NET NEW
    
    viz_cum_nn <- function(df, agency){
      
      df_viz <- df %>% 
        filter(fundingagency == agency,
               indicator == "TX_NET_NEW")
        
      df_nn_targets <- df_viz %>% 
        filter(period == "FY19_TARGETS") %>% 
        group_by(mech_name, primepartner) %>% 
        summarise_at(vars(val), sum, na.rm = TRUE) %>% 
        ungroup()
      
      df_viz <- df_viz %>% 
        filter(str_detect(period, "FY19Q")) %>% 
        group_by(mech_name, primepartner, period) %>% 
        summarise_at(vars(val), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        arrange(mech_name, period) %>% 
        group_by(mech_name, primepartner) %>% 
        mutate(cum = cumsum(val)) %>% 
        ungroup() %>% 
        mutate(neg = cum < 0) 
      
      df_viz %>% 
        ggplot(aes(period, cum, fill = neg)) +
        geom_hline(data = df_nn_targets, aes(yintercept = val), 
                   color = pal[4], linetype =  "dashed", size = 1.25) +
        geom_col() +
        geom_hline(yintercept = 0) +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = c(clr_pos, clr_neg)) + 
        facet_wrap(primepartner + mech_name ~ .) +
        labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
             subtitle = paste0("FY19 TZA Cumulative TX_NET_NEW | ", agency)) +
        theme_light() +
        theme(legend.position = "none",
              axis.ticks = element_blank(),
              plot.caption = element_text(color = "gray30"),
              text = element_text(family = "Gill Sans MT"))
    }
    
    viz_cum_nn(df_tza_tx, "USAID") 
    
    ggsave("../Downloads/TZA_Tx_Cum_Trends_USAID_IM.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)

    viz_cum_nn(df_tza_tx, "CDC") 
    
    ggsave("../Downloads/TZA_Tx_Cum_Trends_CDC_IM.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)
    
    
  #SNUs
    df_tza_tx %>% 
      filter(fundingagency == "USAID",
             str_detect(period, "Q")) %>% 
      spread(indicator, val) %>% 
      group_by(fundingagency, snu1, primepartner, period) %>% 
      summarise_at(vars(TX_NET_NEW, TX_CURR), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(neg = TX_NET_NEW < 0,
             snu1 = fct_reorder(snu1, desc(TX_CURR))) %>% 
      ggplot(aes(period, TX_NET_NEW, fill = primepartner)) +
      geom_hline(yintercept = 0) +
      geom_col() +
      scale_y_continuous(labels = comma, position = "right") +
      scale_fill_manual(values = c(pal)) + 
      facet_wrap(snu1 ~ .) +
      scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle = paste0("TZA Treatment Trends")) +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            plot.caption = element_text(color = "gray30"),
            text = element_text(family = "Gill Sans MT"))
    
    
    viz_cum_nn_snu <- function(df, agency){
      
      df_viz <- df %>% 
        filter(fundingagency == agency,
               indicator == "TX_NET_NEW")
      
      df_nn_targets <- df_viz %>% 
        filter(period == "FY19_TARGETS") %>% 
        group_by(snu1) %>% 
        summarise_at(vars(val), sum, na.rm = TRUE) %>% 
        ungroup()
      
      df_viz <- df_viz %>% 
        filter(str_detect(period, "FY19Q")) %>% 
        group_by(snu1, period) %>% 
        summarise_at(vars(val), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        arrange(snu1, period) %>% 
        group_by(snu1) %>% 
        mutate(cum = cumsum(val)) %>% 
        ungroup() %>% 
        mutate(neg = cum < 0) 
      
      df_viz %>% 
        ggplot(aes(period, cum, fill = neg)) +
        geom_hline(data = df_nn_targets, aes(yintercept = val), 
                   color = pal[4], linetype =  "dashed", size = 1.25) +
        geom_col() +
        geom_hline(yintercept = 0) +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = c(clr_pos, clr_neg)) + 
        facet_wrap(snu1 ~ .) +
        labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
             subtitle = paste0("FY19 TZA Cumulative TX_NET_NEW | ", agency)) +
        theme_light() +
        theme(legend.position = "none",
              axis.ticks = element_blank(),
              plot.caption = element_text(color = "gray30"),
              text = element_text(family = "Gill Sans MT"))
    }
   
    
    viz_cum_nn_snu(df_tza_tx, "USAID")
    
    ggsave("../Downloads/TZA_Tx_Cum_Trends_USAID_SNU.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)
    
    viz_cum_nn_snu(df_tza_tx, "CDC")
    
    ggsave("../Downloads/TZA_Tx_Cum_Trends_CDC_SNU.png", dpi = 300,
           units = "in", width = 6.66, height = 5.63)
    
    
    df_tza_tx %>% 
      filter(str_detect(period, "19(_TARGETS|CUMULATIVE)"),
             fundingagency == "USAID",
             indicator == "TX_NET_NEW") %>% 
      group_by(fundingagency, indicator, snu1, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(period, val) %>% 
      mutate(achv = FY19CUMULATIVE/FY19_TARGETS) %>% 
      arrange(achv)
      
  
    
    
#df for tx and nn
  df_tza_nn <- df_tza %>% 
    filter(fundingagency %in% c("USAID", "HHS/CDC"),
           mech_name != "AGPAHI", #closed out in FY17
           indicator %in% c("TX_NET_NEW", "TX_CURR"),
           fiscal_year %in% c(2018, 2019),
           standardizeddisaggregate == "Total Numerator") %>% 
    left_join(partner_map, by = "primepartner") %>% 
    mutate(primepartner = partner_short,
           mech_name = str_remove_all(mech_name, " -| \\(.*$")) %>% 
    group_by(fundingagency, mech_code, mech_name, primepartner, fiscal_year, indicator) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd("long") %>% 
    filter(!str_detect(period, "cumulative|targets")) %>% 
    mutate(period = str_remove(period, "20") %>% toupper,
           fundingagency = str_remove(fundingagency, "HHS/"),
           neg = val < 0) %>% 
    arrange(mech_code, period)


  mech_order <- df_tza_nn %>% 
    filter(indicator == "TX_CURR") %>% 
    mutate(pd2 = str_remove(period, "FY") %>% str_replace("Q", "\\.") %>% as.numeric()) %>% 
    group_by(mech_code) %>% 
    filter(pd2 == max(pd2)) %>% 
    ungroup() %>% 
    arrange(desc(fundingagency), desc(val)) %>% 
    pull(mech_name)
    
  df_tza_nn <- df_tza_nn %>% 
    mutate(mech_name = factor(mech_name, mech_order))
  
  
  viz_agency <- function(df, agency){
    
    df %>% 
      filter(fundingagency == agency) %>% 
      group_by(primepartner, mech_name, period, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(neg = val < 0) %>% 
      ggplot(aes(period, val, fill = neg)) +
      geom_hline(yintercept = 0) +
      geom_col() +
      scale_y_continuous(labels = comma, position = "right") +
      scale_fill_manual(values = c(clr_pos, clr_neg)) + 
      facet_grid(indicator ~ primepartner + mech_name, scales = "free_y",
                 switch = "y") +
      scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle = paste0("TZA Treatment Trends | ", agency)) +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            plot.caption = element_text(color = "gray30"),
            text = element_text(family = "Gill Sans MT"))
  }

  viz_agency(df_tza_tx, "USAID") 
  
  ggsave("TZA_Cum_Tx_USAID_IM_Trends.png",
         path = "../Downloads/",
         dpi = 300, width = 6.66, height = 5.63, units = "in")
  
  viz_agency(df_tza_tx, "CDC")  
  
  ggsave("TZA_Cum_Tx_CDC_IM_Trends.png",
         path = "../Downloads/",
         dpi = 300, width = 6.66, height = 5.63, units = "in")

  df_tza_nn %>% 
    filter(fundingagency == "CDC") %>% 
    count(fundingagency, period, indicator, wt = val) %>% 
    spread(indicator, n)
  
  df_tza_nn %>% 
    filter(fundingagency == "CDC",
           primepartner %in% c("MDH", "THPS"),
           period %in% c("FY18Q4", "FY19Q1")) %>% 
    count(primepartner, mech_name, period, indicator, wt = val) %>% 
    spread(indicator, n)
  
  
  df_tza_nn_cum <- df_tza_nn %>% 
    select(-neg) %>% 
    spread(indicator, val) %>% 
    arrange(mech_code, period) %>% 
    mutate(fy = str_sub(period, 3,4)) %>% 
    group_by(mech_code, fy) %>% 
    mutate(TX_NN_cum = cumsum(TX_NET_NEW),
           neg = TX_NN_cum < 0) %>% 
    ungroup()
  
  
  df_tza_nn_cum %>% 
    ggplot(aes(period, TX_NN_cum, fill = neg)) +
    geom_hline(yintercept = 0) +
    geom_col() +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values = c(clr_pos, clr_neg)) + 
    facet_wrap(. ~ primepartner + mech_name, nrow = 2) +
    scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
    #facet_wrap(. ~ mech_name, ncol = 5, scales = "free_y") +
    labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
         subtitle = "TZA Cumulative TX_NET_NEW Trends") +
    theme_light() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          text = element_text(family = "Gill Sans MT"),
          plot.caption = element_text(color = "gray30"))
  
  
  
  df_tza %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in% c(2018, 2019)) %>%
    left_join(partner_map, by = "primepartner") %>% 
    mutate(primepartner = partner_short) %>% 
    filter(fundingagency == "HHS/CDC",
           primepartner %in% c("MDH", "THPS")
           ) %>% 
    reshape_msd() %>% 
    group_by(fundingagency, primepartner, mech_name, snu1) %>% 
    summarise_at(vars(fy2018q4, fy2019q1), sum, na.rm = TRUE) %>%
    ungroup()
  
  df_tza_nn %>% 
    filter(fundingagency == "CDC",
           primepartner %in% c("MDH", "THPS"),
           period %in% c("FY18Q4", "FY19Q1")) %>% 
    count(snu1, primepartner, mech_name, period, indicator, wt = val) %>% 
    spread(indicator, n)
  
  
  df_tza_snu <- df_tza %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in% c(2018, 2019)) %>%
    left_join(partner_map, by = "primepartner") %>% 
    mutate(primepartner = partner_short,
           mech_name = str_remove_all(mech_name, " -| \\(.*$")) %>% 
    filter(fundingagency == "HHS/CDC",
           #primepartner %in% c("MDH", "THPS")
           ) %>% 
    reshape_msd("long") %>% 
    group_by(fundingagency, primepartner, mech_code, mech_name, snu1, period) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>% 
    filter(str_detect(period, "q")) %>% 
    mutate(period = str_remove(period, "20") %>% toupper,
          fundingagency = str_remove(fundingagency, "HHS/"),
          neg = val < 0) %>% 
    arrange(mech_code, period)
  
  df_tza_snu %>% 
    tidyr::unite("partner_mech", c("primepartner", "mech_name"), sep = ": ") %>% 
    ggplot(aes(period, val, fill = partner_mech)) +
    geom_col() +
    facet_wrap(. ~ snu1) +
    scale_fill_manual(values = c(pal[5], pal[2], pal[4], pal[6], pal[3])) + 
    scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
         subtitle = "TZA Treatment Current Regional Trends | CDC") +
    theme_light() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(family = "Gill Sans MT"))
  
  ggsave("../Downloads/TZA_Tx_Trends_CDC_SNU.png", dpi = 300,
         units = "in", width = 9.5, height = 4)
  
  
  
  df_tza_snu_nn_cum <- df_tza %>% 
    filter(indicator %in% c("TX_CURR","TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           snu1 != "_Military Tanzania",
           fiscal_year %in% c(2018, 2019)) %>%
    reshape_msd("long") %>%
    group_by(fundingagency, snu1, period, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>% 
    filter(str_detect(period, "q")) %>% 
    mutate(fy = str_sub(period, 5, 6)) %>% 
    spread(indicator, val) %>% 
    group_by(fundingagency, snu1, fy) %>% 
    mutate(nn_cum = cumsum(TX_NET_NEW)) %>% 
    ungroup() %>% 
    mutate(period = str_remove(period, "20") %>% toupper,
           fundingagency = str_remove(fundingagency, "HHS/"),
           neg = nn_cum < 0) %>% 
    arrange(snu1, period)
  
  
  viz_agency_snu <- function(df, agency, ind){
    
    filter()
    #ind <- str_replace(indicator, "nn_cum", "TX_NET_NEW Cumulative"))
    
    df %>% 
      filter(fundingagency == agency) %>% 
      ggplot(aes(period, !!ind, fill = neg)) +
      geom_col() +
      facet_wrap(. ~ fct_reorder(snu1, desc(TX_CURR))) +
      scale_fill_manual(values = c(clr_pos, clr_neg)) + 
      scale_x_discrete(breaks = c("FY18Q1", "", "", "", "FY19Q1", "", "")) +
      #scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle = paste("TZA", ind, "by Region |", agency)) +
      theme_light() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            text = element_text(family = "Gill Sans MT"))
  }
  
  viz_agency_snu(df_tza_snu_nn_cum, "USAID", "TX_CURR")
    
    
    df %>% 
    filter(operatingunit == "Tanzania",
           fundingagency %in% c("USAID", "HHS/CDC"),
           indicator %in% c("TX_NET_NEW", "TX_CURR"),
           fiscal_year %in% c(2018, 2019),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fundingagency, fiscal_year, indicator) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd("long") %>% 
    filter(!str_detect(period, "cumulative|targets")) %>% 
    mutate(period = str_remove(period, "20") %>% toupper,
           fundingagency = str_remove(fundingagency, "HHS/"),
           neg = val < 0) %>% 
    arrange(fundingagency, period)
  
  df_tza_agency %>% 
      ggplot(aes(period, val, fill = neg)) +
      geom_hline(yintercept = 0) +
      geom_col() +
      scale_y_continuous(labels = comma, position = "right") +
      scale_fill_manual(values = c(clr_pos, clr_neg)) + 
      facet_grid(indicator ~ fundingagency, scales = "free_y",
                 switch = "y") +
      scale_x_discrete(breaks = c("FY18Q1", "", "FY18Q3", "", "FY19Q1", "", "FY19Q3")) +
      labs(x = NULL, y = NULL, caption = "Source: FY19Q3c MSD",
           subtitle = "TZA Treatment Trends") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(family = "Gill Sans MT"))
  
  ggsave("../Downloads/TZA_Tx_Trends_both.png", dpi = 300,
         units = "in", width = 9.5, height = 4)
  

 
 

 
  