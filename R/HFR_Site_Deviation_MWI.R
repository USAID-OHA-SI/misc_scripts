##  PROJECT:  HFR Analysis
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  analyze site deviation from PSNU median 
##  DATE:     2019-12-11
##  UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(Wavelength)
  library(extrafont)
  library(RColorBrewer)


# GLOBAL VARIABLES --------------------------------------------------------

  #focal OU
    ou_sel <- "Malawi"
  #indicators of focus
    ind_sel <- c("HTS_TST", "TX_NEW")
  #what share of sites constitues as large? Top x % of sites
    share <- .6
  
  #HFR file path
    path_hfr <- "~/GitHub/Wavelength/out/joint/HFR_2020.01_GLOBAL_tableau_extract_20191205.csv"
  #MSD site file path
    path_msd <- "../Downloads/MER_Structured_Datasets_SITE_IM_FY17-20_20191115_v1_1_Malawi.zip"

  #colors 
    red <- brewer.pal(3, "RdBu")[1]
    blue <- brewer.pal(3, "RdBu")[3]
    
# IMPORT ------------------------------------------------------------------

  #import HFR data
    df_hfr <- hfr_read(path_hfr)
  
  #import MER site data
    df_msd <- read_msd(path_msd, save_rds = FALSE)
    

# FUNCTION: IDENTIFY LARGE SITES ------------------------------------------

  #identify the largest sites
    identify_lrg <- function(df, ind, share){
      lrg_sites <- df %>%
        filter(fiscal_year == 2020,
               indicator == {{ind}},
               targets > 0) %>% 
        group_by(mech_code, orgunituid, indicator) %>% 
        summarise(mer_target = sum(targets, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(cume_dist(mer_target) >= (1-share)) %>% 
        arrange(indicator, desc(mer_target))
      
      df_lrg <- inner_join(df, lrg_sites, by = c("orgunituid", "mech_code", "indicator"))
      
      return(df_lrg)
    }
    
# MUNGE MER ---------------------------------------------------------------

  #filter to relevant indicators
    df_msd <- df_msd %>% 
      filter(operatingunit == ou_sel,
             indicator %in% ind_sel,
             fundingagency == "USAID",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year > 2018)
    
  #identify large sites
    df_lrg_tx <- identify_lrg(df_msd, "TX_NEW", .6)
    
  #median results
    df_med4w_tx <- df_lrg_tx %>% 
      filter(fiscal_year == 2019) %>% 
      mutate(tx_4w = round((qtr4/13)*4, 0)) %>% 
      group_by(indicator, psnu) %>% 
      summarise(psnu_med = median(tx_4w, na.rm = TRUE)) %>% 
      ungroup()
      

# MUNGE HFR ---------------------------------------------------------------

  #filter to relevant indicators
    df_hfr <- df_hfr %>% 
      filter(operatingunit == ou_sel,
             indicator %in% ind_sel)
    
  #filter to large sites
    lrg_sites <- df_lrg_tx %>% 
      distinct(mech_code, orgunituid, indicator)
    
    df_hfr_lrg <- inner_join(df_hfr, lrg_sites)
      rm(lrg_sites)

    df_hfr_lrg <- df_hfr_lrg %>% 
      filter(val > 0) 
    
  #aggregate
    df_hfr_lrg <- df_hfr_lrg %>% 
      group_by(primepartner, mech_code, 
               psnu, orgunit, orgunituid, 
               hfr_pd, fy,
               indicator) %>% 
      summarise(hfr_val = sum(val, na.rm = TRUE)) %>% 
      ungroup()
    
  #generate flag
    df_hfr_lrg <- df_hfr_lrg %>% 
      left_join(df_med4w_tx, by = c("psnu", "indicator")) %>% 
      mutate(under_med = hfr_val < psnu_med) #%>% 
      # select(-psnu_med)
    
  #combine with median
    df_viz <- df_hfr_lrg %>% 
      #bind_rows(df_med4w_tx) %>% 
      mutate(psnu = str_remove(psnu, " District"),
             psnu = fct_reorder(psnu, psnu_med, na.rm = TRUE),
             hfr_pd = fy + (hfr_pd/100)) 

    

# VISUALIZE ---------------------------------------------------------------

    df_viz %>% 
      ggplot(aes(psnu, hfr_val, color = under_med)) +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_jitter(position = position_jitter(width = 0.2, height = 0.1),
                  alpha = .5, 
                  size = 3, na.rm = TRUE) +
      geom_errorbar(aes(ymin = psnu_med, ymax = psnu_med),
                    color = "gray50", size = 1, na.rm = TRUE) +
      labs(x = NULL, y = "weekly new on treatment",
           title = "IMPROVING TRENDS IN TX_NEW | MALAWI",
           subtitle = "HFR reporting vs median FY19Q4 4-week district median",
           caption = "Source: FY19Q4i MSD + HFR 2020.01") +
      scale_color_manual(values = c(blue, red)) +
      coord_flip() +
      facet_wrap(.~ hfr_pd) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(family = "Gill Sans MT", size = 12),
            plot.caption = element_text(color = "gray30", size = 11),
            strip.text = element_text(face = "bold", size = 13))

    ggsave("../Downloads/MWI_Site_Deviation_TX.png", dpi = 300,
           width = 6.85, height = 5.12)    
    
    