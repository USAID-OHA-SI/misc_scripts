##  PROJECT:  HFR Analysis
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  analyze VL and MMD 
##  DATE:     2019-12-19
##  UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(Wavelength)
library(extrafont)
library(RColorBrewer)
library(scales)
library(patchwork)

# GLOBAL VARIABLES --------------------------------------------------------

  #focal OU
    ou_sel <- "Zambia"

  #HFR file path
    path_hfr <- "~/GitHub/Wavelength/out/joint/HFR_2020.01_GLOBAL_tableau_extract_20191205.csv"
  #MSD site file path
    path_msd <- "../Downloads/PEPFAR-Data-Genie-SiteByIMs-2019-12-19_ZAM.zip"
  
  #colors 
    red <- brewer.pal(3, "RdBu")[1]
    blue <- brewer.pal(3, "RdBu")[3]

# IMPORT ------------------------------------------------------------------
  
  #import HFR data
    df_hfr <- hfr_read(path_hfr)
  
  #import MER site data
    df_msd <- read_msd(path_msd, save_rds = FALSE, remove_txt = FALSE)

  
# MUNGE MER ---------------------------------------------------------------
    
  #filter to relevant indicators & designate denoms
    df_msd <- df_msd %>% 
      filter(operatingunit == ou_sel,
             indicator %in% c("TX_CURR","TX_PVLS"),
             fundingagency == "USAID",
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
             fiscal_year == 2019) %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) 
    
  #reshape long & filter for by pd and indicator
    df_msd_lng <- df_msd %>% 
      reshape_msd("long") %>% 
      filter((indicator == "TX_CURR" & period == "fy2019q2") |
             (indicator != "TX_CURR" & period == "fy2019q4"))
    
    df_msd_lng <- df_msd_lng %>% 
      filter(val > 0) %>% 
      group_by(orgunituid, sitename, snu1, psnu, 
               mech_code, mech_name, primepartner,
               indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, val) 
    
# MUNGE HFR ---------------------------------------------------------------
    
  #filter to relevant indicators
    df_hfr_zam <- df_hfr %>% 
      filter(operatingunit == ou_sel,
             indicator %in% c("TX_MMD"),
             fy == 2019,
             hfr_pd == 13)
    
  #fix missing uids
    df_hfr_zam <- df_hfr_zam %>% 
      mutate(orgunituid = case_when(orgunit == "Muunga" ~ "RPOR8FJjOL6",
                                     orgunit == "Mary Berg Kalumbila" ~ "ypVrrUERxqy",
                                     orgunit == "Shiwa Ng'andu Urban" ~ "oGzIjvvopHN",
                                     TRUE ~ orgunituid))

  #adjust months
    df_hfr_zam <- df_hfr_zam %>% 
      mutate(otherdisaggregate = case_when(otherdisaggregate %in% c("1 month", "2 months") ~ "u3m",
                                           otherdisaggregate %in% c("3 months", "4-5 months") ~ "35m",
                                           otherdisaggregate == "6 months or more" ~ "6pm")) 
  #aggregate
    df_hfr_zam <- df_hfr_zam %>% 
      group_by(orgunituid, mech_code, 
               indicator, otherdisaggregate) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup()
    
  #reshape
    df_hfr_zam <- df_hfr_zam %>% 
      unite(ind, c(indicator, otherdisaggregate), sep = "_") %>% 
      spread(ind, val)
    
    

# VISUALIZE ---------------------------------------------------------------

  #join HFR and MER, filtering out blank rows
    df_combo <- df_msd_lng %>% 
        right_join(df_hfr_zam) %>% 
        filter_if(is.numeric, any_vars(.!=0))
    
  #create coverage, suppression, and MMD share
    df_viz <- df_combo %>% 
        mutate(coverage = TX_PVLS_D / TX_CURR,
               suppression = TX_PVLS / TX_PVLS_D,
               mmd_share = 1 - (TX_MMD_u3m/(TX_MMD_u3m + TX_MMD_35m + TX_MMD_6pm))) 
      
  df_viz %>% 
    ggplot(aes(mmd_share, suppression, size = TX_CURR)) +
    geom_point(alpha = .6)
    
  
  df_viz2 <- df_viz %>% 
    select(-mmd_share) %>% 
    gather(mmd, val, starts_with("TX_MMD")) %>% 
    mutate(mmd = case_when(mmd == "TX_MMD_u3m" ~ "<3 months",
                           mmd == "TX_MMD_35m" ~ "3-5 months",
                           mmd == "TX_MMD_6pm" ~ "6+ months"),
           mmd = factor(mmd, c("6+ months", "3-5 months", "<3 months")),
           tx_grp = ntile(TX_CURR, 3),
           tx_grp = case_when(tx_grp == 1 ~ "Small",
                              tx_grp == 2 ~ "Medium",
                              tx_grp == 3 ~ "Large"),
           tx_grp = factor(tx_grp, c("Small", "Medium", "Large"))) 
    
  df_combo2 <- df_combo2 %>% 
    filter(!is.na(suppression),
           !is.na(tx_grp)) %>% 
    group_by(primepartner, tx_grp, mmd) %>% 
    mutate(med_sup = median(suppression, na.rm = TRUE)) %>% 
    ungroup()
    
  df_combo2 %>% 
    ggplot(aes(mmd, suppression)) +
    geom_vline(xintercept = 0) +
    geom_point() +
    coord_flip()
    # geom_jitter(position = position_jitter(width = 0.3, height = 0.1),
    #             alpha = .5,
    #             size = 3,
    #             na.rm = TRUE) +
    geom_errorbar(aes(ymin = med_sup, ymax = med_sup),
                  color = "gray50", size = 1, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    facet_grid(primepartner ~ tx_grp) +
    labs(x = "VL suppression", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT"))
    
    
    df_viz3 <- df_combo %>% 
      group_by(psnu) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(suppression = TX_PVLS / TX_PVLS_D,
             mmd_share_6pm = TX_MMD_6pm/(TX_MMD_u3m + TX_MMD_35m + TX_MMD_6pm),
             psnu = str_remove(psnu, " District"),
             psnu = fct_reorder(psnu, mmd_share_6pm)) 
    
    cor.test(df_viz3$mmd_share_6pm,df_viz3$suppression)
    
    df_viz3 <- df_viz3 %>%
      filter(!is.na(psnu)) %>% 
      select(-TX_CURR:-TX_MMD_u3m) %>% 
      gather(ind, val, suppression, mmd_share_6pm) %>% 
      mutate(ind = case_when(ind == "mmd_share_6pm" ~ "Share on 6+mo MMD",
                             ind == "suppression" ~ "VL Suppressed",
                             ind == "coverage" ~ "VL Coverage"),
             ind = factor(ind, c("Share on 6+mo MMD", "VL Suppressed")))
      
    
    
    df_viz3 %>% 
      ggplot(aes(val, psnu)) +
      geom_point(na.rm = TRUE) +
      geom_segment(aes(x=0, xend=val, y=psnu, yend=psnu), na.rm = TRUE) +
      facet_wrap(. ~ ind) +
      labs(x = NULL, y = NULL) +
      theme_minimal()
    
   
    df_viz4 <- df_combo %>% 
      group_by(psnu) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
      ungroup() %>%
      filter(TX_PVLS_D > 0) %>% 
      mutate(suppression = TX_PVLS / TX_PVLS_D,
             mmd_share_u3m = TX_MMD_u3m/(TX_MMD_u3m + TX_MMD_35m + TX_MMD_6pm),
             mmd_share_35m = TX_MMD_35m/(TX_MMD_u3m + TX_MMD_35m + TX_MMD_6pm),
             mmd_share_6pm = TX_MMD_6pm/(TX_MMD_u3m + TX_MMD_35m + TX_MMD_6pm),
             psnu = str_remove(psnu, " District"),
             psnu = fct_reorder(psnu, mmd_share_6pm)) %>% 
      filter(!is.na(psnu))
    

    df_viz4_1 <- df_viz4_1 <- df_viz4 %>%
      select(-TX_CURR:-suppression) %>% 
      gather(ind, val, mmd_share_u3m, mmd_share_35m, mmd_share_6pm) %>% 
      mutate(labs = case_when(psnu == "Mwense" & ind == "mmd_share_35m" ~ "3-5 mo",
                              psnu == "Mwense" & ind == "mmd_share_6pm" ~ "6+ mo",
                              psnu == "Mpika" & ind == "mmd_share_u3m" ~ "<3 mo"))
    
  
   v1 <- df_viz4_1 %>% 
      ggplot(aes(val, psnu, color = ind)) +
      geom_vline(xintercept = c(0, 1), color = "gray30") +
      #geom_segment(aes(x=0, xend=1, y=psnu, yend=psnu),color = "gray30", na.rm = TRUE) +
      geom_point(size = 2.5, na.rm = TRUE) +
      geom_text(aes(label = labs, vjust = -1.2, 
                    hjust = ifelse(psnu == "Mpika", -.2, .5)),
                size = 3,
                family = "Gill Sans MT",
                na.rm = TRUE) +
      scale_x_continuous(labels = percent_format(1)) +
      scale_color_manual(values = c("gray60", red,"gray70")) +
      labs(x = NULL, y = NULL,
           title = "WEAK CORRELATION | ZAMBIA",
           subtitle = "MMD Share (2019.13)",
           caption = "") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(family = "Gill Sans MT"),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 10))
    
    v2 <- df_viz4 %>%
      ggplot(aes(suppression, psnu)) +
      geom_vline(xintercept = c(0, 1), color = "gray30") +
      geom_segment(aes(x=0, xend=suppression, y=psnu, yend=psnu), color = blue, na.rm = TRUE) +
      geom_point(size = 2.5, color = blue, na.rm = TRUE) +
      scale_x_continuous(labels = percent_format(1)) +
      scale_y_discrete(position = "right") +
      labs(x = NULL, y = NULL,
           title = "",
           subtitle = "VL Suppression (Q4)",
           caption = "Source: DATIM Genie 2019-12-19 + HFR 2020.01") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(family = "Gill Sans MT", size = 11),
            axis.text.y = element_text(size = 7, color = "white"),
            axis.text.x = element_text(size = 10),
            plot.caption = element_text(color = "gray30", size = 10))
    
    v_c <- v1 + v2

    ggsave("../Downloads/vls.png", v_c, dpi = 300, width = 6.85, height = 5.12)
        