## ECT II analysis
## 9/20/18
## Josh Davis

library(tidyverse)
library(ICPIutilities)
library(writexl)
library(stringr)
library(glue)


  ## ECT 2 specific dataset aggragted at SNU1
  ## define objects:  data    = folder path where your msd is
  ##                  ect2    = ect OUs only
  ##                  cascade = indicator set
  ##                  FY      = Obvious
  
  data <- "C:\\Users\\GHFP\\Documents\\data\\9_22_release"
  ect2 <- c("Botswana", "Kenya", "Lesotho", "Malawi","Namibia", "Rwanda", "Eswatini", "Uganda", "Zambia", "Zimbabwe")
  cascade <- c("TX_CURR", "TX_NEW", "HTS_TST", "HTS_TST_POS", "TX_NET_NEW", "HTS_TST_NEG")
  fy17 <- c("fy2017_targets", "fy2017q1", "fy2017q2", "fy2017q3", "fy2017q4", "fy2017apr")
  fy18 <- c("fy2018_targets", "fy2018q1", "fy2018q2", "fy2018q3")
  
  ## read in PSNU by IM dataset

  psnu_im <- read_rds(file.path(data,"MER_Structured_Dataset_PSNU_IM_FY17-18_20180921_v2_1.Rds"))
  
  ## subset
  # first for ECT2 OUs
  ## then group and summarise to SNU1 level
  
  
  ect2_snu1 <- psnu_im %>%
    filter(operatingunit %in% ect2) %>%
    filter(indicator %in% cascade) %>%
    select(-fy17) %>% 
    filter_at(.vars = vars(fy18), .vars_predicate = any_vars(!is.na(.))) %>% 
    select(-c(psnu, psnuuid)) %>% 
    group_by_if(is.character) %>% 
    summarise_if((is.numeric),  ~sum(., na.rm = TRUE)) %>%
    add_cumulative()
  
  
  ect2_snu1 <- ect2_snu1 %>% 
    write_csv(file.path(data, "ect2_9_22_msd_cascade_snu1.csv"))
    
  
  ect2_snu1 <- ect2_snu1 %>% 
    write_csv(file.path(data, "ect2_9_22_msd_cascade_snu1.csv"))
  
    
  ##############################################################
  ##  Make ECT PPRs
  
  ect <- read_rds(file.path(data, "MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1.rds")) %>% 
    filter(operatingunit %in% ect2)
  
                 
 data <- "C:/Users/GHFP/Documents/data/11_15_release"
  
 #install
 install.packages("devtools")
 devtools::install_github("achafetz/PartnerProgress")
 #load package
 library("genPPR")
  
  
   
   
  
  

                 