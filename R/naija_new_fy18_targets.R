

library(tidyverse)
library(ICPIutilities)
library(readxl)

df <- read_msd("C:\\Users\\GHFP\\Documents\\data\\8.15_release\\PREP_NEW\\PREP_NEW_genie_9.5.18.txt") %>% 
  mutate(fy2018q2 = ifelse(mechanismid=="18456" & disaggregate=="Total Numerator", NA, fy2018q2))

df %>% filter(mechanismid=="18456") %>% View()


data <- "C:\\Users\\GHFP\\Documents\\data\\8.15_release"
indc <- c("PrEP_NEW")

df <- read_rds(file.path(data, "MER_Structured_Dataset_OU_FY17-18_20180815_v1_1.rds")) %>% 
  filter(indicator %in% indc) %>% 
  write.csv(file.path(data, "prep_new_q3_unclean.csv"))

############################################################################################################
## Naija scracth, targets

cc <- c(14505, 18441, 14664)
indc <- c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")
adjustment <- "C:\\Users\\GHFP\\Documents\\ICPI\\Nigeria\\targets"

## bring in adjustment factor for fy18 targets
map <- readxl::read_xlsx(file.path(adjustment, "USAID FY18 Adjusted Targets__updated07302018_jd.xlsx"), sheet = "map")

## adjust msd
tarj <- ng_psnu %>% 
  filter((mechanismid %in% cc) & (indicator %in% indc) & 
            (standardizeddisaggregate == "Total Numerator") & 
              (!is.na(fy2018_targets))) %>%      ## reduce to just the things we need
  select(mechanismid, indicator, fy2018_targets, psnu, psnuuid) %>% 
  mutate(mechanismid = as.double(mechanismid))

new_tarj <- right_join(tarj, map) %>% 
  mutate(new_fy2018_targets = fy2018_targets*value) %>% 
  write_csv(file.path(data, "ng_new_fy18_targets.csv"))
  
#############################################################################################################
##  Nigeria target import part II
##  Import target file

## sidhas
sidhass <- readxl::read_xlsx(file.path(adjustment, "USAID FY18 Adjusted Targets__updated07302018_jd.xlsx"), sheet = "SIDHAS  Targets ",
        skip = 2) %>% 
        select(-c("X__1", "X__2", `FY18 adjusted targets HTS TST`, `FY18 Adusted HTS TST POS`, 
                  `FY18 Adusted HTS TST POS`, `FY18 TX NEW Adjusted Target` )) %>% 
                  rename(psnu = `LGA Name`,TX_NEW = `FY18 TX NEW Adjusted Target (excluding KP)`,
                         HTS_TST_POS = `FY18 Adusted HTS TST POS (Excluding KP)`,
                         HTS_TST = `FY18 adjusted targets HTS TST (excluding KP)`) %>% 
                  mutate(mechanismid = 14505)
  
## HAI
hai <- readxl::read_xlsx(file.path(adjustment, "USAID FY18 Adjusted Targets__updated07302018_jd.xlsx"), sheet = "HAI Targets", skip = 2) %>% 
                         select(-c("X__1", "X__2", `FY18 Adjusted KP HTS TST__1`, `FY18 Adjusted KP_HTS_TST_POS`, 
                                   `FY18 Adjusted KP_TX_NEW  Target__1`)) %>%
                        rename(psnu = `LGA`, TX_NEW = `FY18 Adjusted KP_TX_NEW  Target`, HTS_TST_POS = `FY18 Adjusted HTS_TST_POS (Key Pop)`,
                               HTS_TST = `FY18 Adjusted KP HTS TST`) %>% 
                        mutate(mechanismid = 14664)

  
catss <- readxl::read_xlsx(file.path(adjustment, "USAID FY18 Adjusted Targets__updated07302018_jd.xlsx"), sheet = "CATSS Targets", skip = 2) %>%
                        select(-c("X__1", "X__2", `FY18 adjusted targets HTS TST`, `FY18 Adusted HTS TST POS`, 
                                  `FY18 TX NEW Adjusted Target`)) %>% 
                        rename(psnu = `LGA Name`,TX_NEW = `FY18 TX NEW Adjusted Target (excluding KP)`,
                              HTS_TST_POS = `FY18 Adusted HTS TST POS (Excluding KP)`,
                              HTS_TST = `FY18 adjusted targets HTS TST (excluding KP)`) %>% 
                        mutate(mechanismid = 18441)

fy18_tarj <- bind_rows(catss, hai, sidhass) %>% 
  gather(indicator, fy2018_targets, -psnu, -mechanismid) %>% 
  filter(fy2018_targets != 0) %>% 
  mutate(fy2018_targets = round(fy2018_targets)) %>%
  write_csv(file.path(data, "ng_new_targets.csv"))



## Merge with MSD
# disaggragate == "Total Numerator" & indicatortype == "DSD"

## add on as 'ajdusted targets'

  









  
