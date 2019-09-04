## PROJECT: FY19Q3 OHA REVIEW
## PURPOSE: create a dataset for use with Tableau
## AUTHOR:  A.CHAFETZ | USAID
## DATE:    2019-08-22
## UPDATED: 2019-08-29

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(readxl)
library(lubridate)

# FILE PATHS --------------------------------------------------------------

#MSD can be found on PEPFAR Pano and the rest of the data is available on Google Drive
#https://drive.google.com/open?id=1ULN7Vz3pp82AUxdLZ3WPXhaXbmww_GNX
path_msd <- "~/ICPI/Data/MER_Structured_Datasets_PSNU_IM_FY17-19_20190815_v1_1.rds"
path_outlays <- "~/OHA Quarterly Review/FY19 Q3 Outlay Tool_Mission Review Version.xlsx"
path_hfr <- "~/GitHub/Wavelength/out/joint/HFR_GLOBAL_output_20190822.2334.txt"
path_output <- "~/OHA Quarterly Review/"

# MER PROCESSING ----------------------------------------------------------

#import data
df_msd <- read_rds(path_msd)

#indicators to keep
ind_sel <- c("AGYW_PREV", "HTS_TST", "HTS_TST_POS", "OVC_HIVSTAT", 
             "OVC_SERV_UNDER_18","TB_PREV_D", "TB_PREV",
             "TX_CURR", "TX_NEW", "TX_TB", "TX_TB_D", "TX_TB_D_NEG_D", 
             "TX_NET_NEW", "TX_PVLS", "TX_PVLS_D", "VMMC_CIRC")
snapshot <- c("TX_CURR", "AGYW_PREV", "OVC_HIVSTAT", "TX_PVLS")


#identify up denom indicators
df_msd <- mutate(df_msd, indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator))

#filter for just the variables needed
df_fltr <- filter(df_msd, 
                  indicator %in% ind_sel,
                  (standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) |
                    (indicator == "VMMC_CIRC" & standardizeddisaggregate == "Age/Sex") |
                    (indicator == "OVC_HIVSTAT" & standardizeddisaggregate == "ReportedStatus") |   
                    (indicator %in% c("HTS_TST", "HTS_TST_POS") & 
                       standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result")) |
                    (indicator == "AGYW_PREV")   
)

#clean up disaggregates
df_fltr <- df_fltr %>% 
  mutate(otherdisaggregate = ifelse(is.na(otherdisaggregate), "XXX", otherdisaggregate)) %>% 
  filter(otherdisaggregate != "No HIV Status") %>% #OVC_HIVSTAT
  mutate(otherdisaggregate = na_if(otherdisaggregate, "XXX"),
         vmmc_age = case_when(indicator == "VMMC_CIRC" & trendscoarse == "<15" ~ "<15",
                              indicator == "VMMC_CIRC" & trendsfine %in% c("15-19", "20-24", "25-29") ~ "15-29",
                              indicator == "VMMC_CIRC" & trendscoarse %in% c("Unknown Age", NA) ~ as.character(NA),
                              indicator == "VMMC_CIRC" ~ "30+"),
         modality = case_when(str_detect(modality, "Index") ~ "Index", 
                              modality %in% c("N", NA) ~ as.character(NA),
                              TRUE ~ "Other Modality"))
#aggreate up values
df_fltr <- df_fltr %>% 
  group_by(operatingunit, 
           snu1, psnu, psnuuid, 
           fundingagency, primepartner, 
           mech_code, mech_name, indicator, standardizeddisaggregate, vmmc_age,
           trendscoarse, sex, otherdisaggregate, modality, statushiv, fiscal_year) %>% 
  summarise_at(vars(targets, contains("qtr"), cumulative), sum, na.rm = TRUE) %>% 
  ungroup()

#net new targets
df_nn <- df_fltr %>% 
  dplyr::filter(indicator == "TX_CURR",
                standardizeddisaggregate == "Total Numerator") %>% 
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

#reshape long
df_long <- df_fltr %>% 
  reshape_msd("long") %>% 
  bind_rows(df_nn)

#MWI adjustment
# path <- "../Downloads/Partner Review File_2019_08_08_0750.xlsx"
# 
# df_dha <- read_excel(path, sheet = "TX_CURR Review File") %>%
#   select(snu1 = Region, psnu = District, fundingagency = FundingAgency, 
#             mech_code = MechanismNumber, val = TX_CURR_DHA)  %>% 
#   mutate(mech_code = as.character(mech_code),
#          operatingunit = "Malawi",
#          indicator = "TX_CURR",
#          standardizeddisaggregate = "Total Numerator",
#          period = "fy2019cumulative") %>% 
#   rename_official()
# 
# df_dha <- df_dha %>% 
#   group_by_if(is.character) %>% 
#   summarise_at(vars(val), sum, na.rm = TRUE) %>% 
#   ungroup()
# 
# df_long <- df_long %>% 
#   mutate(val = ifelse(operatingunit == "Malawi" & 
#                       indicator %in% c("TX_CURR","TX_NET_NEW") &
#                       str_detect(period, "2019(q|c)"), NA, val)) %>% 
#   bind_rows(df_dha)


#adjust for Tableau
df_long <- df_long %>% 
  mutate(type = case_when(str_detect(period, "q") ~ "results",
                          str_detect(period, "targets") ~ "targets",
                          str_detect(period, "cum") ~ "cumulative"),
         result_target = ifelse(type == "targets", "Targets", "Results"),
         period = str_remove(period, "20|_targets|cumulative"),
         period = str_remove(period, "_targets|cumulative"),
         period = toupper(period),
         fy = str_sub(period, end = 4),
         val_spread = val) %>%
  spread(type, val_spread)

#create a FY18Q4 TX_CURR varaible needed for retention
df_long <- df_long %>% 
  mutate(tx_curr_fy18q4 = ifelse(indicator == "TX_CURR" & 
                                   period == "FY18Q4", results, 0),
         tx_curr_fy18q4 = ifelse(is.na(tx_curr_fy18q4), 0, tx_curr_fy18q4)) %>% 
  group_by_at(c("indicator", "psnuuid", "mech_code")) %>% 
  mutate(tx_curr_fy18q4 = max(tx_curr_fy18q4, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(tx_curr_fy18q4 = ifelse(fy != "FY19", NA, tx_curr_fy18q4))

#create cumulative quarterly results and quarterly targets
grp <- c("psnuuid", "mech_code", "indicator", "standardizeddisaggregate", "vmmc_age",
         "trendscoarse", "sex", "otherdisaggregate", "statushiv", "modality", "fy", "period")

df_long <- df_long %>% 
  arrange_at(grp) %>% 
  mutate(results = ifelse(is.na(results), 0, results)) %>% 
  group_by_at(setdiff(grp, "period")) %>% 
  mutate(cumulative_qtrly = ifelse(indicator %in% snapshot, results, cumsum(results)),
         targets_qtrly = max(targets, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_at(vars(results,cumulative_qtrly, targets_qtrly), ~ na_if(., 0)) %>% 
  mutate(targets_qtrly = ifelse(is.finite(targets_qtrly), targets_qtrly, NA),
         source = "MSD")

#add in ISO codes for OU scatter plots
hlink <- "https://raw.githubusercontent.com/USAID-OHA-SI/Wavelength/master/data-raw/ISOcodes_PEPFAR_Countries.csv"
iso_map <- read_csv(hlink)
df_long <- df_long %>% 
  left_join(Wavelength::iso_map, by = "operatingunit") %>% 
  select(-c(countryname, iso))

# OUTLAYS PROCESSING ------------------------------------------------------


#import data
df_outlays <- read_excel(path_outlays, skip = 2)

#munge - clean variable names, adjust mech_code for M&O
df_outlays <- df_outlays %>% 
  select(operatingunit = `Operating Unit`,
         fundingagency = `Funding Agency`,
         mech_code = MechID,
         mech_name = `Mechanism Name`,
         primepartner = `Prime Partner`,
         record_type = `Record Type`,
         cop18_budget = `Approved COP 2018 Planning Level`,
         outlays = `Total Outlays for FY 2019 Q1 - Q3`) %>% 
  mutate(mech_code = ifelse(mech_code == "0", "99999", mech_code),
         mech_name = ifelse(record_type == "M&O", record_type, mech_name),
         primepartner = ifelse(record_type == "M&O", record_type, primepartner),
         period = "FY19") %>% 
  filter(record_type %in% c("IM", "M&O")) %>% 
  select(-record_type) %>% 
  mutate(source = "Outlays")

# HFR PROCESSING ----------------------------------------------------------

#import data
df_hfr <- read_tsv(path_hfr, col_types = c(.default = "c"))

#aggregate to PSNU level
df_hfr <- df_hfr %>% 
  mutate(date = ymd(date),
         otherdisaggregate = case_when(indicator == "TX_MMD" & otherdisaggregate == "1 month" ~ "1 month",
                                       indicator == "TX_MMD" ~ "2 months or more")) %>%
  filter(date >= "2019-07-01") %>% 
  mutate_at(vars(val, mer_targets, targets_gap, weekly_targets_gap), as.numeric) %>% 
  group_by(fundingagency, operatingunit, mechanismid, primepartner, 
           implementingmechanismname, hfr_pd, date,
           indicator, otherdisaggregate) %>% 
  summarise_at(vars(val, mer_targets, targets_gap, weekly_targets_gap), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  rename(mech_code = mechanismid,
         mech_name = implementingmechanismname) 

hfr_grp <- c("mech_code", "indicator", "otherdisaggregate")
df_hfr <- df_hfr %>% 
  group_by_at(hfr_grp) %>% 
  mutate(cumulative_wkly = ifelse(indicator %in% snapshot, val, cumsum(val))) %>% 
  ungroup() %>% 
  mutate_at(vars(cumulative_wkly), ~ na_if(., 0)) %>% 
  mutate(source = "HFR")


# JOIN AND EXPORT ---------------------------------------------------------

#bind sources
df_out <- bind_rows(df_long, df_outlays, df_hfr)

#rename mechanism and partner names to what is in DATIM
df_out <- rename_official(df_out)

#export
write_tsv(df_out, 
          file.path(path_output, paste0("FY19Q3_ReviewData_", Sys.Date() %>% str_remove_all("-"), ".txt")),
          na = "")