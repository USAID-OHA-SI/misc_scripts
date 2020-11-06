#----------------------------------------------------------------------------------------
##  Created: 2.7.19 - j davis
##  Updated: 2.24.19
##        Updated: 7.2.2020 by MH- taking over this task
##        Updated: 10.21.20 to take advantage of genie upgrade
##  Create site-level datasets for SCM division
##  At the request of Julia Bem and Meaghan Douglass, and approved by r lucas on 2/6/2019,
##  create-site level datasets for SCM division to pass to contractor for supply chain,
##  triangulation
##  UPDATE 7.10.20 - update to collapse obs to exclude mech, IP, and agency

##  site level MSD to be subset to indicators of interest (see below) and with 
##  mechanismuid, implementingmechanismname, mechanismid, and primepartner removed
##  OUs: Mozambique, Lesotho, Uganda, Tanzani, Namibia, Malawi, Botswana, Haiti
##  Angola, Zambia, Zimbabwe, Cote d'Ivoire, Cameroon, Nigeria, Cameroon, DRC, Botswana, Burundi

install.packages("devtools")
devtools::install_github("ICPI/ICPIutilities")

#----------------------------------------------------------------------------------------
##  dependancies

library(tidyverse)
library(readxl)
library(ICPIutilities)
library(vroom)
library(glamr)

##  indicators to keep
indc <- c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "TX_NEW", "TX_PVLS")

##  site level data goes here
input <- "C:/Users/Josh/Documents/data/fy20_q3_v1/psm/mer_psm"

## where you want the output
output <- "C:/Users/Josh/Documents/data/fy20_q3_v1/psm/out"

## list of OUs per Nagesh on 3/25 and approved by SCM
OUs <- c("Angola",
         "Botswana",
         "Burundi",
         "Cameroon",
         "Cote d'Ivoire",
         "Democratic Republic of the Congo",
         "Ghana",
         "Haiti",
         "Lesotho",
         "Malawi",
         "Mozambique",
         "Namibia",
         "Nigeria",
         "South Sudan",
         "Tanzania",
         "Uganda",
         "Zambia",
         "Zimbabwe")


#-----------------------------------------------------------------------------------------
##  create master scm dataset

files <- dir(input, pattern = "*txt", full.names = TRUE)

##  create function to subset

##
get_scm <- function(input) {
  
  df_mer <- ICPIutilities::read_msd(input) %>% 
    ICPIutilities::reshape_msd("long") %>% 
    dplyr::filter(period == 'fy2020q3',
                  countryname %in% c(OUs),
                  indicator == "TX_CURR" &
                    is.na(trendscoarse) | trendscoarse == "15+" &
                    standardizeddisaggregate %in%
                    c("Age/Sex/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")|
                    indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>% 
    dplyr::group_by_at(vars(-primepartner, -fundingagency, -mech_code, -mech_name,
                     -pre_rgnlztn_hq_mech_code, -prime_partner_duns, -award_number)) %>%
    dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    dplyr::ungroup()  
}

##

big_df <- purrr::map_dfr(.x = files, .f = ~get_scm(.x))

readr::write_csv(big_df, file.path(output, "mer_fy20_q3_site_psm.csv"))

#-------------------------------------------------------------------------------------------
##  check vals

big_df %>% dplyr::distinct(standardizeddisaggregate) %>% dplyr::arrange(standardizeddisaggregate) %>% print(n=Inf)
big_df %>% dplyr::distinct(indicator, standardizeddisaggregate) %>% dplyr::arrange(standardizeddisaggregate) %>% print(n=Inf)

big_df %>% dplyr::distinct(operatingunit) %>% dplyr::arrange(operatingunit) %>% print(n=Inf)
big_df %>% dplyr::distinct(indicator) %>% dplyr::arrange(indicator) %>% print(n=Inf)


#test-----------------------------------------------------
input <- "C:/Users/Josh/Documents/data/fy20_q3_v1/psm/mer_psm/Genie_SITE_IM_Angola_Frozen_97fcc5e8-68a0-4d41-9d3a-f6a4c022b5c4.txt"


df_mer_old <- ICPIutilities::read_msd(input) %>%
  ICPIutilities::reshape_msd("long") %>% 
  dplyr::filter(period == 'fy2020q3',
                countryname %in% c(OUs),
                (indicator %in% indc &
                   numeratordenom == "N" & trendscoarse %in% c("<15", "15+")) |
                  (indicator %in% indc & 
                     standardizeddisaggregate == "Total Numerator")) %>% 
  dplyr::group_by_at(vars(-primepartner, -fundingagency, -mech_code, -mech_name,
                          -pre_rgnlztn_hq_mech_code, -prime_partner_duns, -award_number,
                          -ageasentered, -trendsfine, -trendssemifine, -sex, -categoryoptioncomboname)) %>%
  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  dplyr::ungroup() 

df_mer <- ICPIutilities::read_msd(input) %>%
  ICPIutilities::reshape_msd("long") %>% 
  dplyr::filter(period == 'fy2020q3',
                countryname %in% c(OUs),
                indicator == "TX_CURR" &
                           is.na(trendscoarse) | trendscoarse == "15+" &
                           standardizeddisaggregate %in%
                           c("Age/Sex/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")|
                           indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>% 
  dplyr::group_by_at(vars(orgunituid, sitename, operatingunit, countryname, snu1, psnu, facility, sitetype,
                          indicator, standardizeddisaggregate, trendscoarse, period)) %>%
  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  spread(period, val)

##trying to get rid of keypops
df_mer <- ICPIutilities::read_msd(input) %>%
  ICPIutilities::reshape_msd("long") %>% 
  dplyr::filter(period == 'fy2020q3',
                countryname %in% c(OUs),
                indicator == "TX_CURR" &
                  is.na(trendscoarse) | trendscoarse == "15+" &
                  standardizeddisaggregate %in%
                  c("Age/Sex/HIVStatus", "Total Numerator", "Age/Sex/HIVStatus")|
                  indicator %in% indc & standardizeddisaggregate == "Total Numerator") %>% 
  dplyr::group_by_at(vars(orgunituid, sitename, operatingunit, countryname, snu1, psnu, facility, sitetype,
                          indicator, standardizeddisaggregate, trendscoarse, period)) %>%
  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  spread(period, val)


df_mer %>% dplyr::distinct(standardizeddisaggregate) %>% dplyr::arrange(standardizeddisaggregate) %>% print(n=Inf)
df_mer %>% dplyr::distinct(indicator, standardizeddisaggregate) %>% dplyr::arrange(standardizeddisaggregate) %>% print(n=Inf)



