#----------------------------------------------------------------------------------------
##  2.7.19 - j davis
##  Create site-level datasets for SCM division
##  At the request of Julia Bem and Meaghan Douglass, and approved by r lucas on 2/6/2019,
##  create-site level datasets for SCM division to pass to contractor for supply chain,
##  triangulation

##  site level MSD to be subset to indicators of interest (see below) and with 
##  mechanismuid, implementingmechanismname, mechanismid, and primepartner removed
##  OUs: Mozambique, Lesotho, Uganda, Tanzani, Namibia, Malawi, Botswana, Haiti
##  Angola, Zambia, Zimbabwe, Cote d'Ivoire, Cameroon


#----------------------------------------------------------------------------------------
##  dependancies

library(tidyverse)
library(readxl)
library(ICPIutilities)

##  indicators to keep
indc <- c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "TX_CURR", "TX_NEW")

##  site level data goes here
input <- "C:/Users/GHFP/Documents/data/12_21_release/site_level"

## where you want the output
output <- "C:/Users/GHFP/Documents/data/12_21_release/scm_output"

#-----------------------------------------------------------------------------------------
##  create rds files from site level

## create obj that is list of files
makey_rds <- dir(input, pattern = "*.txt", full.names = TRUE)

##  create function
site.msd <- function(file) {
  read_msd(file, remove_txt = TRUE)
}

map( .x = makey_rds, .f = ~site.msd(.x))

#-----------------------------------------------------------------------------------------
##  create master scm dataset

setwd("C:/Users/GHFP/Documents/data/12_21_release/site_level")

rdss <- dir(input, pattern = "*rds", full.names = TRUE)

##  create function to subset

get_scm <- function(input) {
  
  df_mer <- read_rds(input) %>% 
    filter((indicator %in% c("TX_NEW", "TX_CURR") &
              numeratordenom == "N" & agecoarse %in% c("<15", "15+")) |
             (indicator %in% c("TX_NEW", "TX_CURR","HTS_TST", "HTS_TST_POS", "HTS_TST_NEG") & 
                standardizeddisaggregate == "Total Numerator")) %>% 
    select(-c("mechanismuid", "implementingmechanismname", "mechanismid", "primepartner")) 
}

  big_df <- map_dfr(.x = rdss, .f = ~get_scm(.x))
  
  write_csv(big_df, file.path(output, "mer_q4_site_scm.csv"))

#-------------------------------------------------------------------------------------------
##  check vals
  
  big_df %>% distinct(standardizeddisaggregate) %>% arrange(standardizeddisaggregate) %>% print(n=Inf)
  big_df %>% distinct(operatingunit) %>% arrange(operatingunit) %>% print(n=Inf)
  big_df %>% distinct(indicator) %>% arrange(indicator) %>% print(n=Inf)
  
  
  
  
  
  
  
  
  