#----------------------------------------------------------------------------------------
##  2.7.19 - j davis
##  Updated: 2.24.19
##  Create site-level datasets for SCM division
##  At the request of Julia Bem and Meaghan Douglass, and approved by r lucas on 2/6/2019,
##  create-site level datasets for SCM division to pass to contractor for supply chain,
##  triangulation
##  UPDATE 

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

##  indicators to keep
indc <- c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "TX_CURR", "TX_NEW", "TX_PVLS")

##  site level data goes here
input <- "C:/Users/Josh/Documents/data/fy20_q1_v1/site_level"


## where you want the output
output <- "C:/Users/Josh/Documents/data/fy20_q1_v1/scm_output"

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
##  create rds files from site level

## create obj that is list of files
  makey_rds <- dir(input, pattern = "*.zip", full.names = TRUE)
  
  ##  create function
  site.msd <- function(file) {
    ICPIutilities::read_msd(file, remove_txt = TRUE)
  }
  
  purrr::map( .x = makey_rds, .f = ~site.msd(.x))

#-----------------------------------------------------------------------------------------
##  create master scm dataset

rdss <- dir(input, pattern = "*rds", full.names = TRUE)

##  create function to subset

##
get_scm <- function(input) {
  
  df_mer <- readr::read_rds(input) %>% 
    dplyr::filter((indicator %in% indc &
              numeratordenom == "N" & trendscoarse %in% c("<15", "15+")) |
             (indicator %in% indc & 
                standardizeddisaggregate == "Total Numerator"))  
}

##

  big_df <- purrr::map_dfr(.x = rdss, .f = ~get_scm(.x))
  
  #write_csv(big_df, file.path(input, "all_sites_q2.csv"))

  
  readr::write_csv(big_df, file.path(output, "mer_fy20_q1_v1_site_scm.csv"))

#-------------------------------------------------------------------------------------------
##  check vals
  
  big_df %>% dplyr::distinct(standardizeddisaggregate) %>% dplyr::arrange(standardizeddisaggregate) %>% print(n=Inf)
  big_df %>% dplyr::distinct(operatingunit) %>% dplyr::arrange(operatingunit) %>% print(n=Inf)
  big_df %>% dplyr::distinct(indicator) %>% dplyr::arrange(indicator) %>% print(n=Inf)


  
  
  
  