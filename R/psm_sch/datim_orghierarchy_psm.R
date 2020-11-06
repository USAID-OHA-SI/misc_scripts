#----------------------------------------------------------------------------------------
##  Created: 10.22.20 - j davis
##  Updated:
##        
##  Create a version of the DATIM orghierarchy table for PSM
##  
##  Remove lat long
##  Subset to follow OUs: 
##  OUs: Mozambique, Lesotho, Uganda, Tanzani, Namibia, Malawi, Botswana, Haiti
##  Angola, Zambia, Zimbabwe, Cote d'Ivoire, Cameroon, Nigeria, Cameroon, DRC, Botswana, Burundi

#dependancies-------------------------------------------------------

library(tidyverse)
library(readxl)
library(ICPIutilities)
library(vroom)
library(glamr)

#globals--------------------------------------------------------------------------------
##  here is where the orghierarhcy table is
input <- "C:/Users/Josh/Documents/data/fy20_q3_v1/HFR_FY20_GLOBAL_orghierarchy_20201001.csv"

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

#read in and munge--------------------------------------------------

df <- readr::read_csv(input) %>% 
  filter(operatingunit %in% OUs) %>% 
  select(-latitude, -longitude)

#write------------------------------------------------
df %>% readr::write_csv(file.path(output, "DATIM_orghierarchy_10.2020.csv"))















