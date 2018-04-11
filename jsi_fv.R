##   Project:  JSI structured dataset
##   Autors:   J.Davis - USAID
##   Purpose:  create JSI - specific OU by IM structured dataset
##   Date:     2018-04-10
##   Updated:  2018-04-11


# Dependencies ------------------------------------------------------------

library(tidyverse)
library("devtools")
library(writexl)
#install_github("ICPI/ICPIutilities")
library(ICPIutilities)



# User Inputs -------------------------------------------------------------

filepath <- "C:/Users/GHFP/Documents/data/3.23 refresh"
partner <- c("John Snow Inc (JSI)", "John Snow, Inc.")

#########
# read in psnu by im txt file, and save as .rds

df <- read_msd("ICPI_MER_Structured_Dataset_PSNU_IM_20180323_v2_1.txt", path = filepath)

# next create distict list of prime partners to locate which ones to filter for(JSI, etc..)

prime_partner<- df %>% 
  distinct(primepartner) %>% 
  arrange(primepartner)

# filter out only primerpartners of interest

jsi <- filter(df, primepartner=="John Snow Inc (JSI)" | primepartner=="John Snow, Inc.")

# from the filtered out data frame, create list of IMs

jsi_mechs <- jsi %>%
  distinct(implementingmechanismname) %>%
  arrange(implementingmechanismname)

# write both files to excel

write_xlsx(jsi, path = "3.23.2018_jsi_PSNU_IM.xlsx", col_names = TRUE)
write_xlsx(jsi_mechs, path = "3.23.2018_jsi_IMs.xlsx", col_names = TRUE)

#




