##   Import EA data
##   A.Chafetz
##   Purpose: EA data stored in two locations; merge and standardize
##   Date: 2018.04.12
##   Updated: 


# NOTES
# Source: https://www.pepfar.net/Project-Pages/collab-74/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2FProject-Pages%2Fcollab-74%2FShared%20Documents%2FEA%20FY17%20Factview&FolderCTID=0x012000C4AC9B35DC4AB84FAEEF47AE703A28CE0029B6723273A27B45BC2E77A97A80D252&View=%7BB007F13D-714D-4799-AD45-6E179739279F%7D#InplviewHashb007f13d-714d-4799-ad45-6e179739279f=FolderCTID%3D0x012000C4AC9B35DC4AB84FAEEF47AE703A28CE0029B6723273A27B45BC2E77A97A80D252-RootFolder%3D%252fProject%252dPages%252fcollab%252d74%252fShared%2520Documents%252fEA%2520FY17%2520Factview


# Dependencies ------------------------------------------------------------

library(tidyverse)

# Import Function ---------------------------------------------------------

read_ea <- function(filepath){
  df <- read_csv(filepath, 
           col_types = cols(
             .default = col_character(), 
             mechanismid = col_character(),
             mech_promis_id = col_character(),
             fy15 = col_double(),
             fy16 = col_double(),
             fy17 = col_double())) %>% 
  rename_all(., ~ tolower(.))
}


# Import ------------------------------------------------------------------


#import EA file (1 of 2)

df_ea1 <- read_ea("C:/Users/achafetz/Downloads/2015-2017 ICPI Batch1 Countries FV 10JAN18.csv")
df_ea2  <- read_ea("C:/Users/achafetz/Downloads/2015-2017 ICPI Batch2 Countries FV 10JAN18.csv")


# Merge and Save ----------------------------------------------------------

df_ea <- bind_rows(df_ea1, df_ea2)
  rm(df_ea1, df_ea2) 
write_rds(df_ea, "~/ICPI/Data/ICPI_EA_Structured_Dataset_PSNU_IM_20180110.Rds")
