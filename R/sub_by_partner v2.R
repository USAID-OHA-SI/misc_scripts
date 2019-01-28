##  Partner site level datasets
##  Purrr my way through some partner data
##  10.19.18
##  jdavis


library(ICPIutilities)
library(tidyverse)

data_path <- "C:/Users/GHFP/Documents/data/11_15_release"
results <- "C:/Users/GHFP/Documents/data/11_15_release/IP_data"
data_path <- "C:/Users/GHFP/Documents/data/11_15_release/site_level"


texty <- dir(data_path, pattern = "*.txt", full.names = TRUE)
files <- dir(data_path, pattern = "*.rds", full.names = TRUE)

# list of all partner names of interest
  jsi_lst <- c("John Snow Inc (JSI)", "John Snow, Inc.")
  fhi_lst <- c("FHI 360")
  psi_lst <- c("Population Services International")
  
  listy <- list(c("John Snow Inc (JSI)", "John Snow, Inc."), c("FHI 360"), c("Population Services International"))

  ## bulk convert all the txt to rsd

  
site.msd <- function(file) {
  read_msd(file, remove_txt = TRUE)
}

map( .x = texty, .f = ~site.msd(.x))


##############

jsi <- read_rds(file.path(data_path, "MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.rds")) %>% 
  filter(primepartner %in% jsi_lst) %>% 
  write_csv(file.path(results, "jsi_MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.csv"))

fhi <- read_rds(file.path(data_path, "MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.rds")) %>% 
  filter(primepartner %in% fhi_lst) %>% 
  write_csv(file.path(results, "fhi_MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.csv"))

  ## itertae through the msds for each partner

for(i in listy) {
  
  
  
}


#######

gen.part <- function(file) {
  
  read_rds(file) %>% 
    filter(indicator %in% listy) %>% 
      write_csv(file.path(data_path, "`listy`".csv))
    

}

df <- map(.x = files, .f = ~gen.part(.x))




