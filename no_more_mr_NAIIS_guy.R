## Pull NAIIS data

library(tidyverse)
library(readxl)

data <- "C:\\Users\\GHFP\\Documents\\data\\NAIIS"

data_path <- "C:\\Users\\GHFP\\Documents\\data\\NAIIS"

files <- dir(data_path, pattern = "*.xlsx", full.names = TRUE)

import.naiis <- function(file) {
  
  state <- read_xlsx(file, sheet = "Home", range = "O1", col_names = FALSE) %>% 
    pull()
  
  print(ou)
  
  df <- read_xlsx(file, sheet = "Assumption Input", skip = 3 ) %>%   
    mutate(operatingunit = !!ou) %>% 
    select(operatingunit, Msnulist, M_priority_snu, contains("plhiv")) %>% 
    filter(!Msnulist %in% c("set default values >>>", "Filter Row")) 
  return(df)
  
}

naiis <- read_xlsx(file.path(data, "nasarawa.xlsx"), skip = 1)
