## datapack exercise
## 10.12.18
## JD
## trying to read in multiple datapacks

library(tidyverse)
library(readxl)

data_path <- "C:/Users/GHFP/Documents/ICPI/denominators/COP18_datapack/"
files <- dir(data_path, pattern = "*.xlsx", full.names = TRUE)

###

import.dp <- function(file) {
  
  ou <- read_xlsx(file, sheet = "Home", range = "O1", col_names = FALSE) %>% 
    pull()
  
  print(ou)
  
  df <- read_xlsx(file, sheet = "Assumption Input", skip = 3 ) %>%   
    mutate(operatingunit = !!ou) %>% 
    select(operatingunit, Msnulist, M_priority_snu, contains("plhiv"), contains("art_est")) %>% 
    filter(!Msnulist %in% c("set default values >>>", "Filter Row")) 
    return(df)
    
}

data <- map_dfr(.x = files, .f = ~import.dp(.x)) %>% 
  gather(meta, plhiv, contains("plhiv")) %>% 
  mutate(meta = str_remove(meta, "M_plhiv_")) %>% 
  mutate(age = ifelse(str_detect(meta, "u15"), "<15", "total")) %>% 
  mutate(year = str_remove(meta, "u15_")) %>%
  group_by(operatingunit, Msnulist, M_priority_snu) %>%
  select(-meta) %>% 
  spread(age, plhiv) %>% 
  mutate(`15+` = round(total - `<15`)) %>% 
  gather(age, plhiv, 5:7) %>% 
  mutate(plhiv = round(plhiv))

write_csv(data, file.path(data_path, "COP18_datapacks_combined.csv"))
  
#######################
data <- map_dfr(.x = files, .f = ~import.dp(.x)) %>% 
  gather(meta, value, contains("plhiv"), contains("art_est_")) %>% 
  mutate(meta = str_remove(meta, "M_")) %>% 
  mutate(age = ifelse(str_detect(meta, "u15"), "<15", "total")) %>% 
  mutate(year = str_remove(meta, "u15_")) %>% 
  mutate(year = str_remove(year, c("plhiv_"))) %>% 
  mutate(year = str_remove(year, c("art_est_"))) %>%
  mutate(meta = str_remove(meta, "_fy17"))%>%
   mutate(meta = str_remove(meta,  "_fy18"))%>%
   mutate(meta = str_remove(meta,  "_fy19"))%>%
   mutate(meta = str_remove(meta,  "_u15_fy17"))%>%
   mutate(meta = str_remove(meta,     "_u15_fy18"))%>%
   mutate(meta = str_remove(meta,     "_u15_fy19"))%>%
   mutate(meta = str_remove(meta,     "_est_fy17"))%>%
   mutate(meta = str_remove(meta,     "_est_fy18"))%>%
   mutate(meta = str_remove(meta,     "_est_fy19"))%>%
   mutate(meta = str_remove(meta, "_est_u15_fy17"))%>%
   mutate(meta = str_remove(meta, "_est_u15_fy18"))%>%
   mutate(meta = str_remove(meta, "_est_u15_fy19")) %>% 
  mutate(meta = str_remove(meta, "_est")) %>% 
  mutate(meta = str_remove(meta, "_est_u15")) %>% 
  mutate(meta = str_remove(meta, "_u15")) %>% 
  write_csv(file.path(data_path, "COP18_datapacks_combined_art.csv"))


########################
##
###########################

data <- map_dfr(.x = files, .f = ~import.dp(.x)) %>% 
  gather(meta, value, contains("plhiv"), contains("art_est_")) %>% 
  mutate(meta = str_remove(meta, "M_")) %>% 
  mutate(age = ifelse(str_detect(meta, "u15"), "<15", "total")) %>% 
  mutate(year = str_remove(meta, "u15_")) %>% 
  mutate(year = str_remove(year, c("plhiv_"))) %>% 
  mutate(year = str_remove(year, c("art_est_"))) %>%
  mutate(meta = str_extract(meta, "art|plhiv")) %>%
  write_csv(file.path(data_path, "COP18_datapacks_combined_art.csv"))




%>% 
  spread(age, meta)

%>% 
  mutate(`15+` = round(total - `<15`)) %>% 
  gather(age, plhiv, 5:7) %>% 
  mutate(plhiv = round(plhiv))


data %>% distinct(meta) %>% arrange(meta) %>% print(n=Inf)
