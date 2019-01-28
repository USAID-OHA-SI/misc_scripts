##  read in data from 2019 PLL
##  data was copied and pasted into worksheets, read in worksheets
##  1.25.19


  
  #--------------------------------------------------------------------
  
  library(tidyverse)
  library(readxl)
  
  path <- "C:/Users/GHFP/Documents/ICPI/COP19/PLL_exercise/PLL_data"
  files <- dir(path, pattern = "*.xlsx", full.names = TRUE)
  
  get_ppl <- function(path){
    df_mer <- read_xlsx(path, sheet = "FY20 targets", skip = 1) %>% 
      gather(disag, value, -ou, -indicator, -period ) %>% 
      mutate(class = "mer") %>% 
      mutate(source  = "pll")
    
    df_budget <- read_xlsx(path, sheet = "Outlays vs Budget table", skip = 1) %>% 
      gather(meta, value, -ou, -funding_agency) %>%
      separate(meta, c("indicator", "period"), ",") %>% 
      rename("disag"  =  "funding_agency") %>% 
      mutate(period = as.double(period)) %>% 
      mutate(class = "budget") %>% 
      mutate(source  = "pll")
    
    df <- full_join(df_mer, df_budget)
      
  }
  
  pll_all <- map_dfr(.x = files, .f = ~get_ppl(.x))
  
  
  
    #####
  
