##  Create a site-level file for certain IPs
##  2.27.19
##  jdavis

#--------------------------------------------------------------
# dependancies

library(tidyverse)
library(ICPIutilities)

#---------------------------------------------------------------
# Set up folders

input <- "C:/Users/GHFP/Documents/data/2_15_19_release/site_level"
output <- "C:/Users/GHFP/Documents/data/2_15_19_release/ip_files"

#----------------------------------------------------------------
# create some objects

# list of Partners
site_files <- list.files("C:/Users/GHFP/Documents/data/2_15_19_release/site_level", full.names = TRUE)

##  create rdss from MSDs

texty <- dir(input, pattern = "*.txt", full.names = TRUE)

site.msd <- function(file) {
  read_msd(file, remove_txt = TRUE)
}

map( .x = texty, .f = ~site.msd(.x))

#----------------------------------------------------------------

site_ip <- function(filepath) {
  
  ip_list <- c("John Snow Inc (JSI)", "John Snow, Inc.", "FHI 360", "Population Services International")
  
  readr::read_rds(filepath) %>% 
    filter(primepartner %in% ip_list) %>% 
    mutate(primepartner = ifelse(primepartner == "John Snow, Inc.", "John Snow Inc (JSI)", primepartner))

}

df <- map_dfr(.x = site_files, .f = site_ip)

df %>%
  split_save(primepartner, output, "msd_q1.1_", include_date = TRUE) 
  
  
  
  











