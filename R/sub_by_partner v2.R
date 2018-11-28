##  Partner site level datasets
##  Purrr my way through some partner data
##  10.19.18
##  jdavis


library(ICPIutilities)
library(tidyverse)

data_path <- "C:/Users/GHFP/Documents/data/11_15_release"

texty <- dir(data_path, pattern = "*.txt", full.names = TRUE)
files <- dir(data_path, pattern = "*.Rds", full.names = TRUE)

site.msd <- function(file) {
  read_msd(file, remove_txt = TRUE)
}

map( .x = texty, .f = ~site.msd(.x))




gen.part <- function(file) {
  
  read_rds(file)

}

df <- map(.x = files, .f = ~gen.part(.x))
