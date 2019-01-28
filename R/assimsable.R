## create SIMS



#install devtools
install.packages("devtools")

#install/update package
devtools::install_github("USAID-OHA-SI/asSIMSble")
#load package
library("asSIMSble")

library(tidyverse)
library(ICPIutilities)

#parameters - adjust to reflext location on your machine
  filepath <- "C:/Users/GHFP/Documents/data/11_15_release/tester"
  template_filepath <- "C:/Users/GHFP/Documents/data/11_15_release/asSIMSble/Products/sims_prioritization_template.xlsx"
  output_folderpath <- "C:/Users/GHFP/Documents/data/11_15_release/asSIMSble/Products"
  


  #list files
  files <- dir(filepath, pattern = "*.rds", full.names = TRUE)
  

  #process multiple template creations
  map(.x = files,
      .f = ~ assemble(.x, template_filepath, output_folderpath))
  

  