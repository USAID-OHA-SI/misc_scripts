# Combine datasets
# Purpose: show how csv files can be combined via purrr
# Author: A.Chafetz
# Date:   2018-11-01


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(fs)


# Create a temp folder ----------------------------------------------------

  tmp_fldr <- path_temp()
  (output_fldr <- path(tmp_fldr, "TESTING")) #create file path for OU
  dir_create(output_fldr) #create folder


# FUNCTIONS ---------------------------------------------------------------

  # generate a dummy dataset for a given country
    gendummy_df <- function(ctry){
      df <- data.frame(replicate(5,sample(0:1,10,rep=TRUE)))
      df <- df %>% 
        mutate(opunit = !!ctry) %>% 
        select(opunit, everything())
      return(df)
    }

  # save dataset 
    save_df <- function(df, ctry, folderpath){
      name <- paste0("GBV_", ctry, ".csv")
      filepath <- path(folderpath, name)
      write_csv(df, filepath)
    }
    
  # combine generate and save
    run <- function(ctry, folderpath){
      gendummy_df(ctry) %>% 
      save_df(ctry, folderpath)
    }
  

# Test purrr --------------------------------------------------------------
  
  #test what the dummy df looks like
    gendummy_df("Angola")
    
  #can do this one by one 
    # run("Angola")
    # run("Nigeria")
    # run("Tanzania")
  #better option is to use map/walk (walk doesn't print output)
    ous <- c("Angola", "Nigeria", "Tanzania")
    walk(.x = ous,
         .f = ~ run(.x, output_fldr))
  
  #lets see what the directory looks like
    (files <- list.files(output_fldr, full.names = TRUE))

  #let's use map_dfr to combine all the csv files
    map_dfr(.x = files,
            .f = ~ read_csv(.x)) %>% 
      print(n = Inf)
