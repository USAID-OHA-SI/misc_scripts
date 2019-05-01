# PROJECT:  COP19 
# AUTHOR:   A.CHAFETZ
# PURPOSE:  Create trend visuals for partners in HTS_POS and TX_NEW
# DATE:     2019-03-08


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(RColorBrewer)


# MUNGE -------------------------------------------------------------------

#folder location
  folderpath <- "C:/Users/achafetz/Downloads/"

## PLHIV

  #read in Data Pack
    df_dp <- read_excel(file.path(folderpath, "DataPack_Haiti_V2_Fixed (4).xlsx"),
                        sheet = "Epi Cascade I",
                        skip = 4)

  #clean up
    df_dp <- df_dp %>% 
      select(psnu = PSNU, Sex, AgeCoarse, plhiv = `PLHIV.NA.Age/Sex/HIVStatus.20T`) %>% 
      mutate(Sex = ifelse(AgeCoarse == "<15", "Peds", Sex)) %>%
      unite(group, c(Sex, AgeCoarse), sep = " ") %>%
      group_by(psnu, group) %>% 
      summarise(plhiv = sum(plhiv, na.rm = TRUE)) %>% 
      ungroup() %>% 
      separate(psnu, c("psnu", "psnuuid"), sep = " \\(") %>%
      mutate(psnuuid = str_remove(psnuuid, "\\)"))

## MAPPING CROSS WALK BETWEEN PSNU AND DISTRICT
    
  #import and clean up
    df_psnu_mapping <- read_csv(file.path(folderpath, "HTI_psnuuid_mapping.csv")) %>% 
      mutate(District = ifelse(District == "M\xd4LE SAINT NICOLAS", "MÔLE SAINT NICOLAS", District)) %>% 
      select(-psnu)

## DIAGNOSED
  
  #read in diagnosted data
    df_diagnosed <- read_excel(file.path(folderpath, "MAPS of GAPS____SALVH-Total HIV diagnosed.xls"),
                               sheet = "TotalNumberDiagnosedbygroupAgea")

  #clean up
    df_diagnosed <- df_diagnosed %>% 
      filter(!GroupeAge %in% c("NR", "(A) 0 à 1 an"),
             District != "NR") %>% 
      mutate(agecoarse = ifelse(GroupeAge %in% c("(B) 1 à 9 ans", "(C) 10 à 14 ans"), "<15", "15+"),
             Sexe = case_when(agecoarse == "<15" ~ "Peds", 
                              Sexe == "F"        ~ "Female",
                              Sexe == "M"        ~ "Male")) %>%
      filter(!is.na(Sexe)) %>% 
      unite(group, c(Sexe, agecoarse), sep = " ") %>%
      group_by(District, group) %>% 
      summarise(diagnosed = sum(`# of unique ID`, na.rm = TRUE)) %>% 
      ungroup() %>% 
      left_join(df_psnu_mapping, by = "District") %>% 
      mutate(psnuuid = ifelse(District == "MÔLE SAINT NICOLAS", "c0oeZEJ8qXk", psnuuid)) %>% 
      select(psnuuid, group, diagnosed)

  rm(df_psnu_mapping)


## UNMET NEED

  #merge
    df_unmet_need <- full_join(df_dp, df_diagnosed, by = c("psnuuid", "group"))

  #clean up
    df_unmet_need <- df_unmet_need %>% 
      mutate(unmet_share = 1 - (diagnosed/plhiv),
             unmet_share = ifelse(unmet_share < 0, 0, unmet_share))

  #export
    write_csv(df_unmet_need, file.path(folderpath, "COP19_HTI_unmet_need.csv"), na = "")
    
  rm(df_diagnosed, df_dp)
  

# IMPORT SHAPEFILE --------------------------------------------------------


  shp_hti <- st_read(file.path(folderpath, "Haiti_PROD_5_District_DistrictLsib_2017_May", "Haiti_PROD_5_District_DistrictLsib_2017_May.shp")) %>% 
    mutate(psnuuid = as.character(uid)) %>% 
    select(psnuuid, geometry)



# MAP THEME ---------------------------------------------------------------

  map_theme <- function() {
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour="white"),
          panel.grid.minor = element_line(colour="white"),
          panel.background = element_blank()
    )
  }
  
  palette <- brewer.pal("YlOrBr", n = 5)


# LEGEND ------------------------------------------------------------------

  
  ntile_legend <- function(col, n = 5){
    col %>% 
      quantile(., seq(0, 1, 1/n)) %>% 
      round(digits = 2) %>% 
      tibble(breaks = .) %>% 
      mutate(quantile = 1:n(),
             breaks = scales::percent(breaks),
             legend_lab = paste(breaks, "-", lead(breaks))) %>%
      filter(quantile != max(quantile)) %>% 
      pull(legend_lab)
  }
  
  #base off overall
 df_unmet_psnu <- df_unmet_need %>% 
    group_by(psnuuid) %>% 
    summarise_at(vars(plhiv, diagnosed), sum, na.rm = TRUE) %>% 
    mutate(unmet_share = 1 - (diagnosed/plhiv),
           unmet_share = ifelse(unmet_share < 0, 0, unmet_share)) %>% 
   filter(!is.na(unmet_share),
          unmet_share > 0)
  
  lgnd <- ntile_legend(df_unmet_psnu$unmet_share, 4)
  lgnd_adj <- c("0% [at or surpassed PLHIV est.]", "1%-36%", "37%-61%", "62%-85%", "85%-100%")
  
# Map ---------------------------------------------------------------------
  
  df_map <- df_unmet_need %>% 
    full_join(shp_hti, by = "psnuuid") %>% 
    filter(!psnu %in% c("_Military Haiti", "Baradères", "Cerca-la-Source", "Coteaux", NA),
           !is.na(unmet_share))
  
  #create quintiles for mapping
  # df_map <- df_map %>% 
  #   mutate(quantile = ntile(df_map$unmet_share, 5))
    df_map <- df_map %>% 
      mutate(quantile = case_when(unmet_share == 0  ~ 1,
                                  unmet_share < .37 ~ 2,
                                  unmet_share < .62 ~ 3,
                                  unmet_share < .86 ~ 4,
                                  unmet_share <= 1  ~ 5))
  
  df_map %>%
    ggplot() +
    geom_sf(aes(fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      labels = lgnd_adj,
                      name = "Legend") +
    facet_wrap(. ~ group) +
    map_theme() +
    labs(title = "UNMET GAP TO 1ST 90",
         caption = "Source: COP19 Data Pack and SALVH [Known Status],
         Note: <1 removed due to low coverage") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(color = "#494949", size = 9))

  ggsave(file.path(folderpath, "COP19_HTI_UnmetNeed_Map.png"), dpi = 300,
         width = 9, height = 4, units = "in")
  
  df_map %>%
    filter(group == "Male 15+") %>% 
    ggplot() +
    geom_sf(aes(fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      labels = lgnd_adj,
                      name = "Legend") +
    map_theme() +
    labs(title = "UNMET GAP TO 1ST 90 | Males 15+",
         caption = "Source: COP19 Data Pack and SALVH [Known Status],
         Note: <1 removed due to low coverage") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(color = "#494949", size = 9))
  
  ggsave(file.path(folderpath, "COP19_HTI_UnmetNeed_Map_Male.png"), dpi = 300,
         width = 8, height = 8, units = "in")
  
  df_map %>%
    filter(group == "Female 15+") %>% 
    ggplot() +
    geom_sf(aes(fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      labels = lgnd_adj,
                      name = "Legend") +
    map_theme() +
    labs(title = "UNMET GAP TO 1ST 90 | Female 15+",
         caption = "Source: COP19 Data Pack and SALVH [Known Status],
         Note: <1 removed due to low coverage") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(color = "#494949", size = 9))
  
  ggsave(file.path(folderpath, "COP19_HTI_UnmetNeed_Map_Female.png"), dpi = 300,
         width = 8, height = 8, units = "in")
  
  df_map %>%
    filter(group == "Peds <15") %>% 
    ggplot() +
    geom_sf(aes(fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      labels = lgnd_adj,
                      name = "Legend") +
    map_theme() +
    labs(title = "UNMET GAP TO 1ST 90 | Female 15+",
         caption = "Source: COP19 Data Pack and SALVH [Known Status],
         Note: <1 removed due to low coverage") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(color = "#494949", size = 9))
  
  ggsave(file.path(folderpath, "COP19_HTI_UnmetNeed_Map_Peds.png"), dpi = 300,
         width = 8, height = 8, units = "in")

