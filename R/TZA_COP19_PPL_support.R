##  Project:  Tanzania Planning Letter
##  Author:   Aaron Chafetz, USAID
##  Purpose:  Support TNZ country cluster for PPL discussion w/ FO
##  Date:     2019-01-26
##  Updated:  2019-01-27


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)

#MSD folderpath
folderpath <- "~/ICPI/Data"

# IMPORT ------------------------------------------------------------------

df_tza <- read_rds(file.path(folderpath, "MER_Structured_Dataset_OU_IM_FY17-18_20181221_v2_1.rds")) %>% 
  filter(operatingunit == "Tanzania")


# PARTNER DISPLAY NAMES ---------------------------------------------------

dname <- tibble::tribble(
           ~mechanismid,   ~displayname,
                "16874",         "THPS",
                "17293",          "MDH",
                "17986", "Ariel Glaser",
                "17991",     "Columbia",
                "18131",  "IntraHealth",
                "18170",          "MOH",
                "16784",      "JHPIEGO",
                "16787",     "AIDSFree",
                "17103",       "Baylor",
                "17358",         "Pact",
                "17420",         "KNCV",
                "18060",        "EGPAF",
                "18237",     "Deloitte",
                "16885",   "MUHAS-TAPP"
           )



# DEFINE PLOT THEME -------------------------------------------------------

tza_theme <- function(){
  theme_light() +
  theme(panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "#808080"),
        strip.text = element_text(size = 18, face = "bold"),
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
  )
}

# ACHIEVEMENT BY PARTNER --------------------------------------------------

ind_sel <- c("HTS_TST", "HTS_TST_POS", "TX_CURR",
             "TX_NEW", "OVC_SERV", "VMMC_CIRC")

df_ach <- df_tza %>% 
  filter(indicator %in% ind_sel,
         standardizeddisaggregate == "Total Numerator",
         !mechanismid  %in% c("00000", "00001")) %>% 
  group_by(fundingagency, mechanismid, indicator) %>% 
  summarise_at(vars(matches("apr|targets")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, starts_with("fy")) %>% 
  mutate(pd = str_replace(pd, "apr", "_apr")) %>% 
  separate(pd, c("fy", "type"), sep = "_") %>% 
  spread(type, val, fill = 0) %>% 
  filter_at(vars(apr, targets), any_vars(. != 0)) %>% 
  mutate(ach = round(apr / targets, 2))

df_ach <- df_ach %>% 
  filter(fy == "fy2018",
         fundingagency %in% c("HHS/CDC", "USAID")) %>% 
  mutate(indicator = factor(indicator, ind_sel),
         ind_order = as.numeric(indicator),
         #name = paste0(primepartner, " [", fundingagency, " ", mechanismid, "]"), 
         ach = ifelse(is.infinite(ach), NA, ach)) %>%
  filter(!is.na(ach))

df_ach <- df_ach %>% 
  left_join(dname, by = "mechanismid") %>% 
  mutate(color = case_when(ach < .75  ~ "#CC5234",
                           ach < .9   ~ "#e09785",
                           TRUE       ~ "#B2B2B2"))

df_ach %>% 
  ggplot(aes(indicator, displayname)) +
  geom_point(size = 6, color = df_ach$color) +
  geom_text(aes(family = "Gill Sans MT", size = 14),
            color = df_ach$color,
            label = percent(df_ach$ach), 
            nudge_x = -.3) +
  #scale_fill_manual(values = c("#B2B2B2", "#739bcc", "#335B8E")) +
  facet_grid(fundingagency ~ ., scales = "free_y") +
  labs(x = "", y = "", caption = "Source: MSD FY18Q4c") +
  scale_x_discrete(position = "top") +
  theme_light() +
  theme(panel.border = element_blank(),
        strip.background = element_rect(fill = "#969696"),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        axis.text.x = element_text(face = "bold"),
        plot.caption = element_text(size = 11, color = "#808080")
        )
  
  ggsave("~/COP19/Tanzania/Viz/1_achievement.png", dpi = 300,
         units = "in", height = 3.8, width = 8.8)

  df_ach %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    mutate(share = targets / sum(targets)) %>% 
    select(fundingagency, displayname, apr, targets, share) %>% 
    arrange(share) %>% 
    mutate(share = percent(share))

# PERFORMANCE TRENDS ------------------------------------------------------

grph_trend <- function(ind, mech) {
  df_plot <- df_tza %>% 
    filter(indicator %in% ind, 
           standardizeddisaggregate == "Total Numerator",
           mechanismid == mech) %>% 
    select(-ends_with("apr"), -fy2019_targets) %>% 
    group_by(mechanismid, indicator) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, -mechanismid, -indicator, -fy2017_targets, -fy2018_targets) %>% 
    mutate(target = ifelse(str_detect(pd, "fy2017"), round(fy2017_targets/4, 0), round(fy2018_targets/4, 0))) %>% 
    select(-ends_with("_targets")) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.))
  
  df_plot %>% 
    ggplot(aes(pd, val, group = indicator)) +
    geom_line(size = 1, color = "#335B8E") +
    geom_point(aes(y = target), shape = 23, size = 5, color = "#808080", fill = "white", stroke = 2) + 
    geom_point(size = 5, color = "#335B8E") +
    expand_limits(y = 0) +
    scale_y_continuous(label = comma) +
    labs(x = "", y = "", caption = "Source: MSD FY18Q4c") +
    facet_wrap(indicator ~ . , scale = "free_y", nrow = 2) +
    theme_light() +
    theme(panel.border = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill = "#808080"),
          strip.text = element_text(size = 18, face = "bold"),
          axis.ticks = element_blank(),
          axis.text.x.bottom = element_text(),
          panel.grid.major.x = element_blank(),
          text = element_text(family = "Gill Sans MT", size = 16),
          plot.caption = element_text(size = 11, color = "#808080")
    )
}

 


#17420 KNCV

grph_trend(c("HTS_TST", "HTS_TST_POS"), "17420")
ggsave("~/COP19/Tanzania/Viz/2_trend_17420_hts.png", dpi = 300,
       units = "in", height = 4.5, width = 8.8)

#18060	EGPAF

grph_trend("TX_NEW", "18060")
ggsave("~/COP19/Tanzania/Viz/3_trend_18060_tx.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)

#18237	Deloitte


grph_trend("TX_NEW", "18237")
ggsave("~/COP19/Tanzania/Viz/4_trend_18237_tx.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)

#7103	Baylor

grph_trend("TX_NEW", "17103")
ggsave("~/COP19/Tanzania/Viz/5_trend_17103_tx.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)


# LINKAGE -----------------------------------------------------------------

df_link <- df_tza %>% 
  filter(indicator %in%  c("HTS_TST_POS","TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, mechanismid, indicator) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(fy2018apr != 0) %>% 
  spread(indicator, fy2018apr) %>% 
  left_join(dname, by = "mechanismid") %>% 
  arrange(HTS_TST_POS) %>% 
  mutate(displayname = as_factor(displayname))

df_link %>% 
  ggplot(aes(displayname, TX_NEW)) +
  #geom_errorbar(aes(ymin = HTS_TST_POS, ymax = HTS_TST_POS), color = "gray", size = 1.5) +
  geom_col(aes(y = HTS_TST_POS), fill = "#DDDDDD") +
  geom_segment(aes(xend=displayname, y=0, yend=TX_NEW),
               color = ifelse(df_link$fundingagency == "USAID", "#335B8E", "#6CA18F"), 
               size = 2, na.rm = TRUE) +
  geom_point(size = 6, color = ifelse(df_link$fundingagency == "USAID", "#335B8E", "#6CA18F"), 
             na.rm = TRUE) +
  coord_flip() +
  scale_y_continuous(label = comma) +
  facet_grid(fundingagency ~ ., scales = "free_y") +
  labs(y = "bar = HTS_POS, dot = TX_NEW", x = "", caption = "Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "#969696"),
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
  )

ggsave("~/COP19/Tanzania/Viz/6_linkage.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)

df_link %>% 
  mutate(linkage = TX_NEW/HTS_TST_POS) %>% 
  arrange(linkage) %>% 
  mutate(linkage = percent(linkage, accuracy = 1))

 df_tza %>% 
  filter(indicator %in%  c("HTS_TST_POS","TX_NEW"),
         agecoarse %in% c("<15", "15+"), 
         fundingagency %in% c("USAID", "HHS/CDC")) %>%
  mutate(sex = ifelse(agecoarse == "<15", "Unknown Sex", sex)) %>%
  unite(agesex, c(sex, agecoarse), sep = ", ") %>% 
  group_by(fundingagency, mechanismid, agesex, indicator) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(fy2018apr != 0) %>% 
  spread(indicator, fy2018apr) %>% 
  arrange(HTS_TST_POS) %>% 
  mutate(mechanismid = as_factor(mechanismid),
         link = TX_NEW/HTS_TST_POS*100) %>% 
   print(n = Inf)

# TESTING MODALITIES ------------------------------------------------------

df_mod <- df_tza %>% 
  filter(indicator == "HTS_TST_POS",
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  group_by(mechanismid, fundingagency, modality) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(fy2018apr !=0)
  

df_mod <- df_mod %>% 
  mutate(modality = ifelse(modality %in% c("Emergency Ward", "VMMC", "Pediatric",
                                           "OtherMod", "STI Clinic", "VCTMod", 
                                           "Malnutrition"), "All Other", modality)) %>% 
  group_by(fundingagency, modality) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  group_by(fundingagency) %>% 
  mutate(share = fy2018apr/sum(fy2018apr)) %>% 
  ungroup() %>% 
  mutate(mod = ifelse(str_detect(modality, "Mod"), "Comm.", "Facility"),
         modality = str_replace(modality ,"Mod", " (Comm.)"),
         fundingagency = factor(fundingagency, c("USAID", "HHS/CDC"))) 


df_mod %>%  
  ggplot(aes(reorder(modality, share), share)) +
  geom_col(fill = ifelse(df_mod$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_text(label = percent(df_mod$share, 1), 
            family = "Gill Sans MT",
            color = "#5F5F5F", hjust = -.3) +
  coord_flip() +
  #scale_y_continuous(label = percent) +
  expand_limits(y = .7) +
  facet_grid(. ~ fundingagency, scales = "free_y") +
  labs(y = "", x = "", caption = "Source: MSD FY18Q4c") +
  theme_light() +
  theme(axis.text.x = element_blank(), 
        panel.border = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080"),
        strip.background = element_rect(fill = "#969696")
  )

ggsave("~/COP19/Tanzania/Viz/7_testing_mod.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)


# INDEX TREND -------------------------------------------------------------

df_index <- df_tza %>% 
  filter(indicator %in% c("HTS_TST"),
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "Index", "nonIndex")) %>% 
  group_by(fundingagency, modality) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, starts_with("fy")) %>% 
  spread(modality, val) %>% 
  mutate(mod_share =  Index/(Index + nonIndex),
         pd = str_remove(pd, "20") %>% toupper(.),
         agency_lab = case_when(pd == "FY18Q4" ~ fundingagency))

df_index %>%  
  ggplot(aes(pd, Index, group = factor(fundingagency))) +
  geom_line(size = 1, 
            color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_point(size = 5,
             color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_text(aes(family = "Gill Sans MT", fontface =  "bold"),
            color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F"),
            label = df_index$agency_lab, 
            vjust = ifelse(df_index$fundingagency == "USAID", 1.9, -.8),
            na.rm = TRUE) +
  scale_y_continuous(labels = comma) +
  labs(y = "", x = "", caption = "NOTE: Index testing included facility and community.
       Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
        #axis.text.x = element_blank()
  )

ggsave("~/COP19/Tanzania/Viz/8_index_trend.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)

df_index <- df_tza %>% 
  filter(indicator %in% c("HTS_TST"),
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  mutate(modality = ifelse(modality %in% c("Index"), "Index", "nonIndex")) %>% 
  group_by(fundingagency, modality) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, starts_with("fy")) %>% 
  spread(modality, val) %>% 
  mutate(mod_share =  Index/(Index + nonIndex),
         pd = str_remove(pd, "20") %>% toupper(.),
         agency_lab = case_when(pd == "FY18Q4" ~ fundingagency))

df_index %>%  
  ggplot(aes(pd, Index, group = fundingagency)) +
  geom_line(size = 1,
            color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_point(size = 5,
             color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_text(aes(family = "Gill Sans MT", fontface =  "bold"),
            color = ifelse(df_index$fundingagency == "USAID", "#335B8E", "#6CA18F"),
            label = df_index$agency_lab, 
            vjust = ifelse(df_index$fundingagency == "USAID", 1.9, -.8),
            na.rm = TRUE) +
  expand_limits(y = 55000) +
  scale_y_continuous(labels = comma) +
  labs(y = "", x = "", caption = "NOTE: Index testing included facility ONLY.
       Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
        #axis.text.x = element_blank()
  )

ggsave("~/COP19/Tanzania/Viz/9_index_trend_fac.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)

df_tza %>% 
  filter(indicator %in% c("HTS_TST"),
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), modality, "nonIndex")) %>% 
  group_by(fundingagency, modality) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  group_by(fundingagency) %>% 
  mutate(share = percent(fy2018apr / sum(fy2018apr)))




# OVC ---------------------------------------------------------------------

df_ovc <- df_tza %>% 
  filter(indicator == "OVC_SERV",
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, -fundingagency) %>% 
  mutate(pd = str_remove(pd, "20") %>% toupper(.),
         fundingagency = factor(fundingagency, c("USAID", "HHS/CDC", "DOD"))) %>% 
  filter(val != 0)

df_ovc  %>% 
  ggplot(aes(pd, val, fill = factor(fundingagency))) +
  geom_col() +
  scale_fill_manual(values = c("#335B8E", "#6CA18F", "#948D79"), name = "") +
  scale_y_continuous(labels = comma) +
  labs(y = "", x = "", caption = "Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.position = "top",
        axis.ticks = element_blank(),
        axis.text.x.bottom = element_text(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
        #axis.text.x = element_blank()
  )

ggsave("~/COP19/Tanzania/Viz/10_ovc.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)



# TX_RET ------------------------------------------------------------------

df_ret <- df_tza %>% 
  filter(indicator == "TX_RET",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, mechanismid, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  gather(pd, val, starts_with("fy")) %>% 
  filter(val != 0) %>% 
  mutate(standardizeddisaggregate = str_remove(standardizeddisaggregate, "Total "),
         pd = str_remove(pd, "fy2018_|fy2018") %>% toupper(.)) %>% 
  spread(standardizeddisaggregate, val) %>% 
  mutate(ret = Numerator/Denominator) %>% 
  select(-c(Numerator, Denominator)) %>% 
  spread(pd, ret) %>% 
  mutate(tgt_low = .9*TARGETS,
         tgt_high = 1.1*TARGETS) %>% 
  left_join(dname, by = "mechanismid") %>% 
  arrange(APR) %>%
  mutate(displayname = as_factor(displayname),
         pct_lab = percent(APR, accuracy = 1))

df_ret %>% 
  ggplot(aes(displayname, APR)) +
  geom_errorbar(aes(ymin = tgt_low, ymax = tgt_high),
                color = "#B2B2B2",
                width = .75) +
  geom_point(size = 13, shape = 21,
             fill = "white", stroke = 2,
             color = ifelse(df_ret$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_text(label = df_ret$pct_lab, 
            family = "Gill Sans MT") +
  coord_flip() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent) +
  facet_grid(fundingagency ~ ., scales = "free_y") +
  labs(y = "", x = "", caption = "Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
  )

ggsave("~/COP19/Tanzania/Viz/11_txret.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)



df_vl <- df_tza %>% 
  filter(indicator == "TX_PVLS",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, mechanismid, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  gather(pd, val, starts_with("fy")) %>% 
  filter(val != 0) %>% 
  mutate(standardizeddisaggregate = str_remove(standardizeddisaggregate, "Total "),
         pd = str_remove(pd, "fy2018_|fy2018") %>% toupper(.)) %>% 
  spread(standardizeddisaggregate, val) %>% 
  mutate(ret = Numerator/Denominator) %>% 
  select(-c(Numerator, Denominator)) %>% 
  spread(pd, ret) %>% 
  mutate(tgt_low = .9*TARGETS,
         tgt_high = 1.1*TARGETS) %>% 
  left_join(dname, by = "mechanismid") %>% 
  arrange(fundingagency, APR) %>%
  mutate(displayname = as_factor(displayname),
         pct_lab = percent(APR, accuracy = 1))

df_vl %>% 
  ggplot(aes(displayname, APR)) +
  geom_errorbar(aes(ymin = tgt_low, ymax = tgt_high),
                color = "#B2B2B2",
                width = .75) +
  geom_point(size = 13, shape = 21,
             fill = "white", stroke = 2,
             color = ifelse(df_vl$fundingagency == "USAID", "#335B8E", "#6CA18F")) +
  geom_text(label = df_vl$pct_lab, 
            family = "Gill Sans MT") +
  coord_flip() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent) +
  facet_grid(fundingagency ~ ., scales = "free_y") +
  labs(y = "", x = "", caption = "Source: MSD FY18Q4c") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
  )

ggsave("~/COP19/Tanzania/Viz/12_txvl.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)


# PHIA --------------------------------------------------------------------

df_phia <- read_csv("~/COP19/Tanzania/PHIA_Results.csv")

df_phia <- df_phia %>% 
  spread(goal90s, pct) %>% 
  arrange(`1st`) %>% 
  mutate(country = as_factor(country)) %>% 
  gather(goal90s, pct, -country) %>% 
  arrange(country, goal90s) %>% 
  mutate(goal90s = as_factor(goal90s),
         bar_col = case_when(country == "Tanzania" & goal90s == "1st" ~ "#CC5234",
                             goal90s == "1st" ~ "#e09785",
                             goal90s == "2nd" ~ "#969696",
                             TRUE             ~ "#B2B2B2")) %>% 
  arrange(country, desc(goal90s))

df_phia %>% 
  ggplot(aes(country, pct, group = goal90s)) +
  geom_col(position = "dodge",
           fill = df_phia$bar_col) +
  geom_text(label =df_phia$pct,
            position = position_dodge(width = 1),
            vjust = -.5,
            family = "Gill Sans MT") +
  expand_limits(y = c(0,105)) +
  labs(y = "", x = "", caption = "Source: PHIA Results from COP19 Guidance") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 16),
        plot.caption = element_text(size = 11, color = "#808080")
  )

ggsave("~/COP19/Tanzania/Viz/13_phia.png", dpi = 300,
       units = "in", height = 3.8, width = 8.8)
