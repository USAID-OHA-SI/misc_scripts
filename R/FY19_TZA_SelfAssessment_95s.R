## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Visualize trends for the 90s indicators
## DATE:     2019-11-26
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(gridExtra)


# IMPORT DATA -------------------------------------------------------------

  df_mer <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE) %>% 
    read_rds()


# PARTNER TABLE -----------------------------------------------------------

df_mech_map <- tibble::tribble(
    ~mech_code,     ~partner,
         "17420", "Challenge TB",
         "18060",        "EGPAF",
         "16784",      "JHPIEGO",
         "16787",          "JSI",
         "12854",     "Vodafone",
         "18237",     "Deloitte",
         "17103",       "Baylor",
         "70356",       "Baylor",
         "17409",      "JHPIEGO"
    )
  
# MUNGE -------------------------------------------------------------------

  df_tza <- df_mer %>% 
    filter(operatingunit == "Tanzania",
           fundingagency == "USAID",
           indicator %in% c("HTS_TST", "HTS_TST_POS", 
                            "TX_CURR", "TX_NET_NEW",
                            "TX_PVLS"),
           standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                           "Modality/Age/Sex/Result",
                                           "Age Aggregated/Sex/HIVStatus",
                                           "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")) %>% 
    left_join(df_mech_map, by = "mech_code") %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
           age = case_when(ageasentered %in% c("<01", "<05", "<10", "<15", 
                                               "01-04", "01-09", "05-09", 
                                               "05-14", "10-14")           ~ "<15",
                           ageasentered %in% c("15-19", "15+", "20-24", 
                                               "20+", "25-29", "25-49", 
                                               "30-34", "35-39", "40-44", 
                                               "40-49", "45-49", "50+")    ~ "15+",
                           TRUE                                            ~ ageasentered),
           sex = ifelse(age == "<15", "Peds", sex),
           modality = case_when(str_detect(modality, "Index") ~ "Index",
                                 !indicator %in% c("HTS_TST", "HTS_TST_POS") ~ as.character(NA),
                                 TRUE ~ "Non-Index")) %>% 
    group_by(fiscal_year, partner, indicator, age, sex, modality) %>% #snu1
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup() %>%
    reshape_msd("long") %>% 
    mutate(period = str_remove(period, "20") %>% toupper) %>% 
    unite(agesex, c(sex, age), sep = " ")
  

df_viz <- df_tza %>% 
  filter(str_detect(period, "Q")) %>% 
  spread(indicator, val) %>% 
  group_by(period, partner, agesex) %>% #snu1
  mutate(HTS_POS_Share = HTS_TST_POS/sum(HTS_TST_POS, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(partner, agesex, period) %>% #snu1
  group_by(partner, agesex) %>% #snu1
  mutate(`VL Coverage` = TX_PVLS_D / lag(TX_CURR, 2),
         `VL Suppression` = TX_PVLS / TX_PVLS_D) %>% 
  ungroup() %>% 
  gather(indicator, val, -period:-modality)

df_viz_yield <- df_viz %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  group_by(period, partner, agesex, indicator) %>% #snu1
  summarise_at(vars(val), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, val) %>% 
  mutate(`Positivity` = HTS_TST_POS/HTS_TST) %>% 
  gather(indicator, val, -period:-agesex) %>% 
  filter(indicator == "Positivity")
  
df_viz <- bind_rows(df_viz, df_viz_yield)


# Test Pos ----------------------------------------------------------------

df_viz %>% 
  group_by(period, partner, agesex, indicator) %>% #snu1
  summarise_at(vars(val), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(indicator == "HTS_TST_POS",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         partner != "Vodafone",
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ comma(val))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, fill = agesex)) +
  geom_col(na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, switch = "y") +
  scale_y_continuous(labels = comma, position = "right") +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Test Positive",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("../Downloads/TZA_SelfAssessment_Pos.png", dpi = 300,
       height = 5.6, width = 10, units = "in")

# Positivity --------------------------------------------------------------

df_viz %>% 
  filter(indicator == "Positivity",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         partner != "Vodafone",
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ percent(val, .1))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, color = agesex)) +
  geom_line(size = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, switch = "y") +
  scale_y_continuous(labels = percent_format(1), position = "right") +
  scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Positivity",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))
  
ggsave("../Downloads/TZA_SelfAssessment_Positivity.png", dpi = 300,
       height = 5.6, width = 10, units = "in")


# Test Index ----------------------------------------------------------------

df_viz %>% 
  group_by(period, partner, agesex, indicator, modality) %>% #snu1
  summarise_at(vars(val), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(indicator == "HTS_TST",
         modality == "Index",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         partner != "Vodafone",
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ comma(val))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, fill = agesex)) +
  geom_col(na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, switch = "y") +
  scale_y_continuous(labels = comma, position = "right") +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Index Tests",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("../Downloads/TZA_SelfAssessment_IndexTests.png", dpi = 300,
       height = 5.6, width = 10, units = "in")

# Index Share -------------------------------------------------------------

df_viz %>% 
  filter(indicator == "HTS_POS_Share",
         modality == "Index",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         partner != "Vodafone",
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ percent(val, 1))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, color = agesex)) +
  geom_blank(aes(y = 1.1 * val)) +
  geom_line(size = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, switch = "y") +
  scale_y_continuous(labels = percent_format(1), position = "right") +
  scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Index Share",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))
  
ggsave("../Downloads/TZA_SelfAssessment_PosIndexShare.png", dpi = 300,
       height = 5.6, width = 10, units = "in")


# TX_CURR -----------------------------------------------------------------

df_viz %>% 
  filter(indicator == "TX_CURR",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         !partner %in% c("Vodafone", "Challenge TB", "JHPIEGO"),
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ comma(val))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, fill = agesex)) +
  geom_col(na.rm = TRUE) +
  geom_blank(aes(y = 1.1 * val)) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, switch = "y") +
  scale_y_continuous(labels = comma, position = "right") +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Current on Treatment",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("../Downloads/TZA_SelfAssessment_TX_CURR.png", dpi = 300,
       height = 5.6, width = 10, units = "in")

# NET_NEW -----------------------------------------------------------------


df_viz %>% 
  filter(indicator == "TX_NET_NEW",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         !partner %in% c("Vodafone", "Challenge TB", "JHPIEGO"),
         !is.na(partner)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ comma(val))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, fill = agesex)) +
  geom_col(na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 3, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex, scales = "free_y", switch = "y") +
  scale_y_continuous(labels = comma, position = "right") +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in Net New on Treatment",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("../Downloads/TZA_SelfAssessment_NetNew.png", dpi = 300,
       height = 5.6, width = 10, units = "in")

# VL ----------------------------------------------------------------------


df_viz_vl <- df_tza %>% 
  filter(str_detect(period, "Q"),
         str_detect(agesex, "Unknown Age", negate = TRUE),
         indicator %in% c("TX_PVLS", "TX_PVLS_D", "TX_CURR")) %>% 
  spread(indicator, val) %>% 
  arrange(partner, agesex, period) %>% 
  group_by(partner, agesex) %>% #snu1
  mutate(`VL Coverage` = TX_PVLS_D / lag(TX_CURR, 2),
         `VL Suppression` = TX_PVLS / TX_PVLS_D) %>% 
  ungroup() %>% 
  gather(indicator, val, -period:-modality)

df_viz_vl %>% 
  filter(indicator %in% c("VL Coverage", "VL Suppression"),
         str_detect(agesex, "Unknown Age", negate = TRUE)) %>%
  mutate(lab = case_when(str_detect(period, "Q4") ~ percent(val, 1))) %>% 
  arrange(partner, agesex, period) %>% 
  ggplot(aes(period, val, group = partner, color = agesex)) +
  geom_line(size = 1, na.rm = TRUE) +
  geom_point(size = 3.5, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "gray50") +
  geom_text(aes(label = lab), color = "gray30",
            size = 2.5, vjust = -1,
            family = "Gill Sans MT", na.rm = TRUE) +
  facet_grid(partner ~ agesex + indicator, switch = "y") +
  scale_y_continuous(labels = percent_format(1), position = "right") +
  scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_x_discrete(labels = c("FY17", "", "", "",
                              "FY18", "", "", "",
                              "FY19", "", "", "")) +
  labs(x = NULL, y = NULL,
       title = "USAID Partner Trends in VL Coverage & Suppression",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 11, face = "bold"))

ggsave("../Downloads/TZA_SelfAssessment_VL.png", dpi = 300,
       height = 5.6, width = 10, units = "in")
