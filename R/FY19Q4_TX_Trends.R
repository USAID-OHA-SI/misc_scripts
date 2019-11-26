##  Project:  FY19Q4 TREATMENT TRENDS
##  Author:   Aaron Chafetz, USAID
##  Purpose:  review agency/OU trends in TX_CURR and TX_NEW
##  Date:     2019-11-25

library(vroom)
library(tidyverse)

df_hfr <- vroom::vroom("~/GitHub/Wavelength/out/joint/HFR_GLOBAL_thru2020.01_output_20191123.0801.csv",
             delim = ",", col_types = c(.default = "c"))

df_hfr %>% 
  filter(val > 0,
         hfr_pd == "13") %>% 
  distinct(orgunit)  

library(ICPIutilities)

df_ouim <- read_msd(list.files("~/ICPI/Data", "OU_IM", full.names = TRUE))

df_ouim_tx <- df_ouim %>% 
  filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")
         ) %>% 
  group_by(fundingagency, fiscal_year, indicator) %>%
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  mutate(period = str_remove(period, "20") %>% toupper,
         val_m = val/1000000,
         lab = case_when(str_detect(period, "Q4") ~ round(val_m, 1) %>% paste0("m")),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC")))

df_ouim_tx %>% 
  filter(str_detect(period, "Q")) %>% 
  ggplot(aes(period, val_m, fill = fundingagency)) +
  geom_blank(aes(y = 1.15 * val_m)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = lab), vjust = -1, family = "Gill Sans MT", color = "gray30", na.rm = TRUE) +
  facet_grid(indicator ~ fundingagency, scales = "free_y") +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  scale_fill_manual(values = c("#0067B9", "#A7C6ED")) + 
  labs(x = NULL, y = "patients (millions)", 
       title = "AGENCY TRENDS IN CURRENT ON TREATMENT",
       subtitle = "",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT", size = 13),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 16, face = "bold"),
        plot.title = element_text(face = "bold"))
    
ggsave("../Downloads/FY19Q4_TX_Trends_Agency.png",
       dpi = 300, units = "in", width = 10, height = 5.6)

q4_mechs <- df_ouim %>% 
  filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(operatingunit, fundingagency, mech_code, mech_name, primepartner, fiscal_year, indicator) %>%
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(q4_only = (qtr1 + qtr2 + qtr3 == 0) & qtr4 > 0) %>% 
  filter(q4_only == TRUE) %>% 
  distinct(mech_code, fiscal_year)


df_ouim_tx2 <- df_ouim %>% 
  filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")
  ) %>% 
  anti_join(q4_mechs) %>% 
  group_by(fundingagency, fiscal_year, indicator) %>%
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  mutate(period = str_remove(period, "20") %>% toupper,
         val_m = val/1000000,
         lab = case_when(str_detect(period, "Q4") ~ round(val_m, 1) %>% paste0("m")),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC")))


df_ouim_tx2 %>% 
  filter(str_detect(period, "Q")) %>% 
  ggplot(aes(period, val_m, fill = fundingagency)) +
  geom_blank(aes(y = 1.15 * val_m)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = lab), vjust = -1, family = "Gill Sans MT", color = "gray30", na.rm = TRUE) +
  facet_grid(indicator ~ fundingagency, scales = "free_y") +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  scale_fill_manual(values = c("#0067B9", "#A7C6ED")) + 
  labs(x = NULL, y = "patients (millions)", 
       title = "AGENCY TRENDS IN CURRENT ON TREATMENT",
       subtitle = "removing Q4 only reporting mechanisms",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT", size = 13),
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 16, face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("../Downloads/FY19Q4_TX_Trends_Agency2.png",
       dpi = 300, units = "in", width = 10, height = 5.6)
