## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  MWI trends in Linakge
## DATE:     2019-11-20
## UPDATED:  2019-11-27

#dependencies
library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)

#import
path <- list.files("~/ICPI/Data", "OU_IM", full.names = TRUE)

df_mer <- read_rds(path)

df_link <- df_mer %>% 
  filter(operatingunit == "Malawi",
         fundingagency == "USAID",
         mech_name != "LINKAGES",
         indicator %in% c("HTS_TST_POS", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator") %>%
  mutate(mech_name = case_when(mech_name == "Baylor TSP" ~ "Baylor",
                           mech_name %in% c("EQUIP","OHSEC") ~ "EQUIP/PIH",
                           TRUE ~ mech_name)) %>%
  group_by(mech_name, indicator, fiscal_year) %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(mech_name, indicator, period) %>% 
  spread(indicator, val) %>% 
  mutate(`Proxy Linkage` = TX_NEW/HTS_TST_POS,
         period = str_remove(period, "20") %>% toupper) %>% 
  filter(!is.na(`Proxy Linkage`)) %>% 
  gather(indicator, val, HTS_TST_POS, TX_NEW, `Proxy Linkage`) %>% 
  mutate(indicator = factor(indicator, c("HTS_TST_POS", "TX_NEW", "Proxy Linkage")),
         max = case_when(indicator == "Proxy Linkage" ~ val * 1.2,
                         TRUE ~ max(val) * 1.1),
         lab = case_when(indicator == "Proxy Linkage" & str_detect(period, "Q(2|4)") ~ percent(val,2),
                         str_detect(period, "Q(2|4)") ~ comma(val)),
         mech_name = factor(mech_name, c("EQUIP/PIH", "Baylor")))

df_link %>% 
  ggplot(aes(period, val, group = mech_name, color = mech_name)) +
  geom_hline(yintercept = 0) +
  geom_blank(aes(y = max)) +
  geom_line(size = 1.5, na.rm = TRUE) +
  geom_point(size = 6, na.rm = TRUE) +
  geom_text(aes(label = lab), na.rm = TRUE, family = "Gill Sans MT",
            vjust = -1,  color = "gray30") +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  scale_color_manual(values = c("#335B8E", "#739bcc")) + #"#26456a",
  labs(x = NULL, y = NULL, caption = "Note: Disaggregate = Total Numerator,
       Source: FY19Q4i MSD") +
  facet_grid(indicator ~ mech_name, scales = "free_y",switch = "y") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.caption = element_text(color = "gray30"),
        strip.text = element_text(size = 12, face = "bold"),
        strip.background.x = element_rect(fill = "gray90", color = "gray90"))

ggsave("C:/Users/achafetz/Downloads/MWI_Linkage.png", dpi = 300,
       height = 5.6, width = 10, units = "in")
