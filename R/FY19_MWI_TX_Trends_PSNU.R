library(tidyverse)
library(ICPIutilities)
library(scales)

df <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds()


df_mwi <- df %>% 
  filter(operatingunit == "Malawi",
         indicator == "TX_CURR",
         fundingagency %in% c("USAID", "HHS/CDC"),
         standardizeddisaggregate == "Total Numerator")

df_mwi <- df_mwi %>% 
  count(fundingagency, fiscal_year, psnu, wt = targets, name = "targets")

df_mwi <- df_mwi %>% 
  mutate(psnu = str_remove(psnu, " District"),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fiscal_year = fiscal_year - 2000,
         targets_fy20 = ifelse(fiscal_year == 20, targets, 0),
         psnu = fct_reorder(psnu, targets_fy20, .fun = sum, .desc = TRUE))
  
df_mwi %>% 
  filter(targets > 0) %>% 
  ggplot(aes(fiscal_year, targets, color = fundingagency)) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_line(size = 1) +
  geom_point(size = 4) + 
  scale_y_continuous(label = comma) +
  scale_color_manual(values = c("#A7C6ED","#0067B9")) +
  facet_wrap(. ~ psnu) + 
  labs(x = NULL, y = NULL,
       subtitle = "MWI Trends in Agency TX_CURR Targets",
       caption = "FY19Q4i MSD",
       color = "Agency") +
  theme_minimal()
