library(tidyverse)
library(scales)
library(gridExtra)

df_msd <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20181221_v2_1.rds")

df_tza <- df_msd %>% 
  filter(operatingunit == "Tanzania")

hts <- df_tza %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(operatingunit, indicator) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  gather(pd, val, -operatingunit,-indicator) %>% 
  ungroup() %>% 
  mutate(fy = str_sub(pd, 3, 6),
         pd = str_remove(pd, "20") %>% toupper(.)) %>% 
  arrange(indicator, pd)

hts_wide <- hts %>% 
  spread(indicator, val) %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST)



viz_pos <- hts_wide %>% 
  ggplot(aes(pd, HTS_TST_POS, group = factor(fy), color = factor(fy))) +
  geom_point(size = 6) + 
  geom_line(size = 1) + 
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FYQ17Q1", "", "FY17Q3", "", "FYQ18Q1", "", "FY18Q3", "")) +
  scale_color_manual(values = c("#D9812C", "#CC5234")) +
  labs(x = "", y = "", title = "POSITIVE TESTS") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "gray"),
        legend.position = "none")


#"Tanzania saw a major increase in testing in FY1"
viz_hts <- hts_wide %>% 
  ggplot(aes(pd, HTS_TST, group = factor(fy), color = factor(fy))) +
  geom_point(size = 6) + 
  geom_line(size = 1) + 
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FYQ17Q1", "", "FY17Q3", "", "FYQ18Q1", "", "FY18Q3", "")) +
  scale_color_manual(values = c("#D9812C", "#CC5234")) +
  labs(x = "", y = "", title = "TESTS") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "gray"),
        legend.position = "none")


#Increase in tests and lower positives leads to lower yields
viz_yield <- hts_wide %>% 
  ggplot(aes(pd, positivity, group = factor(fy), color = factor(fy))) +
  geom_point(size = 6) + 
  geom_line(size = 1) + 
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("FYQ17Q1", "", "FY17Q3", "", "FYQ18Q1", "", "FY18Q3", "")) +
  scale_color_manual(values = c("#D9812C", "#CC5234")) +
  labs(x = "", y = "", title = "POSITIVITY") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "gray"),
        legend.position = "none")


output <- arrangeGrob(viz_pos, viz_hts, viz_yield, nrow = 1, ncol = 3)

ggsave("~/COP19/Tanzania/Testing/tnz_trend.png", output, height = 3, width = 11.5, units = "in", dpi = 300)
