## PROJECT:  USAID VMMC Targets
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  pre-release FY20 target share
## DATE:     2019-08-19


library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)
library(gridExtra)

df <- match_msd("../Downloads/PEPFAR-Data-Genie-OUByIMs-2019-08-16.zip", save_rds = FALSE)

vmmc <- df %>% 
  filter(standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year == "2020") %>% 
  mutate(agency = ifelse(fundingagency == "USAID", fundingagency, "Other Agencies")) %>% 
  group_by(operatingunit, agency, indicator) %>% 
  summarise_at(vars(targets), sum, na.rm = TRUE) %>% 
  group_by(operatingunit) %>% 
  mutate(total = sum(targets),
         share = targets / total) %>% 
  ungroup() %>% 
  mutate(operatingunit = toupper(operatingunit)) %>% 
  filter(agency == "USAID") %>% 
  select(-total)

theme_set(theme_light(base_size = 11, base_family = "Gill Sans MT"))

v_tot <- vmmc %>% 
  ggplot(aes(fct_reorder(operatingunit, targets), targets, fill = indicator)) + 
  geom_hline(aes(yintercept = 0), color = "gray30", size = .5) +
  geom_col() +
  geom_text(aes(label = comma(targets)),
            family = "Gill Sans MT", size = 3,
            color = "gray30",
            hjust = -.2) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "USAID VMMC FY20 TARGETS",
       caption = " ") +
  scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, 380000)) +
  scale_fill_manual(values = "#16a085") + 
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 11),
        plot.caption = element_text(size = 9, color = "gray50"),
        legend.position = "none"
        )

v_share <- vmmc %>% 
  ggplot(aes(share, fct_reorder(operatingunit, targets), color = indicator)) + 
  geom_vline(aes(xintercept = 0), color = "gray30", size = .5) +
  geom_segment(aes(x = 0, xend = share, yend = operatingunit), size = 1.5) + 
  geom_point(size = 5) +
  geom_text(aes(label = percent(share, 1)),
            family = "Gill Sans MT", size = 3,
            color = "gray30",
            hjust = -.7) +
  labs(x = NULL, y = NULL,
       title = "USAID FY20 TARGET SHARE",
       caption = "Data: DATIM Genie 2019-08-19, aggregated Age/Sex/HIVStatus") +
  scale_x_continuous(expand = c(0.005, 0.005), limits = c(0, 1.2),
                     breaks = seq(0, 1.1, .2)) +
  scale_color_manual(values = "#8e44ad") +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        #axis.text.x = element_blank(),
        #axis.text = element_text(size = 15),
        axis.text = element_blank(),
        plot.caption = element_text(size = 8, color = "gray50"),
        legend.position = "none"
  )

v <- grid.arrange(v_tot, v_share, nrow = 1)

ggsave("../Downloads/FY20_VMMC_USAID_Targets.png", v, dpi = 300)

