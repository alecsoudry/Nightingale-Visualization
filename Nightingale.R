load("Nightingale.RData")
library(tidyverse)

Nightingale %>% 
  select(Date, Month, Year, contains("rate")) %>% 
  pivot_longer(cols = 4:6, names_to = "Cause", values_to = "Rate") %>% 
  mutate(Cause = gsub(".rate", "", Cause),
         period = ifelse(Date <= as.Date("1855-03-01"), "April 1855 to March 1856", "April 1854 to March 1855"),
         Month = fct_relevel(Month, "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
  arrange(desc(Rate)) %>% 
  ggplot(aes(Month, Rate)) + 
  geom_col(aes(fill = Cause), width = 1, position = "identity", alpha=0.8) + 
  coord_polar() + 
  facet_wrap(~period) +
  scale_fill_manual(values = c("lightblue3", "grey70", "lavenderblush1")) +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "right",
        plot.background = element_rect(fill = alpha("beige", 0.2)),
        plot.margin = unit(c(10, 10, 10, 10), "pt"),
        plot.title = element_text(hjust = 0.5, vjust = 4)) +
  labs(title = "Diagram of the Causes of Mortality in the Army in the East")

Nightingale %>% 
  select(Date, Month, Year, contains("rate")) %>% 
  pivot_longer(cols = 4:6, names_to = "Cause", values_to = "Rate") %>% 
  mutate(Cause = gsub(".rate", "", Cause)) %>% 
  ggplot(aes(Date, Rate)) + 
  geom_rect(xmin = as.numeric(as.Date("1854-04-01")), 
            xmax = as.numeric(as.Date("1855-03-01")), 
            ymin = 0, ymax = Inf, fill = "grey85") +
  geom_col(aes(fill = Cause)) + 
  scale_fill_manual(values = c("lightblue3", "grey65", "lavenderblush1")) +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "right",
        plot.background = element_rect(fill = alpha("beige", 0.2)),
        plot.margin = unit(c(10, 10, 10, 10), "pt"),
        plot.title = element_text(hjust = 0.5, vjust = 4)) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Diagram of the Causes of Mortality in the Army in the East")