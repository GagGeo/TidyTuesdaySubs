# Pkgs
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggforce)

# DataImport
emperors <- read.csv("~/Projects/TidyTuesdays/2019-08-13/emperors.csv")

# Wrangling
emperors <- emperors %>%
  # New Vars
  mutate(reign_duration = time_length(interval(start = reign_start,
                                               end = reign_end),
                                      unit = "year"),
         age = time_length(x = interval(start = birth,
                                        end = death),
                           unit = "year"),
         age_start = time_length(interval(start = birth, end = reign_start),
                                 unit = "year")) %>%
  # Fixing
  mutate_at(.vars = c("reign_duration", "age", "age_start"),
            .funs = function(x){ifelse(x < 0, -x, x)}) %>%
  # Means
  mutate(reign_mean = mean(reign_duration, na.rm = TRUE)) %>%
  group_by(cause) %>%
  mutate(reign_mean_cause = mean(reign_duration, na.rm = TRUE)) %>%
  ungroup %>%
  group_by(rise) %>%
  mutate(reign_mean_rise = mean(reign_duration, na.rm = TRUE)) %>%
  ungroup

# Plots

p1 <- emperors %>%
  ggplot(aes(x = reign_duration, y = age_start)) +
  geom_point(aes(col = rise)) +
  geom_smooth(method = "lm", fill = "green", col = "green", alpha = 0.2) +
  theme_minimal() +
  labs(title = "Age Start vs Reign Duration",
       x = "Age Start",
       y = "Reign Duration") +
  scale_color_discrete(name = "Rise") +
  theme(title=element_text(face="bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom")

p2 <- emperors %>% ggplot() +
  geom_jitter(aes(x = reign_duration, y = rise, col = rise, size=5, alpha = 0.25)) +
  geom_mark_circle(y=5,x=30.8, color='grey50', label.fill = NA, expand = unit(6, "mm"))+
  scale_x_continuous(limits=c(0,31))+
  geom_segment(aes(y = rise, yend = rise, x = reign_mean_rise, xend = reign_mean), size = 0.5) +
  geom_point(aes(x = reign_mean_rise, y = rise, fill = rise), color="gray30", shape=21, size=7, stroke=1) + 
  geom_vline(xintercept = emperors$reign_mean) +
  annotate("text", x = 25, y = 4, fontface="bold", label = "Constantine the Great")+
  geom_curve(aes(y = 4.1, yend = 4.8, x = 25, xend = 30), arrow = arrow(length = unit(0.07, "inch")), size = 0.6,
             color = "gray20", curvature = -0.25) +
  theme_minimal() + guides(color = FALSE, fill = FALSE, size = FALSE, alpha = FALSE)+
  theme(title=element_text(face="bold"),
        plot.caption = element_text(face = "italic")) +
  labs(
    title="Reign Duration by Rise",
    x= "Reign Duration (Years)",
    y= "Rise")
  
p3 <- emperors %>% select(rise, cause) %>%
  group_by(rise) %>% count(cause) %>%
  ggplot(aes(x = cause, y = n, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + coord_flip() + facet_wrap(~rise, scales = "free") +
  theme(title=element_text(face="bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "bottom") +
  labs(
    title="Causes of Death by Rise",
    x= "Causes of Death",
    y= "N",
    caption = "TidyTuesday Week 33, Source = Wikipedia Zonination") +
  scale_fill_discrete(name = "Causes")

p.all <- arrangeGrob(p1, p2, p3, ncol = 3)

ggsave(filename = "emperors.png", plot = p.all, width = 20, height = 10)

# emperors %>% select(rise, cause) %>%
#   group_by(rise) %>% count(cause) %>%
#   ggplot(aes(x = rise, y = n, fill = cause)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_minimal() + coord_flip()

# emperors %>% ggplot() +
#   geom_jitter(aes(x = reign_duration, y = cause, col = cause, size=5, alpha = 0.25)) +
#   scale_x_continuous(limits=c(0,31))+
#   geom_segment(aes(y = cause, yend = cause, x = reign_mean_cause, xend = reign_mean), size = 0.5) +
#   geom_point(aes(x = reign_mean_cause, y = cause, fill = cause), color="gray30", shape=21, size=7, stroke=1) + 
#   geom_vline(xintercept = emperors$reign_mean) +
#   theme_minimal() + guides(color = FALSE, fill = FALSE, size = FALSE, alpha = FALSE)+
#   theme(title=element_text(face="bold"),
#         plot.subtitle=element_text(face="italic"),
#         plot.caption = element_text(face = "italic")) +
#   labs(
#     title="Reign Duration by Causes of Death",
#     subtitle="Roman Emperors",
#     x= "Reign Duration (Years)",
#     y= "Causes of Death",
#     caption = "TidyTuesday Week 33, Source = Wikipedia Zonination")

