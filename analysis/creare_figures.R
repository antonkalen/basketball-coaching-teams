
# Load packages -----------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(tidytext)
library(hrbrthemes)


# Set parameters ----------------------------------------------------------

theme_set(
  theme_ipsum(
    axis = FALSE,
    grid = FALSE,
    grid_col = "Gray50",
    plot_margin = margin(5, 5, 5, 5),
    base_size = 9,
    strip_text_size = 10,
    subtitle_size = 10,
    subtitle_margin = 10
  ) +
  theme(
    panel.border = element_rect(color = "Gray50", fill = NA),
    strip.text = element_text(vjust = 0, margin = margin(0,0,4,0)),
    panel.spacing.x = unit(.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
  )
)

## Color scale
col_scale <- c("Höjning" = "#1b9e77", "Samma" = "#7570b3", "Sänkning" = "#d95f02")

# Read data ----
pre_post_data_raw <- read_csv(here("data/season1_prepost.csv"))

data_dict <- read_csv(here("data/basketball_season1_prepost_codebook.csv")) |> 
  select(human_name, variable) |> 
  tibble::deframe()

# Prepare data ----


## Post data ----
post_data <- pre_post_data_raw |> 
  filter(time == "post") |> 
  drop_na(shared_leadership) |> 
  group_by(staff) |> 
  filter(n() > 1)



# Syn på delat ledarskap --------------------------------------------------

(fig1 <- post_data |> 
  pivot_longer(
    c(
      starts_with("shared_leadership"), 
      -(shared_leadership_6:shared_leadership_9))
  ) |> 
  mutate(
    name = fct_recode(name, !!!data_dict)
  ) |> 
ggplot(aes(x = staff, y = value)) +
    stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
    stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
    scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
    facet_wrap(~name, ncol = 3, labeller = label_wrap_gen(width = 42, multi_line = TRUE), scales = "free_x") +
    labs(
      x = NULL,
      y = NULL
    ))

ggsave(
  "figure_1.png",
  fig1,
  path = here("output"),
  scale = 1,
  width = 16,
  height = 12,
  units = "cm"
)  


# Roll i staben -----------------------------------------------------------


(fig2 <- post_data |> 
  pivot_longer(starts_with("role")) |> 
  mutate(name = fct_recode(name, !!!data_dict)) |> 
  ggplot(aes(x = staff, y = value)) +
  stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
  stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
  scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
  facet_wrap(~name, ncol = 4, labeller = label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(
    x = NULL,
    y = NULL
  ))

ggsave(
  "figure_2.png",
  fig2,
  path = here("output"),
  scale = 1,
  width = 16,
  height = 5,
  units = "cm"
)  


# Sammarbete --------------------------------------------------------------

(fig3 <- post_data |> 
   pivot_longer(starts_with("collaboration")) |> 
   mutate(
     name = fct_recode(name, !!!data_dict)
   ) |> 
   ggplot(aes(x = staff, y = value)) +
   stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
   stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
   scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
   facet_wrap(~name, ncol = 3, labeller = label_wrap_gen(width = 38, multi_line = TRUE), scales = "free_x") +
   labs(
     x = NULL,
     y = NULL
   ))

ggsave(
  "figure_3.png",
  fig3,
  path = here("output"),
  scale = 1,
  width = 16,
  height = 12,
  units = "cm"
)  

# Psychological sefety --------------------------------------------------------------

(fig4 <- post_data |> 
   pivot_longer(psych_safety) |> 
   mutate(
     name = fct_recode(name, !!!data_dict)
   ) |> 
   ggplot(aes(x = staff, y = value)) +
   stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
   stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
   ylim(1,5) +
   scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
   facet_wrap(~name, ncol = 3, labeller = label_wrap_gen(width = 38, multi_line = TRUE), scales = "free_x") +
   labs(
     x = NULL,
     y = NULL
   ))

ggsave(
  "figure_4.png",
  fig4,
  path = here("output"),
  scale = 1,
  width = 6,
  height = 6,
  units = "cm"
)  

# Transformational leadership --------------------------------------------------------------

(fig5 <- post_data |> 
   pivot_longer(transformational_leadership) |> 
   mutate(
     name = fct_recode(name, !!!data_dict)
   ) |> 
   ggplot(aes(x = staff, y = value)) +
   stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
   stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
   ylim(1,5) +
   scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
   facet_wrap(~name, ncol = 3, labeller = label_wrap_gen(width = 38, multi_line = TRUE), scales = "free_x") +
   labs(
     x = NULL,
     y = NULL
   ))

ggsave(
  "figure_5.png",
  fig5,
  path = here("output"),
  scale = 1,
  width = 6,
  height = 6,
  units = "cm"
)  



# Basic needs -------------------------------------------------------------

(fig6 <- post_data |> 
   pivot_longer(starts_with("basic_needs")) |> 
   mutate(
      name = fct_recode(
        name,
        Autonomi = "basic_needs_autonomy",
        Kompetens = "basic_needs_competence",
        Tillhörighet = "basic_needs_relatedness"
      )
   ) |> 
   ggplot(aes(x = staff, y = value)) +
   stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
   stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
   scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
   facet_wrap(~name, ncol = 3, labeller = label_wrap_gen(width = 38, multi_line = TRUE), scales = "free_x") +
   labs(
     x = NULL,
     y = NULL
   ))

ggsave(
  "figure_6.png",
  fig6,
  path = here("output"),
  scale = 1,
  width = 16,
  height = 6,
  units = "cm"
)  



# Relation ----------------------------------------------------------------

library(corrplot)

cor_mat <- pre_post_data_raw |> 
  filter(time == "post") |> 
  select(
    "Delat ledarskap" = shared_leadership, 
    "Roll" = role, 
    "Samarbete" = collaboration, 
    "Psykologisk trygghet" = psych_safety, 
    "Transformational leadership" = transformational_leadership,
    Autonomi = basic_needs_autonomy,
    Kompetens = basic_needs_competence,
    Tillhörighet = basic_needs_relatedness
  ) |> 
  cor(use = "pairwise.complete.obs")

png("figure_7.png", width = 16, height = 16, units = "cm", res = 300)

  corrplot(
    cor_mat,
    method = 'shade', 
    type = "upper", 
    order = 'FPC', 
    addCoef.col = 'white',
    tl.col = "Gray30",
    number.digits	= 1
  )
dev.off()
