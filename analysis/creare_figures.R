
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
    base_size = 14,
    strip_text_size = 20,
    subtitle_size = 20,
    subtitle_margin = 0
  ) +
  theme(
    panel.border = element_rect(color = "Gray50", fill = NA),
    strip.text = element_text(vjust = 0, margin = margin(0,0,5,0)),
    panel.spacing.x = unit(1.5, "lines"),
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


# Change

pre_post_data_clean <- pre_post_data_raw |> 
  arrange(participant_id, desc(time)) |> 
  group_by(participant_id) |> 
  mutate(
    time = factor(time, levels = c("pre", "post"), labels = c("Före", "Efter")),
    across(
      c(shared_leadership, role, collaboration, psych_safety,
        transformational_leadership, basic_needs_autonomy, 
        basic_needs_competence, basic_needs_relatedness),
      \(x) if_else(any(is.na(x)) == 1, NA, last(x) - first(x)),
      .names = "{.col}_change"
    ),
    across(
      c(shared_leadership_change, role_change, collaboration_change,
        psych_safety_change, transformational_leadership_change,
        basic_needs_autonomy_change, basic_needs_competence_change,
        basic_needs_relatedness_change),
      \(x) {
        case_when(
          between(x, -1, 1) ~ "Samma",
          x >= 1 ~ "Höjning",
          x <= 1 ~ "Sänkning"
        )
      },
      .names = "{.col}_cat"
    )
  )

make_pre_post_plot <- function(var, var_cat) {
  pre_post_data_clean |> 
    ggplot(aes(x = time, y = {{var}}, color = {{var_cat}})) +
    geom_point(
      position = position_jitter(height = .2, width = 0, seed = 2)
    ) +
    geom_line(
      aes(group = participant_id), 
      position = position_jitter(height = .2, width = 0, seed = 2)
    ) +
    scale_color_manual(
      NULL,
      values = col_scale
    ) +
    stat_summary(
      aes(color = NULL),
      data = filter(pre_post_data_clean, time == "Före"),
      position = position_nudge(x = -.1)
    ) +
    stat_summary(
      aes(color = NULL),
      data = filter(pre_post_data_clean, time == "Efter"),
      position = position_nudge(x = .1)
    ) +
    scale_y_continuous(NULL, limits = c(1,5), breaks = 1:5) +
    xlab(NULL)
}

ggsave(
  "shared_leadership_fig.png",
  make_pre_post_plot(shared_leadership, shared_leadership_change_cat),
  path = here("output"),
  scale = .5,
  width = 20,
  height = 15,
  units = "cm"
)  

ggsave(
  "role_fig.png",
  make_pre_post_plot(role, role_change_cat),
  path = here("output"),
  scale = .5,
  width = 20,
  height = 15,
  units = "cm"
) 

ggsave(
  "collaboration_fig.png",
  make_pre_post_plot(collaboration, collaboration_change_cat),
  path = here("output"),
  scale = .5,
  width = 20,
  height = 15,
  units = "cm"
)

ggsave(
  "psych_safety_fig.png",
  make_pre_post_plot(psych_safety, psych_safety_change_cat),
  path = here("output"),
  scale = .5,
  width = 20,
  height = 15,
  units = "cm"
) 

ggsave(
  "transformational_leadership_fig.png",
  make_pre_post_plot(transformational_leadership, transformational_leadership_change_cat),
  path = here("output"),
  scale = .5,
  width = 20,
  height = 15,
  units = "cm"
)

p1 <- make_pre_post_plot(basic_needs_autonomy, basic_needs_autonomy_change_cat) +
  labs(title = "Autonomi") +
  theme(legend.position = "none")

p2 <- make_pre_post_plot(basic_needs_competence, basic_needs_competence_change_cat) +
  labs(title = "Kompetens") +
  theme(legend.position = "none")

p3 <- make_pre_post_plot(basic_needs_relatedness, basic_needs_relatedness_change_cat) +
  labs(title = "Tillhörighet") +
  theme(legend.position = "none")

library(patchwork)

ggsave(
  "basic_needs_fig.png",
  p1 + p2 + p3,
  path = here("output"),
  scale = .7,
  width = 30,
  height = 15,
  units = "cm"
)



# Per team --------------------------------------------------

(per_team_fig <- post_data |> 
  pivot_longer(
    c(
      shared_leadership,
      role,
      collaboration,
      psych_safety,
      transformational_leadership,
      basic_needs_autonomy,
      basic_needs_competence,
      basic_needs_relatedness
    )
  ) |> 
  mutate(
    name = factor(
      name,
      levels = c(
        "shared_leadership",
        "role",
        "collaboration",
        "psych_safety",
        "transformational_leadership",
        "basic_needs_autonomy",
        "basic_needs_competence",
        "basic_needs_relatedness"
      ),
      labels = c(
        "Syn på delat ledarskap",
        "Roll i staben",
        "Samarbete i staben",
        "Psykologisk trygghet",
        "Transformativt ledarskap",
        "Autonomi",
        "Kompetens",
        "Tillhörighet"
      )
    )
  ) |> 
ggplot(aes(x = staff, y = value)) +
    stat_summary(geom = "linerange", fun.min = min, fun.max = max, size = 4, color = "#7570b3", alpha = .5) +
    stat_summary(geom = "point", fun = mean, size = 4, shape = 16, color = "#7570b3") +
    scale_x_discrete(labels = c("Stab 1", "Stab 2", "Stab 3", "Stab 4")) +
    facet_wrap(~name, ncol = 4, labeller = label_wrap_gen(width = 42, multi_line = TRUE), scales = "free_x") +
    labs(x = NULL, y = NULL))

ggsave(
  "per_team_fig.png",
  per_team_fig,
  path = here("output"),
  scale = 1,
  width = 30,
  height = 15,
  units = "cm"
)  