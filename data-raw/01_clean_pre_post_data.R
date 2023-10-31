
# Load packages -----------------------------------------------------------

library(here)
library(fs)
library(purrr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)


# Load data ---------------------------------------------------------------

raw_data <- dir_ls(here("data-raw"), regexp = "[.]xlsx$") |> 
  map(read_excel) |> 
  bind_rows(.id = "season") |> 
  mutate(season = str_extract(season, "[:digit:]{4}"))



# Summarise measures ------------------------------------------------------

reversed_data <- raw_data |> 
  mutate(
    # Big 5 personality
    personality_neurotism_3 = 8 - personality_neurotism_3,
    personality_extrovert_3 = 8 - personality_extrovert_3,
    personality_agreeableness_1 = 8 - personality_agreeableness_1,
    personality_conscientiousness_2 = 8 - personality_conscientiousness_2,
    
    # Psychological safety
    psych_safety_1 = 6 - psych_safety_1,
    psych_safety_3 = 6 - psych_safety_3,
    psych_safety_5 = 6 - psych_safety_5
  )

calculated_data <- reversed_data |> 
  mutate(
    .after = coach_nr_years_shared_leadership,
    
    # Big 5 personality
    personality_neurotism = rowMeans(pick(starts_with("personality_neurotism"))),
    personality_extrovert = rowMeans(pick(starts_with("personality_extrovert"))),
    personality_openness = rowMeans(pick(starts_with("personality_openness"))),
    personality_agreeableness = rowMeans(pick(starts_with("personality_agreeableness"))),
    personality_conscientiousness = rowMeans(pick(starts_with("personality_conscientiousness"))),
    
    # Transformational leadership
    transformational_leadership = rowMeans(pick(starts_with("transformational_leadership"))),
    
    # Shared leadership
    
    ## Convert 6 scaled in 2023 to 5 scaled
    
    across(
      starts_with("shared_leadership"),
      \(x) if_else(season == 2023, (x-1)*(4/5)+1, x)
    ),
    
    shared_leadership_model = rowMeans(
      pick(
        c(
          shared_leadership_1, 
          shared_leadership_2, 
          shared_leadership_3
        )
      )
    ),
    
    shared_leadership_collaboration = rowMeans(
      pick(
        c(
          shared_leadership_4, 
          shared_leadership_5, 
          shared_leadership_6
        )
      )
    ),
    
    shared_leadership_roles = rowMeans(
      pick(
        c(
          shared_leadership_7, 
          shared_leadership_8, 
          shared_leadership_9
        )
      )
    ),
    
    # Psychological safety
    psych_safety = rowMeans(pick(starts_with("psych_safety"))),
  )



# Write clean data --------------------------------------------------------

write_csv(calculated_data, here("data/prepost.csv"))
