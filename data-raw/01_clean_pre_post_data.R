
# Load packages -----------------------------------------------------------

library(here)
library(readxl)
library(readr)
library(dplyr)


# Load data ---------------------------------------------------------------

raw_data <- read_excel(here("data-raw/basketball_season1_prepost.xlsx"))


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
    personality_neurotism = rowMeans(across(starts_with("personality_neurotism"))),
    personality_extrovert = rowMeans(across(starts_with("personality_extrovert"))),
    personality_openness = rowMeans(across(starts_with("personality_openness"))),
    personality_agreeableness = rowMeans(across(starts_with("personality_agreeableness"))),
    personality_conscientiousness = rowMeans(across(starts_with("personality_conscientiousness"))),
    
    # Transformational leadership
    transformational_leadership = rowMeans(across(starts_with("transformational_leadership"))),
    
    # Shared leadership
    shared_leadership = rowMeans(
      across(
        c(
          shared_leadership_1, 
          shared_leadership_2, 
          shared_leadership_3,
          shared_leadership_4, 
          shared_leadership_5
        )
      )
    ),
    
    # Roles
    role = rowMeans(across(starts_with("role"))),
    
    # Collaboration
    collaboration = rowMeans(
      across(c(starts_with("collaboration"), -collaboration_4))
    ),
    
    # Psychological safety
    psych_safety = rowMeans(across(starts_with("psych_safety"))),
  )



# Write clean data --------------------------------------------------------

write_csv(calculated_data, here("data/season1_prepost.csv"))
