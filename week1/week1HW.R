# strings

library(tidyverse)

# Print only unique character strings in this variable
mosquito_egg_raw |>  
  distinct(treatment)

# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
mosquito_egg_clean <- mosquito_egg_raw |>
  mutate(
    treatment = stringr::str_to_upper(
      stringr::str_extract(treatment, "^[^_]+")
    )
  )

# duplicates

library(janitor)

mosquito_egg_clean |> 
  get_dupes()

mosquito_egg_clean <- mosquito_egg_clean |>
  filter(!duplicated(across(everything())))

mosquito_egg_clean |> 
  summarise(
    n = n(),
    n_distinct(female_id)
  )

# Missing data 

mosquito_egg_clean |> 
  group_by(body_mass_mg) |> 
  summarise(mean = mean(body_mass_mg))

library(skimr)
skimr::skim(mosquito_egg_clean)

mosquito_egg_clean |> 
  filter(if_any(everything(), is.na)) |>
  select(body_mass_mg, eggs_hatched, eggs_laid, 
        collector, age_days, female_id,site,
         everything()) 

mosquito_egg_clean |> 
  drop_na()
# drops data set to 159 rows

mosquito_egg_clean |> 
  group_by(body_mass_mg) |> 
  summarise(
    body_mass_mg = mean(body_mass_mg, na.rm = T)
  )


