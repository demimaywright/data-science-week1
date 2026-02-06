# Week 1: Initial Data Exploration ====
# Author: [DW]
# Date: [5/2/26]

# Load packages 
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data 
mosquito_egg_raw <- read_csv(here("week1", "data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview 
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations 
# Your observations (add as comments below):
# - What biological system is this?
#   Mosquitos
# - What's being measured?
#   Egg laying from females
# - How many observations?
#   205
# - Anything surprising?
#   not from first glance.
# - Any obvious problems?
# lots of N/As and duplicates. 

# Week 2 ====

mosquito_egg_raw <- mosquito_egg_raw |>
  mutate(
    treatment = stringr::str_to_upper(
      stringr::str_extract(treatment, "^[^_]+")
    )
  )
# FIX 1: [NA values are present] ====

# Show the problem:
glimpse(mosquito_egg_raw)
# Nas are present in four different columns

# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
  drop_na(body_mass_mg) 

# chose to remove body mass missing data, and leave the collector, as it is an unimportant piece of info. 
# kept NAs in eggs hatched and laid because its possible that the mosquito did not lay an egg or were not found. 
  
  # Verify it worked:

mosquito_egg_data_step1 |>  
 skim()
  
  # What changed and why it matters:
  # body mass mg became 100% complete, since the NAs have been removed. This ensures that all mosquitos that are analysed have a value for body mass.
  # eggs hatched and eggs laid are MNAR, so they have been kept.

  
  # FIX 2: [duplicates in the data]  ====

# Show the problem:
# whole duplicate rows in the data

mosquito_egg_raw %>% distinct()
# 201 distinct rows from 205 observations

mosquito_egg_data_step1 %>% distinct()
# 186 distinct rows from 186 observations

# this suggests that the duplicate rows were contained within the rows with NA body mass.

  # Verify it worked:
mosquito_egg_raw %>%
   get_dupes()
  
  # What changed and why it matters:
  # from correcting the string and correcting body mass values, the duplicates were automatically removed.
  #

