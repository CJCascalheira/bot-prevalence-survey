# DESCRIBE DEMOGRAPHICS ---------------------------------------------------

# Author = Cory J. Cascalheira
# Date created = 04/29/2023

# This R script executes simple descriptive analyses of the demographic 
# information in the bot prevalence survey.

# LOAD LIBRARIES AND IMPORT DATA ------------------------------------------

# Load dependencies
library(psych)
library(tidyverse)

# Import data
bot_survey <- read_csv("data/clean/cleaned_data.csv")

# DEMOGRAPHIC ANALYSESE ---------------------------------------------------

# Total number of participants
nrow(bot_survey)

# Career stage of participants
bot_survey %>%
  count(career) %>%
  mutate(percent = (n / nrow(bot_survey)) * 100) %>%
  arrange(desc(n))

# Primary research domain of participants
bot_survey %>%
  count(domain1) %>%
  mutate(percent = (n / nrow(bot_survey)) * 100) %>%
  arrange(desc(n))

# Other domains of participants - don't report this
bot_survey %>%
  count(domain2) %>%
  mutate(percent = (n / nrow(bot_survey)) * 100) %>%
  arrange(desc(n))

# Population studied - percentages may not add up to 100% due to multiple choice
bot_survey %>%
  select(pop_age) %>%
  mutate(pop_age = str_split(pop_age, ","))
  
  count(pop_age) %>%
  mutate(percent = (n / nrow(bot_survey)) * 100) %>%
  arrange(desc(n))
