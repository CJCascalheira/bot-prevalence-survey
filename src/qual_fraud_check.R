# QUALITATIVE FRAUD CHECK -------------------------------------------------

# Author: Cory J. Cascalheira
# Created: 04/20/2023

# Select the qualitative responses from the open-ended questions to determine
# if the responses are reasonably authored by humans and whether they come from
# the target populations (i.e., psychological researchers and students).

# LIBRARIES AND IMPORT DATA -----------------------------------------------

# Load dependencies
library(tidyverse)

# Import data
passed_bdts_04.20.23 <- read_csv("data/passed_bdts/passed_bdts_04.20.23.csv")

# PREPARE DATA FRAMES - A -------------------------------------------------

# Select the data and create variables
qual_review_a <- passed_bdts_04.20.23 %>%
  # Create filter categories
  mutate(
    edu_email1 = if_else(str_detect(contact1, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email2 = if_else(str_detect(contact2, regex("edu$", ignore_case = TRUE)), 1, 0),
    no_bots_experience = if_else(got_bots != "Yes", 1, 0)
  ) %>%
  unite(has_edu_email, edu_email1:edu_email2) %>%
  mutate(has_edu_email = if_else(has_edu_email > 0, 1, 0)) %>%
  # Name the qual review 
  mutate(qual_review = "A") %>%
  select(ResponseId, qual_review, has_edu_email, no_bots_experience, starts_with("qual"))
qual_review_a

# Divide the data
qual_review_a_no_bot_exp <- qual_review_a %>%
  filter(no_bots_experience == 1) %>%
  select(ResponseId, qual_review, has_edu_email, qual_imagine)
qual_review_a_no_bot_exp

qual_review_a_yes_bot_exp <- qual_review_a %>%
  filter(no_bots_experience == 0) %>%
  select(-no_bots_experience, -qual_imagine)
qual_review_a_yes_bot_exp

# Write to file
write_csv(qual_review_a_no_bot_exp, "data/qual_fraud_check/not_coded/qual_review_a_no_bot_exp.csv")
write_csv(qual_review_a_yes_bot_exp, "data/qual_fraud_check/not_coded/qual_review_a_yes_bot_exp.csv")
