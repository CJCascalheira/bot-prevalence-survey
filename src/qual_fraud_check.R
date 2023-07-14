# QUALITATIVE FRAUD CHECK -------------------------------------------------

# Author: Cory J. Cascalheira
# Created: 04/20/2023

# Select the qualitative responses from the open-ended questions to determine
# if the responses are reasonably authored by humans and whether they come from
# the target populations (i.e., psychological researchers and students).

# LIBRARIES AND IMPORT DATA -----------------------------------------------

# Load dependencies
library(tidyverse)
library(readxl)

# Import passed BDT data
passed_bdts_04.20.23 <- read_csv("data/passed_bdts/passed_bdts_04.20.23.csv")
passed_bdts_05.16.23 <- read_csv("data/passed_bdts/passed_bdts_05.16.23.csv")

# Import coded qual data
list_of_dfs_nobot <- list.files("data/qual_fraud_check/coded/") %>%
  map(~ read_excel(paste0(getwd(), "/data/qual_fraud_check/coded/", .), sheet = 1))

list_of_dfs_yesbot <- list.files("data/qual_fraud_check/coded/") %>%
  map(~ read_excel(paste0(getwd(), "/data/qual_fraud_check/coded/", .), sheet = 2))

# Name list
names(list_of_dfs_nobot) <- str_remove(list.files("data/qual_fraud_check/coded/"), "\\.xlsx")
names(list_of_dfs_yesbot) <- str_remove(list.files("data/qual_fraud_check/coded/"), "\\.xlsx")

# Separate the coded data by A and B
list_of_dfs_nobot_a <- list_of_dfs_nobot[1:5]
list_of_dfs_nobot_b <- list_of_dfs_nobot[6:8]
list_of_dfs_yesbot_a <- list_of_dfs_yesbot[1:5]
list_of_dfs_yesbot_b <- list_of_dfs_yesbot[6:8]

# PREPARE DATA FRAMES - A -------------------------------------------------

# Select the data and create variables
qual_review_a <- passed_bdts_04.20.23 %>%
  # Create filter categories
  mutate(
    edu_email1 = if_else(str_detect(contact1, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email2 = if_else(str_detect(contact2, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email1 = if_else(is.na(edu_email1), 0, edu_email1),
    edu_email2 = if_else(is.na(edu_email2), 0, edu_email2),
    no_bots_experience = if_else(got_bots != "Yes", 1, 0)
  ) %>%
  unite(has_edu_email, edu_email1:edu_email2, sep = "") %>%
  mutate(has_edu_email = as.numeric(has_edu_email)) %>%
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

# PREPARE DATA FRAMES - B -------------------------------------------------

# Select the data and create variables
qual_review_b <- passed_bdts_05.16.23 %>%
  # Create filter categories
  mutate(
    edu_email1 = if_else(str_detect(contact1, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email2 = if_else(str_detect(contact2, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email1 = if_else(is.na(edu_email1), 0, edu_email1),
    edu_email2 = if_else(is.na(edu_email2), 0, edu_email2),
    no_bots_experience = if_else(got_bots != "Yes", 1, 0)
  ) %>%
  unite(has_edu_email, edu_email1:edu_email2, sep = "") %>%
  mutate(has_edu_email = as.numeric(has_edu_email)) %>%
  mutate(has_edu_email = if_else(has_edu_email > 0, 1, 0)) %>%
  # Name the qual review 
  mutate(qual_review = "A") %>%
  select(ResponseId, qual_review, has_edu_email, no_bots_experience, starts_with("qual"))
qual_review_b

# Divide the data
qual_review_b_no_bot_exp <- qual_review_b %>%
  filter(no_bots_experience == 1) %>%
  select(ResponseId, qual_review, has_edu_email, qual_imagine)
qual_review_b_no_bot_exp

qual_review_b_yes_bot_exp <- qual_review_b %>%
  filter(no_bots_experience == 0) %>%
  select(-no_bots_experience, -qual_imagine)
qual_review_b_yes_bot_exp

# Write to file
write_csv(qual_review_b_no_bot_exp, "data/qual_fraud_check/not_coded/qual_review_b_no_bot_exp.csv")
write_csv(qual_review_b_yes_bot_exp, "data/qual_fraud_check/not_coded/qual_review_b_yes_bot_exp.csv")

# SCORE CODED QUAL DATA ---------------------------------------------------

# Select everyone's scores
grand_df_nobot_a <- bind_cols(list_of_dfs_nobot_a) %>%
  select(ResponseId = ResponseId...1, Celinda, Cory, Mackenzie, Oziel, TJ) %>%
  mutate(Celinda = as.double(Celinda))

grand_df_yesbot_a <- bind_cols(list_of_dfs_yesbot_a) %>%
  select(ResponseId = ResponseId...1, Celinda, Cory, Mackenzie, Oziel, TJ) %>%
  mutate(Mackenzie = as.double(Mackenzie))

grand_df_nobot_b <- bind_cols(list_of_dfs_nobot_b) %>%
  select(ResponseId = ResponseId...1, Celinda, Cory = cory, Mackenzie)

grand_df_yesbot_b <- bind_cols(list_of_dfs_yesbot_b) %>%
  select(ResponseId = ResponseId...1, Celinda, Cory = cory, Mackenzie)

# Combine the no bots and yes bots
grand_df_a <- bind_rows(grand_df_nobot_a, grand_df_yesbot_a)
grand_df_b <- bind_rows(grand_df_nobot_b, grand_df_yesbot_b)

# Calculate average
mean_score_a <- grand_df_a %>%
  pivot_longer(cols = Celinda:TJ, names_to = "coders", values_to = "scores") %>%
  group_by(ResponseId) %>%
  summarize(mean_score = mean(scores, na.rm = TRUE))

mean_score_b <- grand_df_b %>%
  pivot_longer(cols = Celinda:Mackenzie, names_to = "coders", values_to = "scores") %>%
  group_by(ResponseId) %>%
  summarize(mean_score = mean(scores, na.rm = TRUE))

# Combine the mean scores
mean_score_df <- bind_rows(mean_score_a, mean_score_b)

# Distribution of scores
ggplot(mean_score_df, aes(x = mean_score)) +
  geom_histogram()

# Remove any participant whose average fraud audit score is < 2.0
mean_score_df <- mean_score_df %>%
  filter(mean_score >= 2)

# FILTER THE MAIN DATA AND EXPORT -----------------------------------------

# Combine data for the participants passing BDTs
passed_bdts <- bind_rows(passed_bdts_04.20.23, passed_bdts_05.16.23)

# Data coded by fraud auditors
coded_participants <- c(qual_review_a_no_bot_exp$ResponseId, qual_review_a_yes_bot_exp$ResponseId, qual_review_b_no_bot_exp$ResponseId, qual_review_b_yes_bot_exp$ResponseId)

# Double check for errors
passed_bdts %>%
  filter(!(ResponseId %in% coded_participants)) %>%
  select(ResponseId, starts_with("qual"))

# No errors because these participants had no qualitative data

# Filter the passed BDT df
cleaned_data <- passed_bdts %>%
  filter(ResponseId %in% mean_score_df$ResponseId)

# Remove identifying info
cleaned_data_no_pii <- cleaned_data %>%
  select(-RecipientLastName, -RecipientEmail, -RecipientFirstName, -IPAddress,
         -LocationLatitude, -LocationLongitude, -contact0, -contact1, -contact2,
         -starts_with("meta"), -Status, -Progress, -Finished, -ExternalReference,
         -DistributionChannel, -UserLanguage)

# Filter just the contact info
cleaned_data_contact_info <- cleaned_data %>%
  select(ResponseId, IPAddress, contact0, contact1, contact2)

# Filter just the qualitative data
cleaned_qual <- cleaned_data %>%
  select(ResponseId, starts_with("qual"))

# Save data
write_csv(cleaned_data_no_pii, "data/clean/deidentified_data.csv")
write_csv(cleaned_data_contact_info, "data/clean/contact_info/contact_info_data.csv")
write_csv(cleaned_qual, "data/clean/cleaned_qual.csv")
