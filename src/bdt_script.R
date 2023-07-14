# BOT DETECTION TACTICS (BDTS) FOR BOT PREVALENCE PROJECT -----------------

# Author: Cory J. Cascalheira
# Created: 04/02/2023

# The purpose of this script is to execute several bot detection tactics (BDTs)
# on the data from the Qualtrics survey, which is the survey posted on the 
# Internet as an anonymous link.

# If you use any of this code in your project, please remember to cite this 
# script; please use this paper for citation purposes: https://psyarxiv.com/gtp6z/

# Resources:
# - https://psyarxiv.com/gtp6z/
# - https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
# - https://stackoverflow.com/questions/34045738/how-can-i-calculate-cosine-similarity-between-two-strings-vectors
# - https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rvest4
# - https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf

# LIBRARIES AND IMPORT DATA -----------------------------------------------

# Load dependencies
library(tidyverse)
library(lubridate)
library(janitor)
library(zipcodeR)
library(rIP)
library(httr)
library(iptools)
library(gplots)
library(lsa)
library(rvest)

# Load botifyR functions
source("util/botifyr/missing_data.R")
source("util/botifyr/attention_checks.R")

# Import data
bot_survey <- read_csv("data/raw/bot_survey.csv")

# Remove the test responses
bot_survey <- bot_survey[-c(1:16), ] %>%
  # Rename the duration variable
  rename(duration = `Duration (in seconds)`) %>%
  # Convert all emails to lowercase
  mutate(
    contact0 = tolower(contact0),
    contact1 = tolower(contact1),
    contact2 = tolower(contact2)
  ) %>%
  mutate(
    # Convert date to ymd format
    StartDate = ymd_hms(StartDate),
    EndDate = ymd_hms(EndDate),
    # Convert to numeric
    duration = as.numeric(duration),
    # Change duration to minutes
    duration = duration / 60
  ) %>%
  # Organize by starting date
  arrange(StartDate)

# Passed previous BDTs
passed_bdts_04.20.23 <- read_csv("data/passed_bdts/passed_bdts_04.20.23.csv")

# Combine all previous BDTs
previous_respondents <- c(passed_bdts_04.20.23$ResponseId)

# Filter out the previous respondents
bot_survey <- bot_survey %>%
  filter(!ResponseId %in% previous_respondents)

# Check count
nrow(bot_survey)

# KEEP .EDU ADDRESSES -----------------------------------------------------

# Find respondents with a .edu email address
bot_survey_edu <- bot_survey %>%
  mutate(
    edu_email1 = if_else(str_detect(contact1, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email2 = if_else(str_detect(contact2, regex("edu$", ignore_case = TRUE)), 1, 0),
    edu_email1 = if_else(is.na(edu_email1), 0, edu_email1),
    edu_email2 = if_else(is.na(edu_email2), 0, edu_email2)
  ) %>%
  # Keep only respondents with .edu email address
  filter(edu_email1 == 1 | edu_email2 == 1) %>%
  select(-edu_email1, -edu_email2)
bot_survey_edu

# Remove the .edu emails from the survey, add them back later
bot_survey <- bot_survey %>%
  filter(!ResponseId %in% bot_survey_edu$ResponseId)

# Check count
nrow(bot_survey)

# KEEP PEOPLE WHO FILTERED OUT EARLY --------------------------------------

# Find people who did not have experiences with bots
bot_survey_no_bots <- bot_survey %>%
  filter(got_bots != "Yes")

# Remove the respondents who said they had no bots or were not sure
bot_survey <- bot_survey %>%
  filter(!ResponseId %in% bot_survey_no_bots$ResponseId)

# Check count
nrow(bot_survey)

# MISSING DATA ------------------------------------------------------------

# This code will remove respondents who have >= 75% missing data. You need to
# replace "ResponseId" with whatever the unique identifier is for each
# respondent in your survey. If using Qualtrics, there is no need to change
# anything.

# Remove respondents with >= 75% missing data
# Keep people with missing data? NO
bot_survey <- missing_data(bot_survey, "ResponseId", missing = .75, keep = FALSE)

# Check count
nrow(bot_survey)

# UNREASONABLE TIME AND DURATION ------------------------------------------

# Change the duration that fits your study. For example, if you think it is
# impossible for real participants to take your 60-min survey in 30 mins, then
# change `duration > 5` to `duration > 35`

# Remove respondents with a duration <= 5 mins
bot_survey <- bot_survey %>%
  filter(duration > 5)

# Check count
nrow(bot_survey)

# SUSPICIOUS QUALITATIVE DATA ---------------------------------------------

# ...1) Duplicated Qual Responses -----------------------------------------

# Check for duplicated qualitative responses and remove them. You will need to
# change the qualitative variables to match the qualitative data in your survey.

# Alternatively, you can safely delete this code without affecting the rest of
# the script.

# Check for qualitative variables
bot_survey %>%
  select(starts_with("qual")) %>%
  names()

# Get duplicated qualitative responses 
qual_compensation <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_compensation) %>%
  # Remove NA values
  filter(!is.na(qual_compensation)) %>%
  # Covert to lower
  mutate(qual_compensation = tolower(qual_compensation)) %>%
  # Remove common words
  filter(!qual_compensation %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_compensation) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_phase <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_phase) %>%
  # Remove NA values
  filter(!is.na(qual_phase)) %>%
  # Covert to lower
  mutate(qual_phase = tolower(qual_phase)) %>%
  # Remove common words
  filter(!qual_phase %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_phase) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_impact <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_impact) %>%
  # Remove NA values
  filter(!is.na(qual_impact)) %>%
  # Covert to lower
  mutate(qual_impact = tolower(qual_impact)) %>%
  # Remove common words
  filter(!qual_impact %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_impact) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_tactics <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_tactics) %>%
  # Remove NA values
  filter(!is.na(qual_tactics)) %>%
  # Covert to lower
  mutate(qual_tactics = tolower(qual_tactics)) %>%
  # Remove common words
  filter(!qual_tactics %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_tactics) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_recommend <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_recommend) %>%
  # Remove NA values
  filter(!is.na(qual_recommend)) %>%
  # Covert to lower
  mutate(qual_recommend = tolower(qual_recommend)) %>%
  # Remove common words
  filter(!qual_recommend %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_recommend) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_ethics <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_ethics) %>%
  # Remove NA values
  filter(!is.na(qual_ethics)) %>%
  # Covert to lower
  mutate(qual_ethics = tolower(qual_ethics)) %>%
  # Remove common words
  filter(!qual_ethics %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_ethics) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_resources2 <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_resources2) %>%
  # Remove NA values
  filter(!is.na(qual_resources2)) %>%
  # Covert to lower
  mutate(qual_resources2 = tolower(qual_resources2)) %>%
  # Remove common words
  filter(!qual_resources2 %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_resources2) %>%
  pull(ResponseId)

# Get duplicated qualitative responses 
qual_open <- bot_survey %>% 
  # Select item with qualitative data
  # Change these variables to match your unique identifier and qualitative item
  select(ResponseId, qual_open) %>%
  # Remove NA values
  filter(!is.na(qual_open)) %>%
  # Covert to lower
  mutate(qual_open = tolower(qual_open)) %>%
  # Remove common words
  filter(!qual_open %in% c("yes", "no", "none", "good", "n/a", "nothing", "thank you", "thanks")) %>%
  get_dupes(qual_open) %>%
  pull(ResponseId)

# Combine all duplicated responses
all_dupes <- c(qual_compensation, qual_phase, qual_impact,
               qual_tactics, qual_recommend, qual_ethics, qual_resources2, qual_open)

# Show the dupes before removing
bot_survey %>%
  filter((ResponseId %in% all_dupes)) %>%
  select(qual_compensation, qual_phase, qual_impact,
         qual_tactics, qual_recommend, qual_ethics, qual_resources2, qual_open) %>%
  View()

# Remove respondents with duplicated responses
bot_survey <- bot_survey %>%
  filter(!(ResponseId %in% all_dupes))

# Check count
nrow(bot_survey)

# ...2) Cosine Similarity -------------------------------------------------

# Check for qualitative responses that are very similar

# NOTE: this code requires personalization.

# Prepare document for cosine similarity
qual_compensation <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_compensation) %>%
  # Remove NA
  filter(!is.na(qual_compensation))

# Create temp files
tdm_qual_compensation = tempfile()
dir.create(tdm_qual_compensation)

# Loop over each response
for (i in 1:nrow(qual_compensation)) {
  write(qual_compensation$qual_compensation[i], 
        file = paste(tdm_qual_compensation, qual_compensation$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_compensation <- textmatrix(tdm_qual_compensation, minWordLength=1)

# Prepare document for cosine similarity
qual_phase <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_phase) %>%
  # Remove NA
  filter(!is.na(qual_phase))

# Create temp files
tdm_qual_phase = tempfile()
dir.create(tdm_qual_phase)

# Loop over each response
for (i in 1:nrow(qual_phase)) {
  write(qual_phase$qual_phase[i], 
        file = paste(tdm_qual_phase, qual_phase$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_phase <- textmatrix(tdm_qual_phase, minWordLength=1)

# Prepare document for cosine similarity
qual_impact <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_impact) %>%
  # Remove NA
  filter(!is.na(qual_impact))

# Create temp files
tdm_qual_impact = tempfile()
dir.create(tdm_qual_impact)

# Loop over each response
for (i in 1:nrow(qual_impact)) {
  write(qual_impact$qual_impact[i], 
        file = paste(tdm_qual_impact, qual_impact$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_impact <- textmatrix(tdm_qual_impact, minWordLength=1)

# Prepare document for cosine similarity
qual_tactics <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_tactics) %>%
  # Remove NA
  filter(!is.na(qual_tactics))

# Create temp files
tdm_qual_tactics = tempfile()
dir.create(tdm_qual_tactics)

# Loop over each response
for (i in 1:nrow(qual_tactics)) {
  write(qual_tactics$qual_tactics[i], 
        file = paste(tdm_qual_tactics, qual_tactics$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_tactics <- textmatrix(tdm_qual_tactics, minWordLength=1)

# Prepare document for cosine similarity
qual_recommend <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_recommend) %>%
  # Remove NA
  filter(!is.na(qual_recommend))

# Create temp files
tdm_qual_recommend = tempfile()
dir.create(tdm_qual_recommend)

# Loop over each response
for (i in 1:nrow(qual_recommend)) {
  write(qual_recommend$qual_recommend[i], 
        file = paste(tdm_qual_recommend, qual_recommend$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_recommend <- textmatrix(tdm_qual_recommend, minWordLength=1)

# Prepare document for cosine similarity
qual_ethics <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_ethics) %>%
  # Remove NA
  filter(!is.na(qual_ethics))

# Create temp files
tdm_qual_ethics = tempfile()
dir.create(tdm_qual_ethics)

# Loop over each response
for (i in 1:nrow(qual_ethics)) {
  write(qual_ethics$qual_ethics[i], 
        file = paste(tdm_qual_ethics, qual_ethics$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_ethics <- textmatrix(tdm_qual_ethics, minWordLength=1)

# Prepare document for cosine similarity
qual_resources2 <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_resources2) %>%
  # Remove NA
  filter(!is.na(qual_resources2))

# Create temp files
tdm_qual_resources2 = tempfile()
dir.create(tdm_qual_resources2)

# Loop over each response
for (i in 1:nrow(qual_resources2)) {
  write(qual_resources2$qual_resources2[i], 
        file = paste(tdm_qual_resources2, qual_resources2$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_resources2 <- textmatrix(tdm_qual_resources2, minWordLength=1)

# Prepare document for cosine similarity
qual_open <- bot_survey %>% 
  # Select item with qualitative data
  select(ResponseId, qual_open) %>%
  # Remove NA
  filter(!is.na(qual_open))

# Create temp files
tdm_qual_open = tempfile()
dir.create(tdm_qual_open)

# Loop over each response
for (i in 1:nrow(qual_open)) {
  write(qual_open$qual_open[i], 
        file = paste(tdm_qual_open, qual_open$ResponseId[i], sep="/")) 
}

# Create a document-term matrix
tdm_qual_open <- textmatrix(tdm_qual_open, minWordLength=1)

# Calculate cosine similarity
cos2 <- cosine(tdm_qual_compensation)
cos3 <- cosine(tdm_qual_phase)
cos4 <- cosine(tdm_qual_impact)
cos5 <- cosine(tdm_qual_tactics)
cos6 <- cosine(tdm_qual_recommend)
cos7 <- cosine(tdm_qual_ethics)
cos8 <- cosine(tdm_qual_resources2)
cos9 <- cosine(tdm_qual_open)

# Find respondents with high cosine similarity
high_cos2 <- as.data.frame(cos2) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos3 <- as.data.frame(cos3) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos4 <- as.data.frame(cos4) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos5 <- as.data.frame(cos5) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos6 <- as.data.frame(cos6) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos7 <- as.data.frame(cos7) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos8 <- as.data.frame(cos8) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Find respondents with high cosine similarity
high_cos9 <- as.data.frame(cos9) %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "ResponseId", 
               values_to = "cos") %>%
  # Remove perfect similarity
  filter(cos != 1) %>%
  # Detect respondents with cos_similarity >= .80
  filter(cos >= .80) %>%
  distinct(ResponseId) %>%
  pull(ResponseId)

# Combine
high_cosine_sim <- c(high_cos2, high_cos3, high_cos4, high_cos5,
                     high_cos6, high_cos7, high_cos8, high_cos9)
high_cosine_sim

# Show people with high cosine similarity
bot_survey %>%
  filter((ResponseId %in% high_cosine_sim)) %>%
  select(qual_compensation, qual_phase, qual_impact,
         qual_tactics, qual_recommend, qual_ethics, qual_resources2, qual_open) %>%
  View()

# Remove respondents with high cosine similarity
bot_survey <- bot_survey %>%
  filter(!(ResponseId %in% high_cosine_sim))

# Check count
nrow(bot_survey)

# SUSPICIOUS META INFORMATION ---------------------------------------------

# Unique screen resolution
unique_resolution <- bot_survey %>%
  distinct(meta_Resolution) %>%
  arrange(meta_Resolution) %>%
  pull()
unique_resolution

# I did not have time to automate the code for finding new suspicious screen
# resolutions. But, below, you will see all the suspicious resolutions from
# Project QueST. It would be best practice to add to this vector each week. To 
# do that, print the unique_resolutions to the console and do a Google search. If
# the screen resolutions is associated with only 2,000 or 3,000 search results, 
# then add it to the vector. 

# For example, if you want to check 1536x864, then type in Google:
# screen "1536x864"

# You will see millions of results for 1536x864, so we would not add this screen
# resolution to the list.

# Create vector of suspicious resolutions
suspect_resolutions <- c("1060x918", "1072x918", "1712x926", "1718x926", "1718x927", "1716x927", "1108x918", "1664x889", "1642x924", "2256x853", "1918x927", "1918x926", "1364x615", "1398x746", "1687x927", "385x833", "377x753", "491x1064", "424x891", "412x938", "951x912", "361x722", "424x848", "444x987", "450x950", "834x1194", "1685x948", "393x830", "1262x889", "1659x926", "384x832", "360x772", "992x1282", "992x1282", "360x772", "384x832", "393x830", "393x852", NA_character_)

# Remove respondents with suspicious screen resolutions
bot_survey <- bot_survey %>%
  filter(!meta_Resolution %in% suspect_resolutions)

# Check count
nrow(bot_survey)

# IP ADDRESS FRAUD CHECK --------------------------------------------------

# ...1) IP Hub ------------------------------------------------------------

# To use this code to check for international IP addresses and IP addresses
# in a fraud database, you need to sign up for: https://iphub.info/

# After you sign up, replace IPHUB_KEY with your unique key

# Select just IP address
bot_survey_ip_address <- bot_survey %>%
  select(contact2, IPAddress) %>%
  # Must be a data frame, not a tibble, to work
  as.data.frame()

# Get the IP address info from IP Hub
iphub_info <- getIPinfo(bot_survey_ip_address, "IPAddress", iphub_key = Sys.getenv("IPHUB_KEY"))

# Keep respondents not recommended to block
iphub_keep <- iphub_info %>%
  filter(IP_Hub_recommend_block != 1) %>%
  pull(IPAddress)

# Filter respondents to keep
bot_survey <- bot_survey %>%
  filter(IPAddress %in% iphub_keep)

# Check count
nrow(bot_survey)

# ...2) Scamalytics -------------------------------------------------------

# Check respondent IP addresses with a free-to-use fraud service.

# Initialize an empty vector
risk_level_vector <- c()

# For each respondent in the dataframe
for (i in 1:nrow(bot_survey)) {
  
  # Get the Scamalytics page for their URL
  my_url <- paste0("https://scamalytics.com/ip/", bot_survey$IPAddress[i])
  
  # Extract the HTML element corresponding to their risk category
  response <- read_html(my_url)
  div_header <- html_elements(response, xpath = '/html/body/div[3]/div[1]')
  
  # Get the risk category from the HTML element
  risk_level <- str_extract(as.character(div_header), regex("\\w+ Risk", ignore_case = TRUE))
  
  # Save the risk category to a vector
  risk_level_vector <- c(risk_level_vector, risk_level) 
}

# Find risky respondents
risky_scamalytics_ids <- bot_survey %>%
  # Add the risk levels to the response IDs
  select(ResponseId) %>%
  mutate(scamalytics_risk = risk_level_vector) %>%
  # Find respondents with high or very high risk
  filter(scamalytics_risk %in% c("High Risk", "Very High Risk")) %>%
  pull(ResponseId)

# Remove the risky Scamalytics IDs from the data
bot_survey <- bot_survey %>%
  filter(!(ResponseId %in% risky_scamalytics_ids))

# Check count
nrow(bot_survey)

# COMBINE DATA ------------------------------------------------------------

# Bind the dataframes
bot_survey <- rbind(bot_survey, bot_survey_edu) %>%
  rbind(bot_survey_no_bots)

# Check count
nrow(bot_survey)

# SAVE CLEANED DATA -------------------------------------------------------

# Other BDTs performed manually
# - Suspicious email patterns
# - Group qualitative analysis

# Export your data into a CSV file. These respondents are reasonably human.
# However, we recommend that you now contact each of them by phone to ensure
# they are not fraudulent (i.e., humans that do not qualify for your study,
# but snuck in and were not detected by our BDTs)

# Save the data
write_csv(bot_survey, "data/passed_bdts/passed_bdts_05.16.23.csv")
