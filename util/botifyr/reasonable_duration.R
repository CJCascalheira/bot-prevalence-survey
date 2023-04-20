# Remove respondents with improbable survey duration (based on a user-specified threshold) and identical survey duration.
#
#
# @param df A data frame or tibble.
# @param time_var The name of the column in the data set that holds the duration variable of survey completion.
# @param too_fast_minutes An integer specifying a minute threshold at, or below which, a respondent should be eliminated for taking the survey too quickly.
# @param time_format A string specifying the time format of the `time_var` column, such as seconds. Defaults to 'seconds'.
#

reasonable_duration <- function(df, time_var, too_fast_minutes = 20, time_format = "seconds") {

  # Check for data frames
  if(!is_tibble(df) | !is.data.frame(df)) {
    stop("`df` must be a tibble or data frame")
  }

  # Check if too_fast_minutes is an integer
  if(!is.numeric(too_fast_minutes)) {
    stop("`too_fast_minutes` must be an integer representing the minutes of survey completion that are considered too fast for a real participant")
  }

  # Check if time_format is expected string
  if(time_format != "seconds") {
    stop("`time_format` must be the string 'seconds'")
  }

  # Create a temp ID
  df <- df %>%
    mutate(temp_id = 1:nrow(df))

  # Prepare the data frame
  df_1 <- df %>%
    # Select time_var variable and change name into usable format
    select(temp_id, my_duration = {{time_var}}) %>%
    # Removing observations with missing duration
    filter(!is.na(my_duration))

  # If the time_var column is a character
  if(is.character(df_1$my_duration) & time_format == "seconds") {

    # Remove respondents with identical duration
    df_2 <- df_1 %>%
      distinct(my_duration, .keep_all = TRUE)

    # Convert to minutes and filter
    df_3 <- df_2 %>%
      mutate(
        # Transform characters to numeric (representing seconds)
        my_duration = as.numeric(my_duration),
        # Convert seconds to minutes
        my_duration = my_duration / 60
      ) %>%
      # Remove respondents <= threshold
      filter(my_duration > too_fast_minutes)

    # Filter the main data frame
    df_cleaned <- df %>%
      # Filter the data
      filter(temp_id %in% df_3$temp_id) %>%
      # Drop the temp id variable
      select(-temp_id)

    return(df_cleaned)

    # Else if the time_var column is numeric
  } else if(is.numeric(df_1$my_duration) & time_format == "seconds") {

    # Remove respondents with identical duration
    df_2 <- df_1 %>%
      distinct(my_duration, .keep_all = TRUE)

    # Convert to seconds and filter
    df_3 <- df_2 %>%
      mutate(
        # Convert seconds to minutes
        my_duration = my_duration / 60
      ) %>%
      # Remove respondents <= threshold
      filter(my_duration > too_fast_minutes)

    # Filter the main data frame
    df_cleaned <- df %>%
      # Filter the data
      filter(temp_id %in% df_3$temp_id) %>%
      # Drop the temp id variable
      select(-temp_id)

    return(df_cleaned)

    # Else if the time_var column is neither a character nor a numeric
  } else {
    stop("`time_var` must be a numeric column (i.e., a column of integers representing seconds) or a column of characters that can be converted to seconds (i.e., '273')")
  }

}
