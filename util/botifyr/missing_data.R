# Detect respondents missing some proportion of data.
#
#
# @param df A data frame or tibble.
# @param id A character string that corresponds to the respondent identifier.
# @param missing A double specifying the percent threshold of missing data.
# @param keep A logical; "keep" the missing data? If TRUE, returns a data frame of respondents with missing data > the `missing` threshold retained; if FALSE, returns a data frame of respondents with missing data <= the `missing` threshold retained.
#

missing_data <- function(df, id, missing = .50, keep = TRUE) {

  # Check for data frames
  if(!is_tibble(df) | !is.data.frame(df)) {
    stop("`df` must be a tibble or data frame")
  }

  # Check for character identifier
  if(!is.character(id)) {
    stop("`id` must be a character string that references an id column in your data frame")
  }

  # Check missing double
  if(!is.double(missing)) {
    stop("`missing` must be a double")
  }

  # Check high confidence boolean
  if(!is.logical(keep)) {
    stop("`keep` must be a boolean")
  }

  # Create the dataframe of misisng value percentages
  missing_df <- as_tibble(df) %>%
    # Transform all columns to character type for pivoting
    mutate(across(everything(), ~ as.character(.))) %>%
    # Pivot to longer tibble
    pivot_longer(!id, names_to = "variable", values_to = "observation") %>%
    # Group and summarize the variables
    group_by(.data[[id]]) %>%
    summarize(
      total = n(),
      n_missing = sum(is.na(observation)),
      perc_missing = n_missing / total
    )

  # If keep = true
  if(keep == TRUE) {

    # Remove participants with missing values > missing threshold
    not_missing <- missing_df %>%
      filter(perc_missing > missing) %>%
      pull(.data[[id]])

    # Select participants without missing data
    df_cleaned <- df %>%
      filter(.data[[id]] %in% not_missing)

    # Return data frame
    return(df_cleaned)

    # If keep is not true
  } else {

    # Keep participants with missing values <= missing threshold
    is_missing <- missing_df %>%
      filter(perc_missing <= missing) %>%
      pull(.data[[id]])

    # Select participants without missing data
    df_cleaned <- df %>%
      filter(.data[[id]] %in% is_missing)

    # Return data frame
    return(df_cleaned)
  }

}
