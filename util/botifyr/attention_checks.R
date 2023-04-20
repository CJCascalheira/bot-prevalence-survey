# Remove respondents who fail 1 or more attention checks.
#
# @param df A data frame or tibble.
# @param vars A character vector; the names of the attention check variables (i.e., columns). For example, c('attn_check1', 'attn_check2', 'attn_check3')
# @param correct_answers A character vector; the correct responses that match the attention check variables. For example, c('strongly agree', 'never', 'sometimes')
# @param threshold An integer; the number of attention checks a respondent must fail to be eliminated. Respondents < threshold will be retained in the data set, respondents >= threshold will be removed from the data set. Default = 1.
#

attention_checks <- function(df, vars, correct_answers, threshold = 1) {

  # Check for data frames
  if(!is_tibble(df) | !is.data.frame(df)) {
    stop("`df` must be a tibble or data frame")
  }

  # Check if vars is a character vector
  if(!is.character(vars) & is.vector(vars)) {
    stop("`vars` must be a vector of characters that represent the names of
         attention check variables, such as c('attn_chk1', 'attn_chk2')")
  }

  # Check if correct_answers is a character or numeric vector
  if(!((is.character(correct_answers) | is.numeric(correct_answers)) &
       is.vector(correct_answers))) {
    stop("`correct_answers` must be a vector of characters or numbers that
         represent the answers to attention check variables")
  }

  # Check if threshold is a numeric
  if(!is.numeric(threshold)) {
    stop("`threshold` must be numeric; for best performance,
         provide an integer instead of a double")
  }

  # Prepare the data frame
  df <- df %>%
    mutate(
      # Create a temporary id
      temp_id = 1:nrow(.),
      # Transform the temp_id into a character for row-wise sum later
      temp_id = as.character(temp_id)
    )

  # Select the variables
  df_1 <- df %>%
    # Get the variables for analysis
    select(temp_id, {{vars}})

  # Prepare the vector of correct answers
  correct_answers <- c("dummy_var", correct_answers)

  # Prepare the vector of attention checks
  vars <- c("dummy_var", vars)

  # For each of the attention checks
  for (i in 2:ncol(df_1)) {

    # Check for the correct answer or missing value
    failed_attn <- df_1[, c(1, i)] %>%
      filter((.data[[vars[i]]] != correct_answers[i]) | is.na(.data[[vars[i]]])) %>%
      # If failed attn check, assign 1
      mutate(failed = rep(1)) %>%
      select(temp_id, failed)

    # Add the failed attention check column
    df_1 <- left_join(df_1, failed_attn) %>%
      # If passed the attention check, assign 0
      mutate(failed = if_else(is.na(failed), 0, failed))

    # Name the failed attention check variable in iteration
    names(df_1)[ncol(df_1)] <- paste0("fail_chk_", i)

  }

  # Find the respondents who pass enough attention checks
  filter_ids <- df_1 %>%
    # Select the temp_id and failed attention check variables
    select(temp_id, starts_with("fail_chk_")) %>%
    # Count the number of failed attention checks
    mutate(fail_sum = rowSums(across(where(is.numeric)))) %>%
    # Keep respondents who fail fewer attention checks
    filter(fail_sum < threshold) %>%
    pull(temp_id)

  # Keep the respondents passing most attention checks
  df_cleaned <- df %>%
    filter(temp_id %in% filter_ids) %>%
    # Remove the temp id
    select(-temp_id)

  return(df_cleaned)

}
