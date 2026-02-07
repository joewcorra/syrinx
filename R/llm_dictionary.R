llm_dictionary <- function(discrepancies,
                           model = "claude-haiku-4-5-20251001",
                           credentials = NULL) {

  # Input validation
  if (!is.data.frame(discrepancies)) {
    stop("discrepancies must be a dataframe.")
  }

  required_cols <- c("column", "value", "issue_type")
  if (!all(required_cols %in% names(discrepancies))) {
    stop("discrepancies must contain columns: column, value, issue_type")
  }

  # Filter to only value issues
  value_issues <- discrepancies |>
    dplyr::filter(issue_type == "value_not_in_dictionary", !is.na(value))

  if (nrow(value_issues) == 0) {
    message("No value discrepancies to process.")
    return(tibble::tibble(
      column = character(),
      original_value = character(),
      suggested_value = character(),
      confidence = numeric(),
      reasoning = character()
    ))
  }

  # Get data dictionary
  vals_path <- system.file("extdata",
                           "data_dictionary_values.csv",
                           package = "syrinx")
  dict_values <- readr::read_csv(vals_path, show_col_types = FALSE)

  # Handle credentials - convert to function if needed
  if (!is.null(credentials)) {
    if (is.character(credentials)) {
      # If user passes a string, wrap it in a function
      api_key <- credentials
      credentials <- function() api_key
    }
  }

  # Create chat object
  chat <- ellmer::chat_anthropic(
    model = model,
    credentials = credentials,
    system_prompt = "You are a data standardization assistant. Return ONLY valid JSON matching the requested schema, with no additional text or markdown formatting."
  )

  # Process each column separately
  results_list <- list()

  columns_to_process <- unique(value_issues$column)

  for (col in columns_to_process) {
    # Get invalid values for this column
    invalid_vals <- value_issues |>
      dplyr::filter(column == col) |>
      dplyr::pull(value) |>
      unique()

    # Get valid values from dictionary
    valid_vals <- dict_values |>
      dplyr::filter(value_variable == col) |>
      dplyr::pull(value)

    if (length(valid_vals) == 0) {
      warning(paste0("Column '", col, "' has no valid values in dictionary. Skipping."))
      next
    }

    # Create prompt requesting JSON output
    prompt <- paste0(
      "For the column '", col, "', suggest corrections for these invalid values:\n",
      paste(invalid_vals, collapse = ", "), "\n\n",
      "Valid values are:\n",
      paste(valid_vals, collapse = ", "), "\n\n",
      "Return ONLY a JSON object (no markdown, no code blocks) with this exact structure:\n",
      '{"suggestions": [{"original_value": "...", "suggested_value": "...", "confidence": 0.95, "reasoning": "..."}]}\n\n',
      "For each invalid value, suggest the most appropriate valid value with a confidence score (0-1) and reasoning."
    )

    # Call LLM
    response <- chat$chat(prompt)

    # Parse JSON response
    tryCatch({
      # Clean the response - remove markdown code blocks if present
      response_text <- as.character(response)
      response_text <- stringr::str_replace_all(response_text, "```json\\s*", "")
      response_text <- stringr::str_replace_all(response_text, "```\\s*", "")
      response_text <- stringr::str_trim(response_text)

      # Parse JSON
      parsed <- jsonlite::fromJSON(response_text)
      suggestions <- parsed$suggestions

      # Convert to dataframe and add column name
      if (nrow(suggestions) > 0) {
        col_results <- tibble::tibble(
          column = col,
          original_value = suggestions$original_value,
          suggested_value = suggestions$suggested_value,
          confidence = suggestions$confidence,
          reasoning = suggestions$reasoning
        )
        results_list[[col]] <- col_results
      }
    }, error = function(e) {
      warning(paste0("Failed to parse response for column '", col, "': ", e$message))
      warning(paste0("Raw response was: ", substr(response_text, 1, 200)))
    })
  }

  # Combine all results
  if (length(results_list) == 0) {
    return(tibble::tibble(
      column = character(),
      original_value = character(),
      suggested_value = character(),
      confidence = numeric(),
      reasoning = character()
    ))
  }

  final_results <- dplyr::bind_rows(results_list)

  return(final_results)
}
