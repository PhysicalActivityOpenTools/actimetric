#' Fill Gaps in Data Using Imputation (LOCF or Set-Value)
#'
#' This helper function fills gaps in time series or vector data based on the `remaining_epochs` vector.
#' Gaps can be filled using Last Observation Carried Forward (LOCF) or a user-defined constant.
#'
#' @param ... Either a data frame, or one or more named numeric vectors of equal length.
#' @param remaining_epochs An integer vector of the same length as the input data.
#'        Each value represents how many times each observation (including the original)
#'        should appear in the result.
#' @param impute_strategy Character string, either `"locf"` (default) or `"set-value"`.
#'        Determines how gap rows are filled:
#'        - `"locf"` repeats the last observed value(s).
#'        - `"set-value"` fills gap rows with the constant provided in `value`.
#' @param value A single numeric value used to fill gaps when `impute_strategy = "set-value"`.
#'        Required in that case; ignored for `"locf"`.
#'
#' @return A data frame (if multiple columns) or vector (if one column),
#'         with the appropriate number of rows and gap values filled.
#'
#' @details
#' This function avoids full memory expansion of raw time series. Instead, it builds the filled
#' result incrementally and supports efficient handling of imputation for gaps defined by
#' `remaining_epochs`. It's especially helpful in constrained environments or with large data.
#'
#' @examples
#' # LOCF with data frame
#' df = data.frame(x = 1:3, y = c(10, 20, 30))
#' impute_gaps_epoch_level(df, remaining_epochs = c(1, 3, 2))
#'
#' # LOCF with a vector
#' impute_gaps_epoch_level(c(5, 6, 7), remaining_epochs = c(2, 1, 3))
#'
#' # Set-value with a single vector
#' impute_gaps_epoch_level(c(1, 2), remaining_epochs = c(3, 1), impute_strategy = "set-value", value = 99)
#'
#' # Set-value with multiple vectors
#' impute_gaps_epoch_level(x = c(1, 2), y = c(10, 20), remaining_epochs = c(2, 2),
#'                       impute_strategy = "set-value", value = 0)
#' @export
impute_gaps_epoch_level = function(..., remaining_epochs,
                                  impute_strategy = "locf",
                                  value = NULL) {
  inputs = list(...)

  # Determine data source: data frame or multiple vectors
  if (length(inputs) == 1 && is.data.frame(inputs[[1]])) {
    data = inputs[[1]]
  } else {
    data = as.data.frame(inputs)
  }

  stopifnot(impute_strategy %in% c("locf", "set-value"))
  if (impute_strategy == "set-value" && is.null(value)) {
    stop("You must provide a 'value' when using impute_strategy = 'set-value'.")
  }

  total_rows = sum(remaining_epochs)
  filled_list = vector("list", total_rows)
  index = 1

  for (i in seq_len(nrow(data))) {
    reps = remaining_epochs[i]

    # Always include the original row
    filled_list[[index]] = data[i, , drop = FALSE]
    index = index + 1

    # For additional rows, fill based on strategy
    if (reps > 1) {
      if (impute_strategy == "locf") {
        for (j in 2:reps) {
          filled_list[[index]] = data[i, , drop = FALSE]
          index = index + 1
        }
      } else if (impute_strategy == "set-value") {
        for (j in 2:reps) {
          filled_list[[index]] = as.data.frame(lapply(data[i, , drop = FALSE], function(x) value))
          index = index + 1
        }
      }
    }
  }

  result = do.call(rbind, filled_list)
  rownames(result) = NULL

  if (ncol(result) == 1) {
    return(result[[1]])
  } else {
    return(result)
  }
}
