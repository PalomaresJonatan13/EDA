#' Frequency tables for categorical variables
#'
#' `frequency_table()` computes, for each selected non-numeric (categorical)
#' variable, the counts and proportions of each level (including `NA`).
#'
#' @param df A data frame or tibble.
#' @param vars Unquoted tidy-select specification of columns to include.
#'   Defaults to all non-numeric columns in `df`.
#'
#' @return A named list of tibbles, one per variable. Each tibble has columns:
#'   \describe{
#'     \item{level}{The factor or character level (including `NA`).}
#'     \item{absolute}{Absolute count of observations at that level.}
#'     \item{relative}{Relative frequency (proportion) summing to 1.}
#'     \item{percent}{Percent frequency (0–100).}
#'   }
#'
#' @examples
#' # Frequencies of Species in iris
#' freq1 <- frequency_table(iris, Species)
#'
#' # Frequencies for multiple factors in a custom data frame
#' df <- data.frame(
#'   gender  = sample(c("M", "F"), 100, TRUE),
#'   outcome = sample(c("yes", "no"),  100, TRUE)
#' )
#' freq2 <- frequency_table(df, c(gender, outcome))
#'
#' @export
frequency_table <- function(df, vars = NULL) {
  data_cat <- select_non_numeric(df, !!rlang::enquo(vars))

  purrr::imap(
    data_cat,
    function(col, nm) {
      cnt <- table(col, useNA = "ifany")
      abs <- as.integer(cnt)
      tibble::tibble(
        level    = names(cnt),
        absolute = abs,
        relative = abs / sum(abs),
        percent  = 100 * relative
      )
    }
  )
}

# TESTS
# frequency_table(tea)
# frequency_table(tea, vars=c(where(is.numeric)))
# frequency_table(tea, vars=c(slimming, Tea, friends))
