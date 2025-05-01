#' Comprehensive data diagnostics for numeric and categorical variables
#'
#' `describe_data()` computes four complementary summaries for a data frame:
#' 1. **Numeric summary** (`summary_numeric`): mean, standard deviation,
#'    variance, min, quartiles, max, kurtosis, skewness, IQR, and non-missing
#'    count for each numeric variable.
#' 2. **Missing data summary** (`summary_missing`): counts and proportions of
#'    missing values as returned by `dlookr::diagnose()`.
#' 3. **Outlier summary** (`summary_outlier`): outlier diagnostics for numeric
#'    variables via `dlookr::diagnose_outlier()`.
#' 4. **Categorical summary** (`summary_category`): frequency and proportion
#'    details for non-numeric variables via `dlookr::diagnose_category()`.
#'
#' @param df A data frame or tibble to summarize.
#' @param vars Optional unquoted tidy-select specification of columns to
#'   include in the analysis (e.g. `c(a, b, starts_with("x"))`).  If `NULL`,
#'   *all* columns are used.  Non-selected columns are ignored.
#'
#' @return A named list with components:
#' \describe{
#'   \item{summary_numeric}{A tibble of numeric summaries per variable.}
#'   \item{summary_missing}{A tibble of missing-data diagnostics.}
#'   \item{summary_outlier}{A tibble of outlier diagnostics for numeric vars.}
#'   \item{summary_category}{A tibble of frequency diagnostics for categorical vars.}
#' }
#'
#' @examples
#' # Describe all variables in iris
#' desc1 <- describe_data(iris)
#'
#' # Describe only Sepal measurements
#' desc2 <- describe_data(iris, vars = c(Sepal.Length, Sepal.Width))
#'
#' # Get summaries for mtcars engine variables
#' desc3 <- describe_data(mtcars, vars = c(mpg, hp, disp))
#'
#' @export
describe_data <- function(df, vars = NULL) {
  nums <- select_numeric(df, !!rlang::enquo(vars), empty=T)
  cats  <- select_non_numeric(df, !!rlang::enquo(vars), empty=T)

  num_summary <- purrr::imap_dfr(
    nums,
    function(v, nm) tibble::tibble(
      variable  = nm,
      mean      = mean(v, na.rm = TRUE),
      sd        = stats::sd(v,  na.rm = TRUE),
      variance  = stats::var(v, na.rm = TRUE),
      min       = min(v,  na.rm = TRUE),
      q1        = stats::quantile(v, 0.25, na.rm = TRUE),
      q2        = stats::median(v, na.rm = TRUE),
      q3        = stats::quantile(v, 0.75, na.rm = TRUE),
      max       = max(v,  na.rm = TRUE),
      kurtosis  = moments::kurtosis(v, na.rm = TRUE),
      skewness  = moments::skewness(v, na.rm = TRUE),
      IQR       = stats::IQR(v,  na.rm = TRUE),
      count     = sum(!is.na(v))
    )
  )

  missing_tbl   <- dlookr::diagnose(cbind(nums, cats))
  outlier_tbl   <- if (ncol(nums) > 0) dlookr::diagnose_outlier(nums)   else tibble::tibble()
  category_tbl  <- if (ncol(cats) > 0) dlookr::diagnose_category(cats) else tibble::tibble()

  list(
    summary_numeric  = num_summary,
    summary_missing  = missing_tbl,
    summary_outlier  = outlier_tbl,
    summary_category = category_tbl
  )
}

# TESTS
# describe_data(tea, vars=c(where(is.numeric), slimming, tearoom))
# describe_data(tea, vars=c(age))
# describe_data(tea, vars=c(!where(is.numeric)))
