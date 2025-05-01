#' Multiple Barplots for Categorical Variables
#'
#' `barplots()` generates individual barplots for each selected non-numeric
#' (categorical) variable, arranges them in a composite faceted layout, and
#' returns the list of ggplot objects invisibly.
#'
#' @param df A data frame or tibble.
#' @param vars Optional unquoted tidy-select specification of categorical
#'   columns to plot (e.g. `c(a, b, starts_with("x"))`).  If `NULL`
#'   (default), all non-numeric columns in `df` are used.
#'
#' @return Invisibly returns a named list of `ggplot` objects, one per
#'   variable.  The primary side effect is printing a combined faceted plot
#'   of all barplots.
#'
#' @details
#' - Each subplot shows counts of levels (including `NA`) for one variable.
#' - Subplot titles are centered and individual y-axis labels are omitted
#'   in favor of a shared count axis.
#' - Composite layout is produced with `facet_wrap(~variable, scales = "free")`
#'   and an approximately square grid: `ncol = ceiling(sqrt(k)),`
#'   `nrow = ceiling(k / ncol)`, where `k` is the number of variables.
#'
#' @examples
#' # All categorical variables in iris
#' bp <- barplots(iris)
#'
#' # Specific factors in a custom data frame
#' df <- data.frame(
#'   gender  = sample(c("M", "F"), 50, TRUE),
#'   outcome = sample(c("yes", "no"), 50, TRUE),
#'   age     = sample(20:30,    50, TRUE)
#' )
#' bp2 <- barplots(df, c(gender, outcome))
#'
#' @export
barplots <- function(df, vars = NULL) {
  data_cat <- select_non_numeric(df, !!rlang::enquo(vars))
  k        <- ncol(data_cat)

  # build individual barplots
  plot_list <- purrr::imap(data_cat, function(col, nm) {
    df_long <- tibble::tibble(level = forcats::fct_na_value_to_level(col))
    ggplot2::ggplot(df_long, ggplot2::aes(x = level)) +
      ggplot2::geom_bar(fill = "#008") +
      ggplot2::labs(title = nm, x = NULL, y = "Count") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.margin = ggplot2::margin(6, 6, 6, 6, "pt")
      )
  })
  names(plot_list) <- colnames(data_cat)

  # combined faceted plot
  long_df <- tidyr::pivot_longer(
    data_cat,
    cols      = dplyr::everything(),
    names_to  = "variable",
    values_to = "level"
  )
  ncolg <- ceiling(sqrt(k))
  nrowg <- ceiling(k / ncolg)

  p_all <- ggplot2::ggplot(long_df, ggplot2::aes(x = level)) +
    ggplot2::geom_bar(fill = "#008") +
    ggplot2::facet_wrap(~variable, scales = "free", ncol = ncolg, nrow = nrowg) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = ggplot2::margin(6, 6, 6, 6, "pt")
    )

  print(p_all)
  invisible(plot_list)
}


# TESTS
# level_list = list(where=c("chain store+tea shop", "tea shop", "chain store"),
#                   slimming=c("slimming", "No.slimming"))
# df1 <- tea |>
#   mutate(dplyr::across(
#     all_of(names(level_list)),               # select only those columns
#     ~ factor(.x, levels = level_list[[dplyr::cur_column()]]),
#     .names = "{.col}"                        # keep same names
#   ))
# bars1 = barplots(df1, vars=c(friends, Tea, healthy, pub, price, slimming, where))
# barplots(tea, vars=c(age))
# bars1$Tea
