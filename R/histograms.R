#' Histograms or Densities for Multiple Numeric Variables
#'
#' `histograms()` creates a collection of individual plots—either histograms or
#' density curves—for the selected numeric variables, arranges them in a
#' roughly square grid, and returns the list of ggplot objects invisibly.
#'
#' @param df A data frame or tibble.
#' @param vars Optional unquoted tidy-select specification of numeric columns
#'   to plot (e.g. `c(a, b, starts_with("x"))`).  If `NULL` (default), all
#'   numeric columns in `df` are used.
#' @param density Logical; if `FALSE` (default), draw histograms, otherwise
#'   draw kernel density curves.
#' @param bins Either:
#'   \itemize{
#'     \item A single integer: applied to all variables.
#'     \item An unnamed numeric vector shorter than or equal to the number of
#'       selected variables: assigns values in order, skipping `NA` entries,
#'       with defaults of 30 for unspecified variables.
#'     \item A named vector or list: names correspond to variable names, and
#'       non-`NA` entries override the default 30 bins for those variables.
#'   }
#'
#' @return Invisibly returns a named list of `ggplot` objects, one per
#'   variable.  The primary side effect is printing the combined faceted plot.
#'
#' @details
#' - When `density = FALSE`, each individual histogram uses its own bin count
#'   from `bins`, with defaults of 30.  The combined plot is built by
#'   pre-binning each variable and rendering with `geom_col()`, so each facet
#'   respects its own bin width.
#' - When `density = TRUE`, kernel-density curves are drawn and the `bins`
#'   argument is ignored for the combined figure.
#' - Composite layout uses `facet_wrap(~ variable, scales = "free")` with
#'   grid dimensions `ncol = ceiling(sqrt(k))` and
#'   `nrow = ceiling(k / ncol)`, where `k` is the number of variables.
#'
#' @examples
#' # Default histograms for all numeric vars in mtcars (30 bins each)
#' h1 <- histograms(mtcars)
#'
#' # Density plots for Sepal dimensions in iris
#' histograms(iris, Sepal.Length, Sepal.Width, density = TRUE)
#'
#' # Custom bins: first 2 variables 50 and 10 bins, rest default
#' histograms(mtcars, bins = c(50, 10))
#'
#' # Named override for specific variables
#' histograms(mtcars, bins = list(mpg = 15, hp = 20))
#'
#' @export
histograms <- function(df, vars = NULL, density = FALSE, bins = 30) {
  nums <- select_numeric(df, !!rlang::enquo(vars))
  var_names <- colnames(nums)
  k         <- length(var_names)

  if (!density) {
    var_names <- colnames(nums)
    k         <- length(var_names)

    # start with all defaults
    bins_vec <- rep(30L, k)
    names(bins_vec) <- var_names

    if (length(bins) == 1L && is.numeric(bins)) {
      # single value → apply to all
      bins_vec[] <- as.integer(bins)
    } else {
      bnm <- names(bins)
      if (!is.null(bnm) && any(bnm != "")) {
        # named list/vector: only override non-NA entries
        for (nm in bnm) {
          if (!nm %in% var_names) {
            stop("Unknown variable in 'bins': ", nm, call. = FALSE)
          }
          val <- as.integer(bins[[nm]])
          if (!is.na(val)) {
            bins_vec[nm] <- val
          }
        }
      } else {
        # unnamed vector: override first length(bins) vars, skip NAs
        n_prov <- min(length(bins), k)
        for (i in seq_len(n_prov)) {
          val <- as.integer(bins[i])
          if (!is.na(val)) {
            bins_vec[i] <- val
          }
        }
      }
    }
  }


  plot_list <- purrr::imap(nums, function(v, nm) {
    g <- ggplot2::ggplot(data.frame(value = v), ggplot2::aes(x = value))
    if (density) {
      g <- g + ggplot2::geom_density(fill = "#9ecae1", colour = "#3182bd",
                                     linewidth = 1)
    } else {
      g <- g + ggplot2::geom_histogram(bins = bins_vec[[nm]],
                                       fill = "#3182bd", colour = "white")
    }
    g + ggplot2::labs(title = nm,
                      x = NULL,
                      y = if (density) "Density" else "Count") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  })
  names(plot_list) <- var_names


  ncol_grid <- ceiling(sqrt(k))
  nrow_grid <- ceiling(k / ncol_grid)

  if (density) {
    ## ---- density composite
    long_df <- tidyr::pivot_longer(nums, dplyr::everything(),
                                   names_to = "variable", values_to = "value")

    p_all <- ggplot2::ggplot(long_df, ggplot2::aes(x = value)) +
      ggplot2::geom_density(fill = "#9ecae1", colour = "#3182bd",
                            linewidth = 0.8) +
      ggplot2::facet_wrap(~variable, scales = "free",
                          ncol = ncol_grid, nrow = nrow_grid) +
      ggplot2::labs(x = NULL, y = "Density") +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    ## ---- histogram composite with variable‑specific bins
    # Pre‑bin each variable separately so each facet honours its own `bins`
    hist_df <- purrr::imap_dfr(nums, function(v, nm) {
      brks <- seq(min(v, na.rm = TRUE),
                  max(v, na.rm = TRUE),
                  length.out = bins_vec[[nm]] + 1)
      h    <- graphics::hist(v, breaks = brks, plot = FALSE)
      tibble::tibble(
        variable = nm,
        mid      = h$mids,
        count    = h$counts,
        width    = diff(brks)[1]           # constant within variable
      )
    })

    p_all <- ggplot2::ggplot(hist_df,
                             ggplot2::aes(x = mid,
                                          y = count,
                                          width = width)) +
      ggplot2::geom_col(fill = "#3182bd", colour = "white") +
      ggplot2::facet_wrap(~variable, scales = "free",
                          ncol = ncol_grid, nrow = nrow_grid) +
      ggplot2::labs(x = NULL, y = "Count") +
      ggplot2::theme_minimal(base_size = 12)
  }

  print(p_all)
  invisible(plot_list)       # return individual plots without auto‑printing
}


# TESTS
# hists1 = histograms(mtcars, vars=c(-all_of(c("cyl", "am", "vs")), -gear), bins=list(drat=15, wt=10))
# hists1$hp
# hists2 = histograms(mtcars, density=T)
# histograms(tea)
# histograms(tea, vars=c(-age))
# histograms(mtcars, bins=5)
# histograms(mtcars, bins=c(10, 3, NA, NA, 20, 15, 5, 2)) # in the same order as names(mtcars)
# names(mtcars)
# histograms(mtcars)
