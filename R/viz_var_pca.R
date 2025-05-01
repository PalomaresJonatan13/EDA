#' Visualize Variable Metrics from PCA
#'
#' `viz_var_pca()` generates multiple diagnostic plots for variable-level
#' metrics from a PCA result: quality of representation (cos²) and variable
#' contributions, each as both bar charts and correlation-matrix heatmaps.
#'
#' @param pca_result A `FactoMineR::PCA` object, or a list containing a
#'   `selected_components` element (e.g., output of `select_pca()`) from which
#'   the PCA object will be extracted.
#' @param top Integer; the number of top variables to display in the bar
#'   charts (`fviz_cos2` and `fviz_contrib`).  Defaults to all variables.
#' @param axes Integer vector; the principal component axes to visualize.
#'   Defaults to all available axes in the PCA result.
#'
#' @return Invisibly returns a named list of four plot objects:
#' \describe{
#'   \item{cos2}{A ggplot object displaying squared cosines of variables on the
#'     specified axes (via `factoextra::fviz_cos2`).}
#'   \item{contrib}{A ggplot object showing variable contributions to the
#'     specified axes (via `factoextra::fviz_contrib`).}
#'   \item{cos2_corr}{A heatmap (corrplot) of the cos² values as a matrix plot.}
#'   \item{contrib_corr}{A heatmap (corrplot) of the contribution values as a
#'     matrix plot.}
#' }
#'
#' @details
#' - If `pca_result` is a list and contains `selected_components`, that element
#'   is used; otherwise, `pca_result` itself must be a PCA object.
#' - The `top` argument limits the number of variables highlighted in the bar
#'   plots; if `NULL`, all variables are shown.
#' - The `axes` argument selects which principal components to include in the
#'   bar charts and heatmaps.
#' - Heatmaps are rendered with `is.corr = FALSE`, so values are shown directly
#'   rather than treated as correlation coefficients.
#'
#' @export
viz_var_pca <- function(pca_result, top=NULL, axes=NULL) {
  if ("selected_components" %in% names(pca_result)) {
    pca_result <- pca_result$selected_components
  }

  n <- length(pca_result$var$coord[1,])
  if (is.null(axes)) axes <- 1:n

  plots <- list(
    cos2 = factoextra::fviz_cos2(pca_result, choice="var", axes=axes,
                                 top=ifelse(is.null(top), n, top)),
    contrib = factoextra::fviz_contrib(pca_result, choice="var", axes=axes,
                                       top=ifelse(is.null(top), n, top)),
    cos2_corr = corrplot::corrplot(pca_result$var$cos2, is.corr=FALSE),
    contrib_corr = corrplot::corrplot(pca_result$var$contrib, is.corr=FALSE)
  )
  invisible(plots)
}


# TESTS
# res1 = select_pca(mtcars, percentage=90)
# var1 = viz_var_pca(res1)
# var1$cos2
# var1$contrib
# var1$cos2_corr
# var1$contrib_corr
# res2 = FactoMineR::PCA(dplyr::sample_n(dplyr::select(diamonds, -c(cut, color, clarity)),
#                                        size=1000), graph=F)
# var2 = viz_var_pca(res2, top=3, axes=2:4)
# var2$cos2
# var2$contrib
# var2$cos2_corr
# var2$contrib_corr
