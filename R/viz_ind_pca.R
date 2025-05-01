#' Visualize Individual Metrics from PCA
#'
#' `viz_ind_pca()` generates diagnostic plots for the quality of representation
#' (cos²) and contributions of observations (individuals) in a PCA result,
#' including both bar charts and heatmaps.
#'
#' @param pca_result A `FactoMineR::PCA` object, or a list containing a
#'   `selected_components` element (e.g., output of `select_pca()`); the PCA
#'   object is extracted from `selected_components` if present.
#' @param top Integer; number of top individuals to display in the bar charts
#'   (`fviz_cos2` and `fviz_contrib`).  Defaults to all individuals if `NULL`.
#' @param axes Integer vector; the principal component axes to visualize.
#'   Defaults to all available axes in the PCA result.
#'
#' @return Invisibly returns a named list of four plot objects:
#' \describe{
#'   \item{cos2}{A ggplot object showing squared cosines (quality of
#'     representation) of individuals on the specified axes
#'     (`factoextra::fviz_cos2`).}
#'   \item{contrib}{A ggplot object showing contributions of individuals to the
#'     specified axes (`factoextra::fviz_contrib`).}
#'   \item{cos2_corr}{A heatmap (corrplot) of the cos² values for individuals,
#'     plotted as a matrix.}
#'   \item{contrib_corr}{A heatmap (corrplot) of the contribution values for
#'     individuals, plotted as a matrix.}
#' }
#'
#' @details
#' - If `pca_result` is a list containing `selected_components`, that element
#'   is used as the PCA object.
#' - The `top` argument limits the number of individuals highlighted in the
#'   bar charts; if `NULL`, all individuals are shown.
#' - The `axes` argument specifies which principal components to include in
#'   both bar charts and heatmaps.
#' - Heatmaps use `is.corr = FALSE` to display raw cos² and contribution values.
#'
#' @export
viz_ind_pca <- function(pca_result, top=NULL, axes=NULL) {
  if ("selected_components" %in% names(pca_result)) {
    pca_result <- pca_result$selected_components
  }

  n <- length(pca_result$ind$coord[1,])
  if (is.null(axes)) axes <- 1:n

  plots <- list(
    cos2 = factoextra::fviz_cos2(pca_result, choice="ind", axes=axes,
                                 top=ifelse(is.null(top), n, top)),
    contrib = factoextra::fviz_contrib(pca_result, choice="ind", axes=axes,
                                       top=ifelse(is.null(top), n, top)),
    cos2_corr = corrplot::corrplot(pca_result$ind$cos2, is.corr=FALSE),
    contrib_corr = corrplot::corrplot(pca_result$ind$contrib, is.corr=FALSE)
  )
  invisible(plots)
}


# TESTS
# res1 = select_pca(mtcars, percentage=90)
# ind1 = viz_ind_pca(res1)
# ind1$cos2
# ind1$contrib
# ind1$cos2_corr
# ind1$contrib_corr
# res2 = FactoMineR::PCA(dplyr::sample_n(dplyr::select(diamonds, -c(cut, color, clarity)),
#                                        size=50), graph=F)
# ind2 = viz_ind_pca(res2, top=3, axes=2:4)
# ind2$cos2
# ind2$contrib
# ind2$cos2_corr
# ind2$contrib_corr
