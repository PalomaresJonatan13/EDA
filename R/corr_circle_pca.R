#' PCA Correlation Circle Plots for Variables and Individuals
#'
#' `corr_circle_pca()` produces a list of correlation circle visualizations for
#' both variables and individuals from a PCA result, colored by metric
#' (none, cos², or contribution) and displayed on specified principal component
#' axes.
#'
#' @param pca_result A `FactoMineR::PCA` object, or a list containing a
#'   `selected_components` element (e.g., output of `select_pca()`), from which
#'   the PCA object will be extracted if present.
#' @param axes Integer vector of length two giving the principal component
#'   axes to plot (e.g., `c(1, 2)` for the first two dimensions).
#'
#' @return A named list of six `ggplot` objects:
#' \describe{
#'   \item{var}{Correlation circle for variables, points colored solid.}
#'   \item{var_cos2}{Correlation circle for variables, colored by squared cosines
#'     (quality of representation).}
#'   \item{var_contrib}{Correlation circle for variables, colored by variable
#'     contributions.}
#'   \item{ind}{Correlation circle for individuals, points colored solid.}
#'   \item{ind_cos2}{Correlation circle for individuals, colored by squared
#'     cosines (quality of representation).}
#'   \item{ind_contrib}{Correlation circle for individuals, colored by
#'     contributions.}
#' }
#'
#' @details
#' - Uses `factoextra::fviz_pca_var()` and `factoextra::fviz_pca_ind()`
#'   with `repel = TRUE` to avoid label overlap.
#' - Color gradients for cos² and contribution are defined by the palette
#'   `c("#00afbb", "#e7b800", "#fc4e07")`.
#' - The variable plots (`var*`) display variable loadings as vectors on the
#'   correlation circle; the individual plots (`ind*`) show observation
#'   projections.
#'
#' @export
corr_circle_pca <- function(pca_result, axes=c(1, 2)) {
  if ("selected_components" %in% names(pca_result)) {
    pca_result <- pca_result$selected_components
  }

  list(
    var         = factoextra::fviz_pca_var(pca_result, col.var="#000", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T # avoiding text overlapping
                  ),
    var_cos2    = factoextra::fviz_pca_var(pca_result, col.var="cos2", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T
                  ), # var_cos2 is the same as var_contrib
    var_contrib = factoextra::fviz_pca_var(pca_result, col.var="contrib", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T
                  ),
    ind         = factoextra::fviz_pca_ind(pca_result, col.ind="#000", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T
                  ),
    ind_cos2    = factoextra::fviz_pca_ind(pca_result, col.ind="cos2", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T
                  ),
    ind_contrib = factoextra::fviz_pca_ind(pca_result, col.ind="contrib", axes=axes,
                       gradient.cols=c("#00afbb", "#e7b800", "#fc4e07"),
                       repel=T
                  )
  )
}


# TESTS
# res1 = select_pca(mtcars, percentage=90)
# circle1 = corr_circle_pca(res1)
# circle1$var
# circle1$var_cos2
# circle1$var_contrib
# circle1$ind
# circle1$ind_cos2
# circle1$ind_contrib
#
# res2 = select_pca(mtcars, percentage=90)
# circle2 = corr_circle_pca(res1, axes=c(2, 4))
# circle2$var
# circle2$var_cos2
# circle2$var_contrib
# circle2$ind
# circle2$ind_cos2
# circle2$ind_contrib
