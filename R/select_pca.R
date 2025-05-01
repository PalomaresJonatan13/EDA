#' Select Principal Components Based on Eigenvalue or Variance Threshold
#'
#' `select_pca()` performs a preliminary PCA on the numeric columns of a data
#' frame, selects principal components according to either the Kaiser criterion
#' (eigenvalues ≥ 1) or a cumulative variance threshold, and then reruns PCA
#' with only the selected number of components.  It returns both summary
#' information and a scree plot.
#'
#' @param df A data frame or tibble containing the data.
#' @param vars Optional unquoted tidy-select specification of numeric columns
#'   to include in the PCA.  If `NULL` (default), all numeric columns are used.
#' @param eigenvalues Logical; if `TRUE`, select all components whose
#'   eigenvalues ≥ 1 (Kaiser criterion).  If `FALSE` (default), select the
#'   smallest number of components whose cumulative explained variance
#'   meets or exceeds `percentage`.
#' @param percentage Numeric; target cumulative percentage of explained
#'   variance (0–100) when `eigenvalues = FALSE`.  Default is 80.
#'
#' @return A list with elements:
#' \describe{
#'   \item{method}{A string indicating the selection method:
#'     `"eigenvalues"` or `"% of explained variance >= {percentage}"`.}
#'   \item{n_components}{The number of principal components retained.}
#'   \item{explained_variance}{The cumulative percentage of variance explained
#'     by the retained components.}
#'   \item{scree_plot}{A ggplot object (from `factoextra::fviz_eig()`)
#'     showing the eigenvalues.}
#'   \item{selected_components}{The result of `FactoMineR::PCA()` rerun with
#'     `ncp = n_components`, containing coordinates, loadings, etc.}
#' }
#'
#' @details
#' - A first PCA is run with `ncp = 1` to extract all eigenvalues and variances.
#' - If `eigenvalues = TRUE`, components with eigenvalue ≥ 1 are kept.
#' - Otherwise, the cumulative variance vector is used to find the minimum
#'   number of components whose sum of percentages ≥ `percentage`.
#' - Finally, PCA is rerun with `ncp` equal to the number of retained components,
#'   and a scree plot is generated.
#'
#' @export
select_pca <- function(df, vars = NULL, eigenvalues = FALSE, percentage = 80) {
  vars_q <- rlang::enquo(vars)
  df_num <- select_numeric(df, !!vars_q)

  res_pca <- FactoMineR::PCA(df_num, ncp=1, graph = FALSE)
  # we just care about the eigenvalues for now, thats why we take ncp=1

  eig_vals <- res_pca$eig[, 1]        # eigenvalues
  eig_var  <- res_pca$eig[, 2]        # % variance

  # ---- select components
  if (eigenvalues) {
    keep <- which(eig_vals >= 1)
    k <- length(keep)
    method_str <- "eigenvalues"
  } else {
    cum_var    <- cumsum(eig_var)
    # find the first PC that reaches or exceeds the target percentage
    k          <- which(cum_var >= percentage)[1]
    if (is.na(k)) k <- length(cum_var)
    keep       <- seq_len(k)
    method_str <- paste0("% of explained variance >= ", percentage)
  }

  n_comp <- length(keep)
  exp_var <- sum(eig_var[keep])

  # Now, the real PCA
  res_pca <- FactoMineR::PCA(df_num, ncp=k, graph = FALSE)
  scree <- factoextra::fviz_eig(res_pca, addlabels = TRUE)

  list(
    method              = method_str,
    n_components        = n_comp,
    explained_variance  = exp_var,
    scree_plot          = scree,
    selected_components = res_pca
  )
}


# TESTS
# select_pca(tea, vars=c(-age))
# pca1 = select_pca(tea)
# pca1$method
# pca2 = select_pca(mtcars, vars=c(disp, drat, wt, qsec, vs, gear, carb), eigenvalues=T)
# pca2$method
# pca2$n_components
# pca2$explained_variance
# pca2$scree_plot
# pca2$selected_components$var$cos2
# pca3 = select_pca(mtcars, percentage=90)
# pca3$method
# pca3$n_components
# pca3$explained_variance
# pca3$scree_plot
# pca3$selected_components$ind$contrib
