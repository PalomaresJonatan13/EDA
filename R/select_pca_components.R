#' PCA Components Selection
#'
#' Performs a PCA on the numeric variables of a dataset and selects the number of
#' components to retain based on the cumulative explained variance or the eigenvalues
#' criterion
#'
#' @param data Data frame to be used.
#' @param vars Variables used from the data frame for the PCA
#' @param perc If 'eigenvalues==F', the principal components selected are the ones for which the at least perc percentage of the variability is explained.
#' @param eigenvalues If TRUE the principal components selected are the ones whose eigenvalues are greater than the mean of the eigenvalues.
#' @return A list containing:
#' \describe{
#'    \item{method}{A character indicating the method used for the component selectoin}
#'    \item{num_components}{Number of components selected}
#'    \item{explained_variance}{Total percentage of variance eplained by the selected components}
#'    \item{pca_result}{Full PCA result from \code{FactoMineR::PCA}}
#'    \item{selected_pca_result}{Subset of PCA results containing only selected components}
#' }
#' @examples
#' select_pca_components(iris, perc=90)
#' select_pca_components(iris, vars=c("Sepal.Length", "Sepal.Width"), eigenvalues=TRUE)
#' @export
select_pca_components <- function(data, vars = NULL, perc = 80, eigenvalues = FALSE) {
  if (is.null(vars)) {
    numeric_data <- data[sapply(data, is.numeric)]
  } else {
    numeric_data <- data[vars]
    numeric_data <- numeric_data[sapply(numeric_data, is.numeric)]
  }
  if (ncol(numeric_data) < 2) {
    stop("PCA requires at least two numeric variables.")
  }
  if (perc < 0 || perc > 100) {
    stop("'perc' must be in the interval [0, 100]")
  }

  pca_result <- PCA(numeric_data, graph = FALSE)
  print(fviz_eig(pca_result, addlabels = TRUE))
  eig_vals <- pca_result$eig[,1]

  if (eigenvalues) {
    avg_eig <- mean(eig_vals)
    n_components <- sum(eig_vals > avg_eig)
    method_used <- paste("Eigenvalues greater than average (", round(avg_eig, 3), ")", sep = "")
  } else {
    cum_var <- cumsum(pca_result$eig[,2])
    n_components <- which(cum_var >= perc)[1]
    method_used <- paste("First components explaining at least", perc, "% of variance")
  }

  result <- list(
    method = method_used,
    num_components = n_components,
    explained_variance = sum(pca_result$eig[1:n_components, 2]),
    pca_result = pca_result,
    selected_pca_result = list(
      eig = pca_result$eig[1:n_components, , drop = FALSE],
      var = list(
        coord = pca_result$var$coord[, 1:n_components, drop = FALSE],
        cos2 = pca_result$var$cos2[, 1:n_components, drop = FALSE],
        contrib = pca_result$var$contrib[, 1:n_components, drop = FALSE]
      ),
      ind = list(
        coord = pca_result$ind$coord[, 1:n_components, drop = FALSE],
        cos2 = pca_result$ind$cos2[, 1:n_components, drop = FALSE],
        contrib = pca_result$ind$contrib[, 1:n_components, drop = FALSE]
      ),
      call = pca_result$call
    )
  )

  return(result)
}
