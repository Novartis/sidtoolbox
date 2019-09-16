
#' explore the correlations between the features
#'
#' This function allows you to plot the correlation between features
#' with either pearson or spearson method
#' using the corrplot function, which displays correlations and p-values
#' prerequisite: exploreCorrelationMatrix
#' dependencies: corrplot
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param method How to calculate the correlation c("pearson", "spearman")
#' @param significant P-value threshold (numeric)
#' @return plots the correlation matrix using corrplot
#' @export
exploreCorrelationMatrix <- function(subgroup.data,
                                     method = "pearson",
                                     significance = 0.025) {
  library(corrplot)
  correlation_matrix <- correlationMatrix(subgroup.data,
                                             method = method)
  corrplot(correlation_matrix$r,
           type="upper",
           order="hclust",
           p.mat = correlation_matrix$P,
           sig.level = significance,
           insig = "blank")
}
