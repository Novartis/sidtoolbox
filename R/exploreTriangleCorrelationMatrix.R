#' explore the correlations between the features
#'
#' This function allows you to plot the correlation between features
#' with either pearson or spearson method using ggplot2
#' prerequisite: exploreCorrelationMatrix
#' dependencies: reshape, ggplot2
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param method How to calculate the correlation c("pearson", "spearman")
#' @param significant P-value threshold (numeric)
#' @return plots the correlation matrix using corrplot
#' @export
exploreTriangleCorrelationMatrix <- function(subgroup.data,
                                             method = "pearson") {
  library(reshape2)
  library(ggplot2)
  correlation_matrix <- correlationMatrix(subgroup.data,
                                             method = method)
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  upper_triangle <- get_upper_tri(correlation_matrix$r)
  melted_cormat <- melt(upper_triangle,
                        varnames = c("features_", "features"))
  p <- ggplot(data = melted_cormat,
              aes(features_, features, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue",
                         high = "red",
                         mid = "white",
                         midpoint = 0,
                         limit = c(-1,1),
                         space = "Lab",
                         name=paste0(method,"\ncorrelation")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    coord_fixed()
  return(p)
}
