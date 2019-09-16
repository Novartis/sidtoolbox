#' explore the correlations between the features
#'
#' This function allows you to get the correlation between features
#' with either pearson or spearson method
#' using the rcorr function, which returns correlations and p-values
#' prerequisite:
#' dependencies: ctree from library party
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param method How to calculate the correlation c("pearson", "spearman")
#' @return a list or R and P values (list)
#' @export
correlationMatrix <- function(subgroup.data,
                                 method = "pearson") {
  library(Hmisc)
  library(dplyr)
  data <- subgroup.data$data %>%
    select_if(is.numeric)
  cormat <- rcorr(x = as.matrix(data), 
                  type = method)
  return(cormat)
}
