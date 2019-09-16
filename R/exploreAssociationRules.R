#' explore association rules within the dataset
#'
#' This function allows you to explore your data by looking at
#' association rules between covariates. Good for identifying biases or
#' higher degree correlations that might exist in your data.
#' dependencies: arules and aruleViz
#' @param subgroup.data a list of data, covariates indices, outcome indices, trt index, and control value in trt
#' @param support minimum support, supgroup size (double) default=0.1
#' @param confidence minimum confidence required for rule (double) default=0.5
#' @param min_order minimum length of rule (integer) default=2
#' @param max_order maximum length of rule (integer) default=10
#' @param onlyCovariates whether to exclude outcomes (logical) default=FALSE
#' @return list of apriori rules
#' @export
exploreAssociationRules <- function(subgroup.data,
                                    support = 0.1,
                                    confidence = 0.5,
                                    min_order = 2,
                                    max_order = 10,
                                    onlyCovariates = FALSE) {
  library(arules)
  if ("constrast" %in% names(subgroup.data)) {
    trt <- subgroup.data$constrast
  } else {
    trt <- c()
  }
  if(onlyCovariates) {
    idx <- c(subgroup.data$covariates,
             trt)
  } else {
    idx <- c(subgroup.data$covariates,
             subgroup.data$outcomes,
             trt)
  }
  data <- subgroup.data$data[, idx, with = FALSE]
  association_rules <- apriori(data,
                               parameter = list(supp = support, 
                                                conf = confidence, 
                                                target = "rules", 
                                                minlen = min_order,
                                                maxlen = max_order))
  return(association_rules)
}

