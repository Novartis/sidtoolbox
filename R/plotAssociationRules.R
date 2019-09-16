#' plot a matrix plot for association rules
#'
#' This function allows you to plot the rules found using aruleViz package
#' prerequisite: exploreAssociationRule
#' dependencies: aruleViz
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param support minimum support, supgroup size (double) default=0.1
#' @param confidence minimum confidence required for rule (double) default=0.5
#' @param min_order minimum length of rule (integer) default=2
#' @param max_order maximum length of rule (integer) default=10
#' @param method c("scatterplot", "two-key plot", "matrix", "matrix3D", "mosaic", "doubledecker", "graph", "paracoord" or "grouped", "iplots") (see arulesViz). Supported are two-key-plot, scatterplot, and graph
#' @param measure the measure to plot c("support", "confidence", "lift")
#' @param onlyCovariates whether to exclude outcomes (logical) default=FALSE
#' @return plot
#' @export
plotAssociationRules <- function(subgroup.data,
                                 support = 0.1,
                                 confidence = 0.5,
                                 min_order = 2,
                                 max_order = 10,
                                 method = "graph",
                                 measure = "confidence",
                                 onlyCovariates = FALSE) {
  library(arulesViz)
  rules <- exploreAssociationRules(subgroup.data,
                                   support = support,
                                   confidence = confidence,
                                   min_order = min_order,
                                   max_order = max_order,
                                   onlyCovariates = onlyCovariates)
  plot(rules, method = method, measure = measure)
}
