#' plot an interactive plot for association rules
#'
#' This function allows you to plot the rules found using aruleViz package
#' prerequisite: exploreAssociationRule
#' dependencies: aruleViz
#' @param subgroup.data a list of data, covariates indices, outcome indices, trt index, and control value in trt
#' @param support minimum support, supgroup size (double) default=0.1
#' @param confidence minimum confidence required for rule (double) default=0.5
#' @param min_order minimum length of rule (integer) default=2
#' @param max_order maximum length of rule (integer) default=10
#' @param onlyCovariates whether to exclude outcomes (logical) default=FALSE
#' @return plot
#' @export
plotAssociationRulesInteractive <- function(subgroup.data,
                                            support = 0.1,
                                            confidence = 0.5,
                                            min_order = 2,
                                            max_order = 10,
                                            onlyCovariates = FALSE) {
  library(arulesViz)
  rules <- exploreAssociationRules(subgroup.data,
                                   support = support,
                                   confidence = confidence,
                                   min_order = min_order,
                                   max_order = max_order,
                                   onlyCovariates = onlyCovariates)
  plot(rules,
       measure=c("support", "confidence"),
       shading = "lift",
       engine = "interactive")
}
