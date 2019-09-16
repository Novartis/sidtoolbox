#' list association rules
#'
#' This function allows you list association rules in your data using aruleViz package
#' prerequisite: exploreAssociationRule
#' dependencies: aruleViz
#' @param subgroup.data a list of data, covariates indices, outcome indices, trt index, and control value in trt
#' @param support minimum support, supgroup size (double) default=0.1
#' @param confidence minimum confidence required for rule (double) default=0.5
#' @param top maximum number of rules to return (numeric) default=1000
#' @param min_order minimum length of rule (integer) default=2
#' @param max_order maximum length of rule (integer) default=10
#' @param filterby the measure to filter by c("support", "confidence", "lift")
#' @param onlyCovariates whether to exclude outcomes (logical) default=FALSE
#' @return data.frame (lists rules)
#' @export
listAssociationRules <- function(subgroup.data,
                                 support = 0.1,
                                 confidence = 0.5,
                                 min_order = 2,
                                 max_order = 10,
                                 top = NULL,
                                 filterby = "support",
                                 onlyCovariates = FALSE) {
  
  rules <- exploreAssociationRules(subgroup.data,
                                   support = support,
                                   confidence = confidence, 
                                   min_order = min_order,
                                   max_order = max_order,
                                   onlyCovariates = onlyCovariates)
  
  rules.table <- DATAFRAME(rules, separate = TRUE)
  rules.table <- rules.table[order(rules.table[,filterby], decreasing = TRUE),]
  if (is.null(top)) {
    return(rules.table)
  }
  return(rules.table[c(1:top), ])
}
