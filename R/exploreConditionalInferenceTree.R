#' explore the conditional inference tree
#'
#' This function allows you to plot the conditional inference
#' in your data as a tree
#' for information on hyper parameters see 'ctree_control' from library party
#' prerequisite:
#' dependencies: ctree from library party
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param stump whether to draw a shallow tree with maximum 3 objects (logical) default=TRUE
#' @param testtype "a character specifying the type of the test statistic to be applied" c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic") default="Bonferroni"
#' @param teststat "a character specifying how to compute the distribution of the test statistic." c("quad", "max") default="quad"
#' @param nresample number of resampling for MonteCarlo method (integer) default=100
#' @param significance threshold for p-value (numeric [0-1]) default=0.025
#' @param nmin minimum number of samples in each leaf (subgroup size), if <1 considered as ratio (numeric) default=0.1
#' @param maxdepth maximum depth (statements in rule) (integer) default=4
#' @return plots a tree using ctree plot method
#' @export
exploreConditionalInferenceTree <- function(subgroup.data,
                                            stump = TRUE,
                                            y_idx,
                                            teststat = "quad",
                                            testtype = "Bonferroni",
                                            nresample = 500,
                                            significance = 0.025,
                                            nmin = 0.1,
                                            maxdepth = 4) {
  library(party)
  if(is.character(y_idx) && 
     !y_idx %in% colnames(subgroup.data$data)[subgroup.data$outcomes])
    return()
  if(is.numeric(y_idx) && !y_idx %in% subgroup.data$outcomes)
    return()
  # allow nmin to be ratio or absolute
  if (nmin < 1) {
    minsplit <- nmin * nrow(subgroup.data$data)
  } else {
    minsplit <- nmin
  }
  
  outcomes <- colnames(subgroup.data$data)[subgroup.data$outcomes]
  
  dataset <- subgroup.data$data[, c(subgroup.data$covariates, subgroup.data$trt), with = FALSE]
  dataset$y = subgroup.data$data[, y_idx, with = FALSE]
  if (is.numeric(y_idx)) {
    outcomeType <- subgroup.data$outcomeTypes[which(subgroup.data$outcomes == y_idx)]
  } else {
    outcomeType <- subgroup.data$outcomeTypes[which(subgroup.data$outcomes == which(colnames(subgroup.data$data) == y_idx))]
  }
  
  if (outcomeType == "count")  {
    print("outcome is count data, will normalize")
    dataset$y <- log(dataset$y + 1)
  }
  decision_tree <- ctree(formula = y ~ .,
                         data = dataset,
                         controls = ctree_control(stump = stump,
                                                  teststat = teststat,
                                                  testtype = testtype,
                                                  nresample = nresample,
                                                  minsplit = minsplit,
                                                  mincriterion = (1 - significance),
                                                  maxdepth = maxdepth))
  print(decision_tree@tree)
  return(decision_tree)
}
