#' runVirtualTwins
#' This function runs the Virtual Twins method Foster et. al (2011) Stat Med 30:2867-2880
#' https://rdrr.io/cran/aVirtualTwins/
#' Borrows counterfactual model idea in causal inference
#' Step 1: Impute unobserved outcome and calculate individualized treatment effects Z, using random forests
#' Step 2: Apply recursive partitioning on X with Z as response to identify subgroups with large Z’s
#' Implemented in aVirtualTwins R package
#' Only handles binary outcomes
#' requires aVirtualTwins, randomForest
#' @param subgroup.data a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, contrast, control [list]
#' @param y_idx index of outcome to run virtual twins on (numeric)
#' @param maxdepth maximum depth of rules [numeric] default=4
#' @param forest.type c("one", "double", "fold") [character] default="double"
#' @param tree.type c("class", "reg") [character] default="reg"
#' @param folds if forest.type=fold [numric] default=10
#' @param sampsize ratio to balance outcome class [numeric] default=1
#' @param method c("absolute", "relative", "logit") [character] default="logit"
#' @param interactions a logical value, whether to calculate interactions [logical] default=TRUE
#' @param ntree number of trees in random forest [numeric] default=1000
#' @return a list of data.table [list]
#' @export
runVirtualTwins <- function(subgroup.data,
                            y_idx,
                            desirable_response = "increasing",
                            forest.type = "double",
                            tree.type = "reg",
                            folds = 10,
                            sampsize = 1,
                            maxdepth = 4,
                            method = "absolute",
                            interactions = TRUE,
                            ntree = 1000) {
  library(aVirtualTwins)
  library(randomForest)
  if(!(y_idx %in% subgroup.data$outcomes || 
       y_idx %in% colnames(subgroup.data$data)[subgroup.data$outcomes])) {
    stop("Error: y_idx not in outcomes")
  }
  if (y_idx %in% colnames(subgroup.data$data)[subgroup.data$outcomes]) {
    y_idx <- which(colnames(subgroup.data$data) == y_idx)
  }
  y_type <- subgroup.data$outcomeTypes[which(subgroup.data$outcomes == y_idx)]
  if (y_type != "binary") {
    stop("aVirtualTwins package currently only supports binary outcomes")
  }
  if(desirable_response == "decreasing") {
    sens <- "<"
  } else {
    sens <- ">"
  }
  data_vt <- subgroup.data$data[,c(subgroup.data$covariates), with = FALSE]
  data_vt$virtual_twin_treatment_arm <- ifelse(
    subgroup.data$data[, subgroup.data$contrast, with = FALSE] == subgroup.data$control, 0, 1)
  outcomes <- colnames(subgroup.data$data[, subgroup.data$outcomes, with = FALSE])
  Y <- subgroup.data$data[[y_idx]]
  data_vt$virtual_twin_outcome_binary <- Y
  # Y is binary
  vt.obj <- vt.data(dataset         = as.data.frame(data_vt),
                    outcome.field   = "virtual_twin_outcome_binary",
                    treatment.field = "virtual_twin_treatment_arm",
                    interactions    = interactions)
  # First step : create random forest model
  vt.for <- vt.forest(forest.type = forest.type,
                      method = method,
                      ratio = sampsize,
                      folds = folds,
                      vt.data      = vt.obj,
                      ntree        = ntree,
                      interactions = interactions)
  # Second step : find rules in data
  vt.trees <- vt.tree(tree.type = tree.type,
                      vt.difft  = vt.for,
                      sens = sens,
                      threshold = quantile(vt.for$difft, seq(.5,.8,.1)),
                      maxdepth  = maxdepth)

  # Print results
  vt.sbgrps <- vt.subgroups(vt.trees)
  #print(class(vt.sbgrps))
  res <- as.data.table(vt.sbgrps)

  return(res)
}
