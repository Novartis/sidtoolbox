#' runTSDT
#' This function runs the TSDT method Battioui et al. (2014)
#' https://rdrr.io/cran/TSDT/man/TSDT.html
#' 1. Draw B bootstrap or subsamples, for each sample
#' 1.2. Construct a tree on one treatment arm, leaves form subgroups
#' 1.3. Calculate the treatment difference in subgroups using the other arm
#' 2. Rank subgroups according to internal and external consistency
#' 3. Calculate the median as the suggested cutoff
#' 4. Apply multiplicity adjustment through P permutations (of Y)
#' 5. Calculate adjusted p values
#' requires pso
#' @param subgroup.data a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, contrast, control [list]
#' @param y_idx index of outcome to run on [numeric or character]
#' @param y_type type of outcome c("numeric", "binary", "survival") [character] default will be read from subgroup.data$outcomeTypes
#' @param maxdepth maximum depth of rules [numeric] default=4
#' @param min_subgroup_n_control minimum size of control samples in subgroup [numeric] default=0.05
#' @param min_subgroup_n_trt minimum size of treatment samples in subgroup [numeric] default=0.05
#' @param desirable_response whether to increase or decrease the difference c("increasing", "decreasing") [character] default="increasing"
#' @param nsamples number of bootstrapping subsamples, suggested 100 [numeric] default=100
#' @param permutations number of permutations, suggested 500 [numeric] default=0
#' @param n_cpu number of cpus to use [numeric] default=8
#' @return a  list of rule structure [list]
#' @export
runTSDT <- function(subgroup.data,
                    maxdepth = 4,
                    y_idx,
                    y_type = NULL,
                    min_subgroup_n_control = 0.05,
                    min_subgroup_n_trt = 0.05,
                    desirable_response = "increasing",
                    nsamples = 100,
                    npermutations = 0,
                    n_cpu = 8) {
  # cannot run for 1 covariate
  # soltution, I add a dummy variable
  # added to all TSDT runs, add a dummy, then had the problem of finding dummy rules
  # added check to remove dummy rules, find is.dummy --> removed
  
  # TODO desirale_response doesn't seem to work
  library(TSDT)
  if(!(y_idx %in% subgroup.data$outcomes || 
       y_idx %in% colnames(subgroup.data$data)[subgroup.data$outcomes])) {
    stop("Error: y_idx not in outcomes")
  }
  if (y_idx %in% colnames(subgroup.data$data)[subgroup.data$outcomes]) {
    y_idx <- which(colnames(subgroup.data$data) == y_idx)
  }
  if (is.null(y_type)) {
    y_type <- subgroup.data$outcomeTypes[which(subgroup.data$outcomes == y_idx)]
  }
  
  M <- length(subgroup.data$covariates)
  data <- subgroup.data$data[, subgroup.data$covariates, with = FALSE]
  idx <- apply(X = data, MARGIN = 1, FUN = function(row) {
    sum(is.na(row)) == 0
  })
  data <- data[idx, ]
  N <- nrow(data)
  outcomes <- colnames(subgroup.data$data)[subgroup.data$outcomes]
  data$dummy <- rnorm(n = N)
  Y <- subgroup.data$data[[y_idx]]
  Y <- Y[idx]
  if (is.factor(Y) || is.character(Y)) {
    Y <- as.numeric(levels(Y))[Y]
  } 
  
  if(y_type == "numeric") {
    type <- "continuous"
  } else if (y_type == "timetoevent") {
    type <- "survival"
  } else if(y_type == "count") {
    type <- "continuous"
    # Caution: TSDT does not have a built-in model for count data. 
    # It assumes it is numeric, and the package adds log transformation on it
    Y <- log(Y + 1)
  } else if (y_type == "survival") {
    type <- "survival"
  } else if (y_type == "binary") {
    type <- "binary"
  }
  
  print(y_type)
  # check to run one arm or two arm
  if ("contrast" %in% names(subgroup.data)) {
    # two arm study
    trt <- subgroup.data$data[[subgroup.data$contrast]]
    trt_control <- subgroup.data$control
    TSDT.subgroups <-
      TSDT(
        response = Y,
        response_type = type,
        trt = trt,
        trt_control = trt_control,
        covariates = as.data.frame(data),
        desirable_response = desirable_response,
        min_subgroup_n_control = min_subgroup_n_control * N,
        min_subgroup_n_trt = min_subgroup_n_trt * N,
        maxdepth = maxdepth,
        rootcompete = maxdepth,
        inbag_score_margin = 0,
        oob_score_margin = 0,
        n_samples = nsamples,     ## use value >= 100 in real world application
        n_permutations = npermutations,     ## use value >= 500 in real world application
        trace = TRUE, n_cpu = n_cpu
      )
  } else {
    # one arm study
    TSDT.subgroups <-
      TSDT(
        response = Y,
        response_type = type,
        covariates = data,
        desirable_response = desirable_response,
        inbag_score_margin = 0,
        oob_score_margin = 0,
        n_samples = nsamples,     ## use value >= 100 in real world application
        n_permutations = npermutations,     ## use value >= 500 in real world application
        maxdepth = maxdepth,
        rootcompete = maxdepth,
        trace = TRUE, n_cpu = n_cpu)
  }
  
  res <- as.data.table(TSDT.subgroups@superior_subgroups)
  return(res)
}
