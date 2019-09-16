#' load data and set covariates, outcomes, and trt arms for subgroup analysis
#'
#' This function takes a dataset in the format of a data.table along with
#' a list of columns representing covariates and outcomes
#' and the column containing the factor of treatment arms.
#' checks that the columns do not overlap, are in bound, and trt is a factor
#' @param dataset The data containing all patients and covariates, outcomes, and trt (data.table)
#' @param covariates A vector of the covariate column names or indices (vector)
#' @param outcomes A vector of the outcome column names or indices (vector)
#' @param outcomeTypes A vector of the outcome types, in c("numeric", "binary", "count", "survival") (vector)
#' @param trt the treatment column label or index (integer or character)
#' @param ctrl the treatment control (a level in trt column)
#' @param rules a list of rules to be added to subgroup.data (list) default=list()
#' @return instance of subgroup.data appropriate for subgroup analysis (list)
#' @export
loadDataset <- function(dataset, 
                        covariates, 
                        outcomes, 
                        outcomeTypes, 
                        trt = "", 
                        ctrl = NULL, 
                        rules = data.table()) {
  library(data.table)
  SUPPORTED_OUTCOMES <- c("binary", "numeric", "count", "timetoevent", "survival")
  dataset <- as.data.table(dataset)
  columns <- colnames(dataset)
  # check if covariates are colnames or integer indexes
  if (is.character(covariates)) { # colnames
    X <- which(columns %in% covariates)
  }
  else if (is.numeric(covariates) &&
           sum(round(covariates) != covariates) == 0) { # indices
    X <- covariates
  }
  else { # error
    # error
    stop("ERROR: covariates have wrong type, should be colnames or indices.\n")
  }
  # check if outcomes are colnames or integer indexes
  if (typeof(outcomes) == "character") { # colnames
    Y <- which(columns %in% outcomes)
  } else if (is.numeric(outcomes) &&
             sum(round(outcomes) != outcomes) == 0) {
    Y <- outcomes
  } else {
    # error
    stop("ERROR: outcomes have wrong type, should be colnames or indices.\n")
  }
  # outcome types should be: binary, numeric, count, time to event / survival
  # or should I name them: binomial, gaussian, neg binomial, log normal
  # or lm, glm.binomial, glm.poisson, hazard ratio
  if (!(is.character(outcomeTypes) && 
      length(outcomeTypes) == length(outcomes) &&
      sum(sapply(X = outcomeTypes, FUN = function(type) {!type %in% SUPPORTED_OUTCOMES})) == 0)) { # colnames
    # error
    stop(paste("ERROR: outcomeTypes have wrong type, should be a character in ",
               'c("binary", "numeric", "count", "timetoevent", "survival"), .\n'))
  }
  
  # check if trt is colname or integer index
  if (length(trt) == 1 && trt != "") {
    # two arm study
    if (is.character(trt)) {
      TRT <- which(columns == trt)
    } else if (is.numeric(trt) && 
               round(trt) == trt) {
      TRT <- trt
    } else {
      # error
      stop(paste("ERROR: trt has wrong type,",
                 typeof(trt),
                 "should be colname or index.\n"))
    }
  } else if(length(trt) == 0 || trt == "") {
    # one arm study
    TRT <- NULL
    print("Loading one arm study")
  } else if (length(trt) > 1) {
    # error
    stop("ERROR: treatment arm should be atomic (or empty for one arm).\n")
  }
  # check for outofbound columns
  outofbound <- setdiff(c(X, Y, TRT), c(1:length(columns)))
  if(length(outofbound) > 0) {
    stop(paste("ERROR: outofbound error:", outofbound))
  }
  
  # check if there are overlapping columns
  if (length(intersect(X, Y)) > 0) {
    # error
    stop("ERROR: outcomes and covariates overlap\n")
  }
  # check the treatment arm if two arm study
  if (!is.null(TRT)) {
    if (TRT %in% c(X, Y)) {
      # error
      stop("ERROR: trt overlaps with outcomes/covariates")
    }
    # check if trt is factor
    trt_data <- dataset[[TRT]]
    if(!is.factor(trt_data)) {
      warning("WARNING: trt is not a factor, converting now...\n")
      trt_data <- as.factor(trt_data)
      dataset[[TRT]] <- trt_data
    }
    # check if control is in trt and trt has more than 1 level
    if(!(ctrl %in% levels(trt_data))) {
      stop("ERROR: ctrl=", ctrl," not found in levels of trt.\n")
    }
    if(length(levels(trt_data)) < 2) {
      stop(paste("ERROR: not enough levels in contrast:",
                 length(levels(trt_data)),
                 "\n"))
    }
    
    # make the structure
    # make a list of all, refered to as subgroup.data
    subgroup.data <- list(data = dataset,
                          covariates = as.vector(X),
                          outcomes = as.vector(Y),
                          outcomeTypes = as.vector(outcomeTypes),
                          contrast = TRT,
                          control = ctrl,
                          rules = rules)
  } else {
    # make a one arm study structure
    subgroup.data <- list(data = dataset,
                          covariates = as.vector(X),
                          outcomes = as.vector(Y),
                          outcomeTypes = as.vector(outcomeTypes),
                          rules = rules)
  }
  
  return(subgroup.data)
}
