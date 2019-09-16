#' perform a simple simulation and return a subgroup.data list
#'
#' This function creates a simulation for subgroup analysis benchmarking
#' prerequisite: makeRandomRule, parseRule, loadDataset
#' dependencies: data.table
#' @param N number of samples (integer) default=2000
#' @param case c("prognostic", "predictive") default="prognostic"
#' @param arm one arm or two arm simulation c("one", "two") default="two"
#' @param overal_treatment_effect the base effect of treatment (double) default=1
#' @param subgroup_enhanced_effect the enhancement of treatment in true positive subgroup (double) default=1
#' @param subgroup_shift_effect the amount of shift for false positive subgroup (double) default=1
#' @param subgroup_ratio desired ratio of subgroups, not guaranteed (double (0, 1))
#' @param covariates_normal list of c(mean, sd) default=list(c(0,1), c(1,2))
#' @param covariates_uniform list of c(min, max) default=list(c(0,1))
#' @param covariates_binomial list of c(size, probabilty) for count data default=list(c(10, 0.5))
#' @param covariates_binary vector of probabilities default=c(0.5)
#' @param treatment_p ratio of treatment to control (double) default=0.5
#' @param rule_depth vector of depth of true positive and false positive rules (integer) default=2
#' @param correlation vector of correlation, length determines number of correlated covariates (vector) default=c(0.2)
#' @param SD_NOISE_RATIO a small value for adding noise in all data simulation (double) default=0.1
#' @param has_FP whether to insert a false positive subgroup (logical) default=TRUE
#' @return instance of subgroup.data (list)
#' @export
simulateSubgroupData <- function (N = 2000,
                                  case = "prognostic",
                                  arm = "two",
                                  overal_treatment_effect = .5,
                                  subgroup_enhanced_effect = 1,
                                  subgroup_shift_effect = 1,
                                  subgroup_ratio = 0.1,
                                  covariates_normal = list(c(0, 1), c(1, 2)),
                                  covariates_uniform = list(c(0, 1)),
                                  covariates_binomial = list(c(10, 0.5)),
                                  covariates_binary = c(0.5),
                                  treatment_p = 0.5,
                                  rule_depth = 2,
                                  correlation = c(0.2),
                                  SD_NOISE_RATIO = 0.01,
                                  has_FP = FALSE) {
  library(data.table)
  # normal covariates
  X_normal <-
    sapply(
      X = covariates_normal,
      FUN = function(x) {
        rnorm(n = N, mean = x[1], sd = x[2])
      }
    )
  # add correlations
  normal_idx <- c(1:ncol(X_normal))
  n_correlation <- length(correlation)
  if (n_correlation > 0 &
      n_correlation <= length(covariates_normal) / 2) {
    from <-
      sample(x = c(normal_idx), # do not need c() but just to make it look like to
             size = n_correlation,
             replace = FALSE)
    to <-
      sample(x = c(setdiff(normal_idx, from)),
             size = n_correlation,
             replace = FALSE)
    for (i in (1:n_correlation)) {
      noise <- rnorm(n = N, 1, sd = SD_NOISE_RATIO)
      X_normal[, from[i]] <-
        X_normal[, from[i]] + noise * correlation[i] * X_normal[, to[i]]
      print(paste0("adding ",
                   correlation[i],
                   " correlation from X",
                   from[i], " to X",
                   to[i]))
    }
  }
  # binary covariates
  X_binary <-
    as.data.frame(sapply(
      X = covariates_binary,
      FUN = function(x) {
        rbinom(n = N, size = 1, prob = x)
      }
    ))
  
  for (i in colnames(X_binary)) {
    X_binary[, i] <- as.factor(X_binary[, i])
  }
  # count covariates
  X_binomial <-
    sapply(
      X = covariates_binomial,
      FUN = function(x) {
        rbinom(n = N,
               size = as.integer(x[1]),
               prob = x[2])
      }
    )
  # uniform covariates
  X_uniform <-
    sapply(
      X = covariates_uniform,
      FUN = function(x) {
        runif(n = N, min = x[1], max = x[2])
      }
    )
  # bug fix, if one of the covariates was empty would give error
  # of combining 0x0 with 2000x?
  combine <- function(X, Y) {
    if(length(X) > 0 && length(Y) > 0) {
      return(cbind(X, Y))
    } else if (length(X) > 0) {
      return(X)
    } else if (length(Y) > 0) {
      return(Y)
    } else {
      return(list())
    }
  }
  
  covariates <- as.data.table(combine(combine(combine(X_normal, X_binary), X_binomial), X_uniform))
  
  X.N <- ncol(covariates)
  X.cols <- paste0("X", c(1:X.N))
  colnames(covariates) <- X.cols
  
  # make rules
  rules <- makeRandomRule(
    covariates = covariates,
    depth = rule_depth,
    subgroup_ratio = subgroup_ratio
  )
  subgroup_TP <- parseRule(covariates = covariates,
                           rule = rules[1, "rule"])
  subgroup_FP <- parseRule(covariates = covariates,
                           rule = rules[2, "rule"])
  
  # make the test
  if (is.null(treatment_p) || 
      treatment_p <= 0 ||
      treatment_p >= 1 ||
      arm == "one") {
    # one arm simulation
    treatment_p <- 1
    subgroup_shift_effect <- 0
  } 
  TRT <- rbinom(n = N,
                size = 1,
                prob = treatment_p)
  
  # make numeric output
  c <- case == "predicitve"
  predictive_value <- rep(0, N)
  if (c) {
    subgroup_idx <- unlist(strsplit(x = as.character(rules[1, "idx"]),
                                    split = ", "))
    pred_covariates <- covariates[, subgroup_idx, with = FALSE]
    prob <- runif(n = length(subgroup_idx))
    prob <- prob / sum(prob)
    for (i in c(1:length(prob))) {
      predictive_value <- predictive_value +
        prob[i] * pred_covariates[[i]] * rnorm(n = N, mean = 1, sd = SD_NOISE_RATIO)
    }
  }
  # p = all 1s if c is FALSE and predictive value if c is true
  p <- c * predictive_value+!c
  Y_numeric <-
    (
      rnorm(n = N, mean = overal_treatment_effect, sd = SD_NOISE_RATIO) *  TRT + # subgroup case 1 and 2
        rnorm(n = N, mean = subgroup_enhanced_effect, sd = SD_NOISE_RATIO) *  TRT * subgroup_TP +
        rnorm(n = N, mean = subgroup_shift_effect, sd = SD_NOISE_RATIO) * subgroup_FP * has_FP
    ) * p + rnorm(n = N, mean = 0, sd = SD_NOISE_RATIO)
  
  # probability of binary outcome y is z
  total_prob <- overal_treatment_effect + subgroup_enhanced_effect + subgroup_shift_effect * has_FP
  # range normalize Y_numeric to become probabilities
  probability <- 
    (Y_numeric - min(Y_numeric)) / (max(Y_numeric) - min(Y_numeric))
  
  # fix bug
  Y_binary <- as.factor(rbinom(n = N, size = 1, p = probability))
  Y_count <- rnbinom(n = N, size = 1 / SD_NOISE_RATIO, mu = 100 * probability)
  Y_count <- Y_count
  # combine all the features and outcomes
  is.onearm <- (arm == "one" || treatment_p == 1)
  data_sim <- data.table(
    covariates,
    Y_numeric = Y_numeric,
    Y_binary = Y_binary,
    Y_count = Y_count
  )
  outcomeTypes <- c("numeric", "binary", "count")
  if(!has_FP) {
    rules <- rules[1,]
  }
  if (is.onearm) {
    print("Simulating one arm study")
    subgroup.data <-
      loadDataset(
        dataset = data_sim,
        covariates = X.cols,
        outcomes = c("Y_numeric", "Y_binary", "Y_count"),
        outcomeTypes = outcomeTypes,
        rules = rules
      )
  } else {
    data_sim$TRT <- as.factor(TRT)
    print("Simulating two arm study")
    subgroup.data <-
      loadDataset(
        dataset = data_sim,
        covariates = X.cols,
        outcomes = c("Y_numeric", "Y_binary", "Y_count"),
        outcomeTypes = outcomeTypes,
        trt = "TRT",
        ctrl = 0,
        rules = rules
      )
  }
  
  # return subgroup.data format
  return(subgroup.data)
}
