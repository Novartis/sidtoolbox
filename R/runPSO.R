#' runPSO
#'
#' This function runs the PSO method to find rules
#' the genomes is size 2*M where M is number of covariates
#' first M genes are used for sign, with range [0,1] where [0,.5 is mapped to <= and [.5, 1] maps to >
#' second M genes are the cutoffs on each covariate, in range of covariates range
#' abstol is used to stop the algorithm early, minimum enhancement required to terminate
#' https://www.rdocumentation.org/packages/pso/versions/1.0.3/topics/psoptim
#' requires pso
#' @param subgroup.data a subgroup.data structure, see loadDataset, a list of data, covariates, outcomes, contrast, control [list]
#' @param y_idx index of outcome to optimize [numeric or character]
#' @param y_type type of outcome c("numeric", "binary", "count") default=get from subgroup.data$outcomeTypes
#' @param nmin minimum size of samples in subgroup [numeric] default=0.1
#' @param depth maximum depth of rules [integer] default=length of covariates
#' @param fitness_method c("absolute", "model", "pareto" [character] default="model"
#' @param desirable_response whether to increase or decrease the difference c("increasing", "decreasing") [character] default="increasing"
#' @param swarm_size number of particles in swarm [numeric] default=40
#' @param iterations number of iteration to run [numeric] default=100
#' @param abstol absolute tolerance [numeric] default=-Inf
#' @param reltol restart tolerance [numeric] default=0.01
#' @param maxit.stagnate maximum number of iterations with no improvement [numeric] default=100
#' @return a rule data table [data.table]
#' @export
runPSO <- function(subgroup.data,
                   y_idx,
                   y_type = NULL,
                   nmin = 0.1,
                   fitness_method = NULL,
                   desirable_response = "increasing",
                   swarm_size = 40,
                   iterations = 100,
                   abstol = -Inf,
                   reltol = 0.01,
                   maxit.stagnate = 100,
                   depth = 0) {
  library(pso)
  library(MASS)
  library(survival)
  CONF <- 1 - nmin
  
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
  
  desirable_response_coeff <- ifelse(test = (desirable_response == "increasing"),
                                     yes = -1,
                                     no = 1)
  data_PSO <- subgroup.data$data
  idx <- apply(X = data_PSO, MARGIN = 1, FUN = function(row){
    sum(is.na(row)) == 0
  })
  data_PSO <- data_PSO[idx, ]
  subgroup.data$data <- data_PSO
  features <- colnames(data_PSO)
  N = nrow(data_PSO)
  covariates <- data_PSO[ , subgroup.data$covariates, with = FALSE]
  ctrl <- subgroup.data$control
  M <- length(subgroup.data$covariates)
  if (depth == 0) {
    depth <- M
  }
  # a matrix (2×n) containing the range of variables, where n is the number of variables, and first and second rows are the lower bound (minimum) and upper bound (maximum) values, respectively. If all variable have equal upper bound, you can define ar as matrix (2×1).
  rangeVar <- matrix(data = c(rep(c(0,1), 2*M)),
                     nrow = 2)
  # TODO testing to use 0 1 as quantiles
  #for (i in subgroup.data$covariates) {
  #  x = covariates[[i]]
  #  if (is.factor(x)) {
  #    x <- (as.numeric(levels(x)))[x]
  #    covariates[[i]] <- x
  #  }
  #  rangeVar[1,M+i] <- min(x)
  #  rangeVar[2,M+i] <- max(x)
  #}
  
  
  subgroup <- function(X) {
    # initialize S to be all the samples
    S = rep(TRUE, N)
    # for each covariate get X[i] and X[M+i]
    # X[i] > 0.5 greater than else less than equal
    # X[M+i] the cutoff value
    # if covariates[i] is binary then cutoff is accumulated probability of levels
    # else cutoff is the cutoff is only applied if it is not beyond confidence levels
    for (i in 1:M) {
      if (X[i] > 0 && X[i] < 1) {
        x <- covariates[[i]]
        x <- x[!is.na(x)]
        is.sign_lessthan <- (X[i] <= 0.5)
        feat <- features[subgroup.data$covariates[i]]
        cutoff <- ifelse(X[M+i] < 0 , 0, ifelse(X[M+i] > 1, 1, X[M+i]))
        check <- cutoff > 1-CONF && cutoff < CONF
        if (is.factor(x)) {
          L <- levels(x)
          probs <- table(x) / N
          
          for (l in 2:length(L)) {
            probs[l] <- probs[l] + probs[l-1]
          }
          l <- L[min(which(probs - cutoff >= 0))]
          if (is.sign_lessthan) {
            s <- x == l
          } else {
            s <- x != l
          }
        } else if (check & is.sign_lessthan) { # could combine these two conditions, kept for readability 
          s <- x <= quantile(x = x, probs = cutoff, na.rm = TRUE)
        } else if (check & !is.sign_lessthan) {
          s <- x > quantile(x = x, probs = cutoff, na.rm = TRUE)
        } else {
          s <- rep(T, N)
        }
        S <- S & s
      }
    }
    return(S)
  }
  
  print_rule <- function(X) {
    # initialize S to be an empty list of rules
    S <- c()
    # for each covariate get X[i] and X[M+i]
    # X[i] > 0.5 greater than else less than equal
    # X[M+i] the cutoff value
    # if covariates[i] is binary then cutoff is accumulated probability of levels
    # else cutoff is the cutoff is only applied if it is not beyond confidence levels
    for (i in 1:M) {
      if (X[i] > 0 && X[i] < 1) {
        x <- covariates[[i]]
        is.sign_lessthan <- (X[i] <= 0.5)
        feat <- features[subgroup.data$covariates[i]]
        cutoff <- ifelse(X[M+i] < 0 , 0, ifelse(X[M+i] > 1, 1, X[M+i]))
        check <- cutoff > 1-CONF && cutoff < CONF
        if (is.factor(x)) {
          L <- levels(x)
          probs <- sapply(X = L, FUN = function(l) {
            sum(x == l) / N
          })
          for (l in 2:length(L)) {
            probs[l] <- probs[l] + probs[l-1]
          }
          l <- L[min(which(probs - cutoff >= 0))]
          if (is.sign_lessthan) {
            s <- paste(feat, "=", l)
          } else {
            s <- paste(feat, "!=", l)
          }
        } else if (check & is.sign_lessthan) { # could combine these two conditions, kept for readability 
          s <- paste(feat, "<=", round(quantile(x = x, probs = cutoff, na.rm = TRUE),2))
        } else if (check & !is.sign_lessthan) {
          s <- paste(feat, ">=", round(quantile(x = x, probs = cutoff, na.rm = TRUE),2))
        } else {
          s <- c()
        }
        S <- c(S, s)
      }
    }
    return(S)
  }
  
  
  ## calculate the optimum solution using Particle Swarm Optimization Algorithm
  min_subgroup_size <- nmin*N
  
  if (is.null(fitness_method)) {
    if("contrast" %in% names(subgroup.data)) {
      # two arm
      fitness_method <- "interactionEffectSize"
    } else {
      # one arm
      fitness_method <- "effectSize"
    }
  }
  
  fitness_function <- function(X) {
    sg <- subgroup(X)
    # relax depth of rule 
    constraint1 <- ifelse (length(print_rule(X)) <= depth, 1, 0)
    constraint2 <- (sum(sg) - min_subgroup_size) / N
    if (sum(sg) < min_subgroup_size || sum(sg) > N - min_subgroup_size) {
      return(desirable_response_coeff*(100))
    }
    if (fitness_method == "interactionEffectSize") {
      effect  <- interactionEffectSize(subgroup.data = subgroup.data,
                                       y_idx = y_idx,
                                       y_type = y_type,
                                       subgroup_idx = sg)
    } else if (fitness_method == "effectSize") {
      effect  <- effectSize(subgroup.data = subgroup.data,
                                       y_idx = y_idx,
                                       y_type = y_type,
                                       subgroup_idx = sg)
    } else {
      effect  <- fitness_method(subgroup.data = subgroup.data,
                            y_idx = y_idx,
                            y_type = y_type,
                            subgroup_idx = sg)
    }
    
    if (is.na(effect[1])) {
      return (desirable_response_coeff*(100))
    }
    return (effect[1] + constraint1 - effect[2])
  }
  
  initial_par <- runif(n = 2 * M, min = (-nmin) , max = (1+nmin))
  while(fitness_function(initial_par) <= -100) {
    initial_par <- runif(n = 2 * M, min = (-1) , max = (1+1))
  }
  param <- list(nmin = 0.1,
                fitness_method = fitness_method,
                desirable_response = desirable_response,
                swarm_size = swarm_size,
                iterations = iterations,
                abstol = abstol,
                reltol = reltol,
                maxit.stagnate = maxit.stagnate,
                depth = depth)
  param_text <- unlist(lapply(X = names(param), FUN = function(p) {
    paste(p, toString(param[[p]]))
  }))
  param_text <- toString(unlist(lapply(X = names(param), FUN = function(p) {
    paste(p, toString(param[[p]]))
  })))
  o <- psoptim(
      par = initial_par,
      fn = fitness_function,
      lower = rep(x = -0.5, M),
      upper = rep(x = 1+0.5, M),
      control = list(
        maxit = iterations,
        maxit.stagnate = maxit.stagnate,
        vectorize = TRUE,
        #hybrid = TRUE,
        trace = 1,
        trace.stats = FALSE,
        s = swarm_size,
        fnscale = desirable_response_coeff
      )
    )
    rules <- as.data.table(
                        cbind(rule = toString(print_rule(o$par)),
                              method = "PSO",
                              outcome = features[y_idx],
                              desc = o$message,
                              param = toString(param_text)
                              )
                        )

  return(rules)
}
