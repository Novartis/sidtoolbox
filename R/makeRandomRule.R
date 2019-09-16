#' simulateSubgroupData
#' create random rules
#'
#' This function creates random subgroup rules on covariates. It considers the subgroup size
#' and tries to apply that. However for binary covariates this will not hold.
#' @param covariates a data.table of covariates (you can use subgroup.data$covariates)
#' @param depth the depth (number of covariates in rule) default=4
#' @param subgroup_ratio the desired ratio of samples in subgroup created by rule default=100
#' @return list of rules
#' @export
makeRandomRule <- function(covariates, depth = 2, subgroup_ratio = 0.1){
  X <- covariates
  X.cols <- colnames(X)
  X.N <- length(X.cols)
  # qquantile to get subgroup_ratio, if we are lucky
  samples <- nrow(covariates)
  Q <- subgroup_ratio ^ (1/depth)
  random_rule <- function(idx) {
    sapply(X = idx,
           FUN = function(i){
             feature <- X[[i]]
             if(is.factor(feature)) {
               sign = "="
               value = sample(x = levels(feature), size = 1)
             } else {
               sign = sample(x = c("<=", ">") , size = 1)
               q = ifelse(sign == "<=" , Q, 1-Q)
               value = round(quantile(feature, q), 2)
             }
             c(paste(X.cols[i], sign, value))
           })
  }
  param <- list(depth = depth, 
                subgroup_ratio = subgroup_ratio)
  param_text <- unlist(lapply(X = names(param), FUN = function(p) {
    paste(p, toString(param[[p]]))
  }))
  # get a true positive rule
  rule_TP_idx <- sample(x = X.N,
                        size = depth,
                        replace = FALSE)
  TP <- cbind(rule = toString(random_rule(rule_TP_idx), sep = ","),
              method = "sim_TP",
              outcome = "",
              desc = "random",
              param = toString(param_text)
  )
  # get a false positive
  rule_FP_idx <- sample(x = setdiff(c(1:X.N), rule_TP_idx) ,
                        size = depth, replace = FALSE)
  FP <- cbind(rule = toString(random_rule(rule_FP_idx), sep = ","),
              method = "sim_FP",
              outcome = "",
              desc = "random",
              param = toString(param_text)
  )
  rules <- rbind(TP, FP)
  return(as.data.table(rules))
}
