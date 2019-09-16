#' parseRule
#' parse rule and return a logical vector for subgroup
#'
#' This function parses a rule.
#' A rule is a vector of strings, each containing a statement
#' in format of "covariate sign value" i.e. "X1 = 1" or "blood >= 2.01"
#' A logical vector is returned indicating membership in subgroup according to the rule.
#' @param covariates a data.table of covariates (you can use subgroup.data$covariates) [list or data.table]
#' @param rule list of statement strings in rule format [character]
#' @return subgroup a logical vector
#' @export
parseRule <-function(covariates, rule) {
  makeSpaceInRule <- function(rule) {
    rule <- gsub(pattern = "\\s", replacement = "", x = rule)  
    if(length(grep(x = rule, pattern = " |\t")) >= 2)
    {
      return(rule)
    }
    if(length(grep(x = rule, pattern = ">=")) > 0) {
      rule <- gsub(x = rule, pattern = ">=", replacement = " >= ")
    } else if (length(grep(x = rule, pattern = "<=")) > 0) {
      rule <- gsub(x = rule, pattern = "<=", replacement = " <= ")
    } else if (length(grep(x = rule, pattern = "==")) > 0) {
      rule <- gsub(x = rule, pattern = "==", replacement = " == ")
    } else if (length(grep(x = rule, pattern = "=>")) > 0) {
      rule <- gsub(x = rule, pattern = "=>", replacement = " >= ")
    } else if (length(grep(x = rule, pattern = "=<")) > 0) {
      rule <- gsub(x = rule, pattern = "=<", replacement = " >= ")
    } else if (length(grep(x = rule, pattern = "<>")) > 0) {
      rule <- gsub(x = rule, pattern = "<>", replacement = " != ")
    } else if (length(grep(x = rule, pattern = "!=")) > 0) {
      rule <- gsub(x = rule, pattern = "!=", replacement = " != ")
    } else if (length(grep(x = rule, pattern = "=")) > 0) {
      rule <- gsub(x = rule, pattern = "=", replacement = " == ")
    } else if (length(grep(x = rule, pattern = ">")) > 0) {
      rule <- gsub(x = rule, pattern = ">", replacement = " > ")
    } else if (length(grep(x = rule, pattern = "<")) > 0) {
      rule <- gsub(x = rule, pattern = "<", replacement = " < ")
    } else if (length(grep(x = rule, pattern = "=")) > 0) {
      rule <- gsub(x = rule, pattern = "=", replacement = " == ")
    }
    return(rule)
  }
  # TODO add support for subgroup.data
  if(class(covariates)[1] %in% c("list")) {
    covariates <- covariates$data[, covariates$covariates, with = FALSE]
  }
  N <- nrow(covariates)
  subgroup <- rep(TRUE, N)
  #rule_corrected <- gsub(x = as.character(rule), pattern = " |\t", replacement = "")
  rule_corrected <- unlist(strsplit(x = as.character(rule),
                                    split = " , |, | ,|,|\n|\t| & |& | &|&| AND | ; | ;|; |;", fixed = FALSE))
  for(r in rule_corrected) {
    r <- makeSpaceInRule(r)
    rule.break <- unlist(strsplit(x = r, split = " |\t"))
    rule.break <- setdiff(rule.break, "")
    feature <- rule.break[1]
    sign <- rule.break[2]
    value <- rule.break[3]
    x <- covariates[, feature, with = FALSE]
    if(is.numeric(x)){
      value <- as.numeric(value)
    }
    # & with subgroup
    sbg <-
      switch(sign,
             ">" = {
               x > value
             },
             ">=" = {
               x >= value
             },
             "==" = {
               x == value
             },
             "!=" = {
               x != value
             },
             "<=" = {
               x <= value
             },
             "<" = {
               x < value
             }
      )
    sbg[is.na(sbg)] <- FALSE
    subgroup <- subgroup & sbg
  }
  return(subgroup)
}
