#' parseTSDTResults
#' parse TSDT table result return rule structure
#'
#' This function parses a data.table returned by runTSDT.
#' A rule structure has at least 5 columns: rule, method, outcome, desc, and param
#' The rule can be parsed by parseRule to get subgroup
#' @param TSDT_table a data.table of results returned by TSDT [data.table]
#' @param outcome name of outcome column that TSDT was run on [character]
#' @param parameters used to run TSDT, saved as log [list or character]
#' @return a rule structure [data.table]
#' @export
parseTSDTResults <- function(TSDT_table, outcome, param, filter = "Strong") {
  library(data.table)
  SPLIT_X <- "xxxxx"
  PATTERN_REPLACE <- "\\[|\\]|\\)|\\(|'"
  NUMBER_SEP <- ";"
  N <- nrow(TSDT_table)
  rules <- TSDT_table$Subgroup
  cutoffs <- TSDT_table$Suggested_Cutoff
  TSDT_table <- TSDT_table[TSDT_table$Strength %in% filter,]
  parsed_rules <- unlist(sapply(X = c(2:N),
         FUN = function(i){
           lhs <- unlist(strsplit(x = rules[i],
                                  split = " & " ))
           lhs <- sapply(X = lhs,
                         FUN = function(str){
                           if(length(grep(x = str, pattern = "%in%")) > 0) {
                             paste0(substr(x = str, start = 1, stop = gregexpr(text = str, pattern = "%in%")[[1]][1] - 1), "=")
                           } else if (length(grep(x = str, pattern = SPLIT_X)) > 0) {
                             substr(x = str, start = 1, stop = gregexpr(text = str, pattern = SPLIT_X)[[1]][1] - 1)
                           }
                         })
           rhs <- unlist(strsplit(x = gsub(x = cutoffs[i],
                                           pattern = PATTERN_REPLACE,
                                           replacement = ""),
                                  split = NUMBER_SEP))
           M <- length(lhs)
           r <-
             sapply(X = c(1:M),
                  FUN = function(j) {
                    if(length(grep(x = lhs[j], pattern = ">=")) > 0) {
                      left <- gsub(x = lhs[j], pattern = ">=", replacement = " >= ")
                    } else if (length(grep(x = lhs[j], pattern = "<=")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "<=", replacement = " <= ")
                    } else if (length(grep(x = lhs[j], pattern = "==")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "==", replacement = " == ")
                    } else if (length(grep(x = lhs[j], pattern = "=>")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "=>", replacement = " => ")
                    } else if (length(grep(x = lhs[j], pattern = "=<")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "=<", replacement = " => ")
                    } else if (length(grep(x = lhs[j], pattern = "<>")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "<>", replacement = " <> ")
                    } else if (length(grep(x = lhs[j], pattern = "!=")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "!=", replacement = " != ")
                    } else if (length(grep(x = lhs[j], pattern = "=")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "=", replacement = " = ")
                    } else if (length(grep(x = lhs[j], pattern = ">")) > 0) {
                      left <- gsub(x = lhs[j], pattern = ">", replacement = " > ")
                    } else if (length(grep(x = lhs[j], pattern = "<")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "<", replacement = " < ")
                    } else if (length(grep(x = lhs[j], pattern = "=")) > 0) {
                      left <- gsub(x = lhs[j], pattern = "=", replacement = " = ")
                    }
                    right <- sapply(X = strsplit(x = rhs[j], split = ","), FUN = as.double)
                    paste0(left, right)
                  })
           toString(unlist(r))

         }))
  N <- length(parsed_rules) # or M <- M - 1
  method_col <- rep("TSDT", N)
  outcome_col <- rep(outcome, N)
  desc_col <- paste0(TSDT_table$Strength[2:(N+1)], 
                     " (", 
                     round(-log(x = TSDT_table$Adjusted_Pvalue[2:(N+1)], base = 10)), 
                     ")"
              )
  param_text <- unlist(lapply(X = names(param), FUN = function(p) {
    paste(p, toString(param[[p]]))
  }))
  param_col <- rep(toString(param_text), N)
  
  res <- cbind(rule = parsed_rules,
               method = method_col,
               outcome = outcome_col,
               desc = desc_col,
               param = param_col)
  return(as.data.table(res))
}
