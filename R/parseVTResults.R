#' parseVTResults
#' parse Virtual Twins table result return rule structure
#'
#' This function parses a data.table returned by runTSDT.
#' A rule structure has at least 5 columns: rule, method, outcome, desc, and param
#' The rule can be parsed by parseRule to get subgroup
#' @param VT_table a data.table of results returned by TSDT [data.table]
#' @param outcome name of outcome column that TSDT was run on [character]
#' @param parameters used to run TSDT, saved as log [list or character]
#' @return a rule structure [data.table]
#' @export
parseVTResults <- function(VT_table, outcome, param) {
  library(data.table)
  N <- nrow(VT_table)
  rules <- VT_table$Subgroup
  desc_col <- paste0(VT_table$`RR (resub)`, "/", VT_table$`RR (snd)`)
  method_col <- rep("VirtualTwins", N)
  outcome_col <- rep(outcome, N)
  param_text <- unlist(lapply(X = names(param), FUN = function(p) {
    paste(p, toString(param[[p]]))
  }))
  param_col <- rep(toString(param_text), N)
  
  res <- cbind(rule = rules,
               method = method_col,
               outcome = outcome_col,
               desc = desc_col,
               param = param_col)
  return(as.data.table(res))
}
