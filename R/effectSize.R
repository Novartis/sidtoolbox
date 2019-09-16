#' get aboslute fitness for a rule
#'
#' This function allows you to calculate the absolute fitness for a given rule.
#' Fitness is what we try to minimize/maximize. Fitness in this version is
#' cohen's d for numeric values and cohen's h for binary.
#'
#' prerequisite:
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param y_idx index of outcome to calculate measure on (integer or string)
#' @param subgroup_idx a logical vector indicating participation in subgroup
#' @return c(effect_size)
#' @export
effectSize <-
  function(subgroup.data,
           y_idx,
           y_type = NULL,
           subgroup_idx) {
    data <- subgroup.data$data
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
    Y <- data[[y_idx]]
    # one arm only
    res <- tryCatch(
      expr = {
        if (y_type == "numeric" || y_type == "continuous") {
          # cohen's d
          # subgroup's d
          #m1 <- median(Y[subgroup_idx], na.rm = TRUE)
          #m2 <- median(Y[!subgroup_idx], na.rm = TRUE)
          #var1 <- sd(Y[subgroup_idx], na.rm = TRUE)
          #var2 <- sd(Y[!subgroup_idx], na.rm = TRUE)
          #n1 <- sum(subgroup_idx)
          #n2 <- sum(!subgroup_idx)
          #pooled_sd <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2 ^ 2) / (n1 + n2 - 2))
          #cohens_d <- (m1 - m2) / pooled_sd
          t <- summary(lm(formula = Y ~ subgroup_idx))
          c(t$coefficients["subgroup_idxTRUE",1], t$coefficients["subgroup_idxTRUE",4])
          #return(cohens_d)
        } else if (y_type == "count") {
          # negative binomial
          t <- summary(glm.nb(formula = Y ~ subgroup_idx))
          c(t$coefficients["subgroup_idxTRUE",1], t$coefficients["subgroup_idxTRUE",4])
        } else if (y_type == "binary") {
          # cohen's h
          #Y <- Y[!is.na(Y)]
          #Y <- as.numeric(levels(Y))[Y]
          #p1 <-
          #  sum(Y[subgroup_idx]) / (length(Y[subgroup_idx]))
          #p2 <-
          #  sum(Y[!subgroup_idx]) / (length(Y[!subgroup_idx]))
          #cohens_h = 2 * (asin(p1) - asin(p2))
          #return(cohens_h)
          t <- summary(glm(Y ~ subgroup_idx, family = "binomial"))
          c(t$coefficients["subgroup_idxTRUE",1], t$coefficients["subgroup_idxTRUE",4])
        } else if (y_type == "survival") {
          t <- summary(survival::coxph(survival::Surv(Y, rep(1,length(Y))) ~ subgroup_idx))
          c(t$coefficients["subgroup_idxTRUE",1], t$coefficients["subgroup_idxTRUE",5])
        }
        else {
          stop("what is Y?")
        }
      }, error = function(e) {
        print(e)
        return(0)
      }
    )
    if(is.nan(res) || is.infinite(res) || is.na(res)) {
      return(0)
    }
    return(res)
  }
