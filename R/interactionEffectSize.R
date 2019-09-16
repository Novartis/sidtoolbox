#' get interaction effect size for a rule
#'
#' This function allows you to calculate the= fitness for a given rule.
#' Fitness is what we try to minimize/maximize. Fitness in this version is
#' the effect size given by a model.
#' For numeric outcome, a linear model is calculate,
#' for binary a glm with family=binomial,
#' and for negative binomial a glm.nb model.
#' The model formula is:
#' The model formula is: Y ~ subgroup + treatment_contrast + subgroup*treatment_contrast
#' where cofficient on the interaction term is returned.
#' To fit to your project, overwrite this function to another that returns 2 scalars:
#' c(effect_size, p-value)
#' p-value is check to be significant while effect size is used for optimization.
#' Ideas to investigate: cohen's d as standardized effect size?
#' TODO: add pareto
#'
#' prerequisite:
#' dependencies: MASS
#' @param subgroup.dataset a list of data, covariates, outcomes and the trt, see loadDataset (list)
#' @param y_idx index of outcome to calculate measure on (integer)
#' @param subgroup_idx a logical vector indicating participation in subgroup
#' @return c(effect_size, p-value)
#' @export
interactionEffectSize <-
  function(subgroup.data,
           y_idx,
           y_type = NULL,
           subgroup_idx) {
    require(MASS)
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
    trt_idx <- data[[subgroup.data$contrast]] != subgroup.data$control
    formula <- Y ~ trt_idx + subgroup_idx + subgroup_idx * trt_idx
    # fit a model, lm for numeric, glm for binary and count
    
    res  <-
      tryCatch(
        expr = {
          if (y_type == "count") {
            # glm
            Y <- (as.numeric(Y))
            model <-
              glm.nb(formula)
          } else if (y_type == "binary") {
            model <-
              glm(formula,
                  family = "binomial")
          } else if (y_type == "numeric" || y_type == "continuous") {
            # linear model
            model <- lm(formula)
          } else{
            stop("what is Y?")
          }
          # get effect size for the interaction of subgroup_idx * trt_idx
          # always last coefficient
          coeff <- summary(model)$coefficients
          col <- nrow(coeff)
          c(coeff["trt_idxTRUE:subgroup_idxTRUE" , 1], coeff["trt_idxTRUE:subgroup_idxTRUE", 4])
        },
        error = function(e) {
          rep(NA, 2)
        }
      )
    return(res)
  }
