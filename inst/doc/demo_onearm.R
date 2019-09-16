## ----load_packaged, message=FALSE, warning=FALSE, include=FALSE----------
library(sidtoolbox)
library(data.table)
library(stringr)
library(ggplot2)


## ----simulate_data-------------------------------------------------------

# simulate a sample dataset with 3 types of variables, numeric, binary, and count
# covariates are X1, X2, ...
# outcomes are Y_[type], namely Y_numeric, Y_binary, and Y_count
# the contrast arm is TRT
# 'N' samples will be created 
# also places two subgroups, oneis true positive (random_TP) of size 'subgroup_ratio'*'N' where the treatment arm is enhanced by 'subgroup_enhanced_effect'
# and false positive (random_FP) of same size but where both arms are shifted by 'subgroup_shift_effect'
subgroup.data <- simulateSubgroupData(
                            N = 2000,
                            case = "prognostic", 
                            arm = "one",
                            overal_treatment_effect = 0.5, 
                            subgroup_enhanced_effect = 1.5, 
                            subgroup_ratio = 0.1,
                            covariates_normal = list(c(0,1), c(0,1), c(0,1)),
                            correlation = c(),
                            covariates_binary = c(0.5, 0.5, 0.5, 0.5), 
                            covariates_uniform = list(c(0,1), c(0,1), c(0,1)), 
                            has_FP = TRUE,
                            SD_NOISE_RATIO = 0.1)
# to load your own data use loadDataset function
# you would need to set your covariates and outcomes.
# for one arm study do not set the contrast parameter.
#loadDataset(dataset = subgroup.data, 
#            covariates = subgroup.data$covariates, 
#            outcomes = subgroup.data$outcomes, 
#            outcomeTypes = subgroup.data$outcomeTypes)


## ----set_global_variables------------------------------------------------
SUBGROUP_MIN_SIZE <- 0.1
MAX_DEPTH <- 2
P_THRESHOLD <- 0.05
MIN_P_VALUE <- 10^-10 # to avoid underflow


## ----exploreCorrelationMatrix, message=FALSE, warning=FALSE--------------

# examples
exploreTriangleCorrelationMatrix(subgroup.data = subgroup.data, method = "spearman")
exploreCorrelationMatrix(subgroup.data = subgroup.data)
exploreDetailedChartCorrelation(subgroup.data = subgroup.data)



## ----exploreAssociationRules, message=FALSE, warning=FALSE---------------

# res <- exploreAssociationRules(subgroup.data)
min_support <- 0.1 # how many samples support the LHS
min_confidence <- 0.5 # how many times RHS | LHS is true
min_order <- 2 # minimum depth of rules
max_order <- 4 # maximum depth of rules
paste("minimum support =", min_support, 
      "minimum confidence =", min_confidence,
      "minimum depth =", min_order,
      "maximum depth =", max_order)

a <- capture.output(p <- plotAssociationRules(subgroup.data = subgroup.data, onlyCovariates = TRUE, support = min_support, confidence = min_confidence, min_order = min_order, max_order = max_order))
p

a <- capture.output(p <- plotAssociationRules(subgroup.data = subgroup.data, method = "graph", measure = "confidence", onlyCovariates = TRUE, support = min_support, confidence = min_confidence, min_order = min_order, max_order = max_order))
p

a <- capture.output(p <- plotAssociationRules(subgroup.data = subgroup.data, method = "scatterplot", measure = "support", onlyCovariates = TRUE, support = min_support, confidence = min_confidence, min_order = min_order, max_order = max_order))
p

a <- capture.output(p <- plotAssociationRules(subgroup.data = subgroup.data, method = "two-key plot", measure = "support", onlyCovariates = FALSE, support = min_support, confidence = min_confidence, min_order = min_order, max_order = max_order))
p

#plotAssociationRulesInteractive(subgroup.data = subgroup.data, onlyCovariates = TRUE)
a <- capture.output(rules <- listAssociationRules(subgroup.data = subgroup.data, onlyCovariates = FALSE, support = min_support, confidence = min_confidence, min_order = min_order, max_order = max_order))

as.data.table(rules)


## ----exploreDecisionTree, message=FALSE, warning=FALSE-------------------
# on all coavriates
a <- lapply(X = subgroup.data$outcomes, FUN = function(y) {
  capture.output(p <- exploreConditionalInferenceTree(subgroup.data = subgroup.data, y_idx = y, stump = FALSE, testtype = "Bonferroni", significance = P_THRESHOLD, nmin = SUBGROUP_MIN_SIZE, maxdepth = MAX_DEPTH))
  plot(main = paste("all covariates - ", colnames(subgroup.data$data)[y]), p)
})


## ----subgroup_discovery, message=TRUE, warning=TRUE----------------------
# set the outcomes you want to run subgroup discovery on
outcome_columns <- colnames(subgroup.data$data)[subgroup.data$outcomes]
# save all rules in allRules
allRules <- subgroup.data$rules
allRules


## ----TSDT, message=TRUE, warning=TRUE------------------------------------
# the true positive / false positive subgroup is
# subgroup.data$rules

param <- list(
  desirable_response = "increasing",
  nsamples = 100, 
  npermutations = 100, # increase the permutation in a real world study 
  maxdepth = MAX_DEPTH, 
  min_subgroup_n_control = SUBGROUP_MIN_SIZE/2, 
  min_subgroup_n_trt = SUBGROUP_MIN_SIZE/2, 
  n_cpu = 1 # increase the number of CPU cores according to your machine
)

resTSDT <- lapply(X = outcome_columns, FUN = function(y_idx) {
  res <- runTSDT(subgroup.data = subgroup.data, 
                     y_idx = y_idx,
                     desirable_response = param$desirable_response,
                     nsamples = param$nsamples, 
                     npermutations = param$npermutations, 
                     maxdepth = param$maxdepth,
                     n_cpu = param$n_cpu)
  allRules <<- rbind(allRules, 
                    parseTSDTResults(TSDT_table = res, outcome = y_idx, param = param, filter = "Strong"))
  return(res)
})

resTSDT


## ----PSO, message=TRUE, warning=TRUE-------------------------------------


# increasing
resPSO <- lapply(X = outcome_columns,
                 FUN = function(y_idx){
  pso <- runPSO(subgroup.data = subgroup.data, 
                y_idx = y_idx,  
                desirable_response = "increasing", 
                depth = MAX_DEPTH, 
                nmin = SUBGROUP_MIN_SIZE, 
                iterations = 1000)
  allRules <<- rbind(allRules, 
                     pso)
  pso
})
resPSO



## ----compare_rules, echo=FALSE, message=FALSE, warning=FALSE-------------
threshold <- -log10(P_THRESHOLD)
max_p_value <- -log10(MIN_P_VALUE)
summarizeRules <- function(subgroup.data, rules) {
  N.rules <- nrow(rules)
  summaryRules <- data.table()
  
  allOutcomes <- outcome_columns
  
  for (i in c(1:N.rules)) {
    row <- rules[i]
    # adding a fix to remove dummy rules wihch were negative controls
    if (!grepl(x = row$rule, pattern = "dummy")) {
      if (str_length(row$outcome) == 0) {
        Y <- allOutcomes
      } else {
        Y <- row$outcome
      }
      for (y in Y) {
        row$outcome <- y
        summaryRules <- rbind(summaryRules, row)
      }
    }
    
  }
  N.rules <- nrow(summaryRules)
  N <- nrow(subgroup.data$data)
  effect_size <- sapply(
    X = c(1:N.rules),
    FUN = function(i) {
      row <- summaryRules[i]
      sbg_idx <-
        parseRule(covariates = subgroup.data, rule = row$rule)
      f <- effectSize(
        subgroup.data = subgroup.data,
        y_idx = row$outcome,
        subgroup_idx = sbg_idx
      )
      c(
        N_ratio = sum(sbg_idx) / N,
        effect = f[1],
        pvalue = f[2]
      )
    }
  )
  summaryRules$N_ratio <- effect_size[1, ]
  summaryRules$effect_size <- effect_size[2, ]
  summaryRules$pvalue <- effect_size[3, ]
  
  # check TP rule and get accuracy precision recall
  truth_subgroup <- which(rules$method == "sim_TP")
  if (length(truth_subgroup) > 0) {
    truth_subgroup <- parseRule(covariates = subgroup.data,
                                rule = rules[truth_subgroup[1], ]$rule)
    performance <- sapply(
      X = c(1:N.rules),
      FUN = function(i) {
        row <- summaryRules[i, ]
        sbg_idx <-
          parseRule(covariates = subgroup.data, rule = row$rule)
        TP <- sum(sbg_idx &  truth_subgroup)
        FP <- sum(sbg_idx & !truth_subgroup)
        TN <- sum(!sbg_idx & !truth_subgroup)
        FN <- sum(!sbg_idx &  truth_subgroup)
        precision <- TP / (TP + FP)
        recall <- TP / (TP + FN)
        F1  <- 2 * (recall * precision) / (precision + recall)
        c(precision = precision,
          recall = recall,
          F1 = F1)
      }
    )
    summaryRules$precision <- performance[1, ] * 100
    summaryRules$recall <- performance[2, ] * 100
    summaryRules$F1 <- performance[3, ] * 100
  }
  return(summaryRules)
}

rules.data <- summarizeRules(subgroup.data = subgroup.data, rules = allRules)
rules.data$log10pvalue <- -log10(rules.data$pvalue)
rules.data$log10pvalue[rules.data$log10pvalue > max_p_value] <- max_p_value


ggplot(
  data = rules.data,
  aes(
    x = precision,
    y = recall,
    shape = outcome,
    color = method,
    size = log10pvalue
  )
) +
geom_point() +
geom_jitter() + 
ggtitle("Correctness of rules")

ggplot(
  data = rules.data,
  aes(
    x = effect_size,
    y = log10pvalue,
    shape = outcome,
    color = method,
    size = N_ratio
  )
) +
geom_point() +
  geom_jitter() + 
ggtitle("Volcano plot of rules") +
geom_hline(yintercept = P_THRESHOLD) +
xlab("subgroup effect size") + 
ylab("-log10(pvalue)")


rules.data <- rules.data[, c("rule", "outcome", "N_ratio", "effect_size", "pvalue", "precision", "recall", "F1")]
as.data.table(rules.data)


