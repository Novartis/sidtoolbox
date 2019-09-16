output$ruleOutcomesUI <- renderUI({
  req(values$sid.data$rules)
  req(length(values$sid.data$rules) > 0)
  if (length(values$sid.data) > 0 && nrow(values$sid.data$data) > 0) {
    columns <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  } else {
    columns <- c()
  }
  selectInput(
    inputId = "selectOutcome",
    label = "Select outcome",
    choices = columns,
    selected = columns,
    multiple = TRUE
  )
})

output$ruleFeaturesUI <- renderUI({
  req(ruleFeatures())
  ruleColumns <- ruleFeatures()
  plotLabels <- c(rulesX = "X-axis", 
                    rulesY = "Y-axis", 
                    pointSize = "point size", 
                    pointColor = "point color", 
                    pointShape = "point shape")
  plotId <- c("rulesX", 
                    "rulesY", 
                    "pointSize", 
                    "pointColor", 
                    "pointShape")
  selected <- c("N_ratio", "pvalue", "effect", "method", "outcome")
  
  lapply(X = c(1:length(plotId)), FUN = function(x) { 
    selectInput(
      inputId = plotId[x],
      label = plotLabels[x],
      choices = ruleColumns,
      selected = selected[x]
    )
  })
})

summarizeRules <- reactive(function(){
  summarizeRules(subgroup.data = values$sid.data)
})

ruleFeatures <- reactive({
  req(values$sid.data$rules)
  req(length(values$sid.data$rules) > 0)
  colnames(summarizeRules(values$sid.data))
})


output$rulesSummary <- renderDataTable({
  req(values$sid.data$rules)
  req(length(values$sid.data$rules) > 0)
  rules <- summarizeRules(subgroup.data = values$sid.data)
  rules <- rules[rules$outcome %in% input$selectOutcome,]
})

output$plotRules <- renderPlot({
  req(values$sid.data$rules)
  req(length(values$sid.data$rules) > 0)
  data <- summarizeRules(subgroup.data = values$sid.data)
  data <- data[data$outcome %in% input$selectOutcome, ]
  ggplot(
    data = data,
    aes_string(
      x = input$rulesX,
      y = input$rulesY,
      shape = input$pointShape,
      color = input$pointColor,
      size = input$pointSize
    )
  ) +
    geom_point() +
    ggtitle("Summary of rules") +
    xlab(input$rulesX) +
    ylab(input$rulesY)
})

output$selectedRulesTable = renderDataTable({
  brush <- input$brushRules
  req(!is.null(brush))
  req(values$sid.data$rules)
  req(length(values$sid.data$rules) > 0)
  rules <- summarizeRules(subgroup.data = values$sid.data)
  idx <- (rules[, input$rulesX, with = F] >= brush$xmin &
          rules[, input$rulesX, with = F] <= brush$xmax &
          rules[, input$rulesY, with = F] >= brush$ymin &
          rules[, input$rulesY, with = F] <= brush$ymax)
  rules[which(idx), ]
})

summarizeRules <- function(subgroup.data) {
  rules <- subgroup.data$rules
  N <- nrow(rules)
  summaryRules <- data.table()
  
  allOutcomes <-
    (colnames(subgroup.data$data)[subgroup.data$outcomes])
  for (i in c(1:N)) {
    row <- rules[i,]
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
  M <- nrow(summaryRules)
  N <- nrow(subgroup.data$data)
  effect_size <- sapply(
    X = c(1:M),
    FUN = function(i) {
      row <- summaryRules[i,]
      sbg_idx <-
        parseRule(covariates = subgroup.data, rule = row$rule)
      if ("contrast" %in% names(subgroup.data)) {
        f <- interactionEffectSize(
          subgroup.data = subgroup.data,
          y_idx = row$outcome,
          subgroup_idx = sbg_idx
        )
      } else {
        f <- effectSize(
          subgroup.data = subgroup.data,
          y_idx = row$outcome,
          subgroup_idx = sbg_idx
        )
      }
      
      c(
        N_ratio = sum(sbg_idx) / N,
        effect = f[1],
        pvalue = f[2]
      )
    }
  )
  summaryRules$N_ratio <- round(effect_size[1, ], 2)
  summaryRules$effect <- round(effect_size[2, ], 2)
  summaryRules$pvalue <-
    round(-log(x = effect_size[3, ], base = 10), 2)
  
  # check TP rule and get accuracy precision recall
  truth_subgroup <- which(rules$method == "sim_TP")
  if (length(truth_subgroup) > 0) {
    truth_subgroup <- parseRule(covariates = subgroup.data,
                                rule = rules[truth_subgroup[1], ]$rule)
    performance <- sapply(
      X = c(1:M),
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
    summaryRules$precision <- round(performance[1, ] * 100, 2)
    summaryRules$recall <- round(x = performance[2, ] * 100, 2)
    summaryRules$F1 <- round(performance[3, ] * 100, 2)
  }
  return(summaryRules)
}
