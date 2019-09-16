output$inferenceTree <- renderUI({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  inferenceTreeTabs <- 
    lapply(X = outcomes, FUN = function(y){
      tabPanel(title = y,
             plotOutput(outputId = paste("inferenceTree", y, sep = "_")))
    })
  do.call(tabsetPanel, inferenceTreeTabs)
})

observe({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  lapply(X = outcomes, FUN = function(y) {
    output[[paste("inferenceTree", y, sep = "_")]] <- renderPlot({
      print(y)
      p <- exploreConditionalInferenceTree(
        subgroup.data = values$sid.data, 
        y_idx = y,
        stump = input$stump, 
        testtype = input$testtype, 
        teststat = input$teststat, 
        significance = input$treeSignificance, 
        nmin = input$minLeafSize, 
        maxdepth = input$treemaxdepth)
      plot(p, main = y)
    })
  })
})


output$associationTable <- renderDataTable({
  req(values$sid.data)
  req(values$sid.data$data)
  res <- listAssociationRules(subgroup.data = values$sid.data, 
                       support = input$support, 
                       confidence = input$confidence, 
                       filterby = input$filterBy, 
                       top = input$topN,
                       onlyCovariates = input$onlyCovariates)
  return(as.data.table(res))
})
output$associationGraph <- renderPlot({
  plotAssociationRules(subgroup.data = values$sid.data, 
                       method = "graph", 
                       support = input$support, 
                       confidence = input$confidence, 
                       measure = input$filterBy,
                       onlyCovariates = input$onlyCovariates)
})
#output$associationInteractive <- renderPlot({
#  plotAssociationRulesInteractive(subgroup.data = values$sid.data, 
#                       support = input$support, 
#                       confidence = input$confidence)
#})
output$associationScatter <- renderPlot({
  plotAssociationRules(subgroup.data = values$sid.data, 
                       method = "scatterplot", 
                       support = input$support, 
                       confidence = input$confidence,
                       measure = input$filterBy,
                       onlyCovariates = input$onlyCovariates)
})
output$associationDepth <- renderPlot({
  plotAssociationRules(subgroup.data = values$sid.data, 
                       method = "two-key plot", 
                       support = input$support, 
                       confidence = input$confidence,
                       measure = input$filterBy)
})

######### correlation
output$correlationMatrix <- renderPlot({
  req(values$sid.data$data)
  exploreCorrelationMatrix(subgroup.data = values$sid.data,
                           method = input$correlationMethod)
})
output$correlationTriangle <- renderPlot({
  req(values$sid.data)
  exploreTriangleCorrelationMatrix(subgroup.data = values$sid.data, 
                           method = input$correlationMethod)
})
output$detailedChart <- renderPlot({
  exploreDetailedChartCorrelation(subgroup.data = values$sid.data)
})
