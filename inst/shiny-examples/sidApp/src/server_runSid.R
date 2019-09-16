########################### run TSDT  ####################
output$TSDTTabs <- renderUI({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  TSDTTabs <- 
    lapply(X = outcomes, FUN = function(y){
      tabPanel(title = y,
               dataTableOutput(outputId = paste("TSDT", y, sep = "_")))
    })
  do.call(tabsetPanel, TSDTTabs)
})

observe({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  lapply(X = outcomes, FUN = function(y) {
    output[[paste("TSDT", y, sep = "_")]] <- renderDataTable({
      req(values$TSDT[[y]])
      return(values$TSDT[[y]])
    })
  })
})

shinyjs::onclick(id = "TSDT_save",
                 expr = {
                   req(values$TSDT)
                   for (y in names(values$TSDT)) {
                     filter <- input$TSDT_filter
                     rules <-
                       parseTSDTResults(TSDT_table = values$TSDT[[y]], 
                                        outcome = y, 
                                        filter = input$TSDTfilter,
                                        param = values$TSDTparam)
                     values$sid.data$rules <- rbind(values$sid.data$rules, rules)
                   }
                   shinyjs::hide(id = "TSDT_save")
                   shinyjs::hide(id = "TSDT_filter")
                 })


onclick(id = "runTSDT", expr = {
  req(values$sid.data$outcomes)
  req(values$sid.data$data)
  shinyjs::hide(id = "TSDT_save")
  shinyjs::hide(id = "TSDT_filter")
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  
  values$TSDT <- 
    lapply(X = outcomes, FUN = function(y){
      runTSDT(
        subgroup.data = values$sid.data, 
        y_idx = y,
        maxdepth = input$TSDTmaxdepth, 
        min_subgroup_n_control = input$min_subgroup_n_control,
        min_subgroup_n_trt = input$min_subgroup_n_trt,
        desirable_response = input$desirable_response, 
        nsamples = input$nsamples,
        npermutations = input$npermutations,
        n_cpu = 1
      )
  })
  names(values$TSDT) <- outcomes
  
  values$TSDTparam <- 
    list(
        maxdepth = input$TSDTmaxdepth, 
        min_subgroup_n_control = input$min_subgroup_n_control,
        min_subgroup_n_trt = input$min_subgroup_n_trt,
        desirable_response = input$desirable_response, 
        nsamples = input$nsamples,
        npermutations = input$npermutations
      )
  shinyjs::show(id = "TSDT_save")
  shinyjs::show(id = "TSDT_filter")
})

########################### run PSO  ####################

output$PSOTabs <- renderUI({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  PSOTabs <- 
    lapply(X = outcomes, FUN = function(y){
      tabPanel(title = y,
               dataTableOutput(outputId = paste("PSO", y, sep = "_")))
    })
  do.call(tabsetPanel, PSOTabs)
})

observe({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  lapply(X = outcomes, FUN = function(y) {
    output[[paste("PSO", y, sep = "_")]] <- renderDataTable({
      req(values$PSO[[y]])
      return(values$PSO[[y]])
    })
  })
})

shinyjs::onclick(id = "PSO_save",
                 expr = {
                   print(values$PSO)
                   for (y in names(values$PSO)){
                     values$sid.data$rules <- rbind(values$sid.data$rules, 
                                                    values$PSO[[y]])
                   }
                   shinyjs::hide(id = "PSO_save")
                 })

onclick(
  id = "runPSO",
  expr = {
    req(values$sid.data$outcomes)
    req(values$sid.data$data)
    shinyjs::hide(id = "PSO_save")
    if(is.na(input$abstol)) {
      abstol = -Inf
    }
    
    outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
    
    values$PSO <- lapply(X = outcomes, FUN = function(y) {
      print(y)
      runPSO(
        subgroup.data = values$sid.data, 
        y_idx = y,
        nmin = input$PSOnmin, 
        depth = input$PSOdepth, 
        desirable_response = input$direction, 
        swarm_size = input$swarm_size,
        iterations = input$iterations, 
        abstol = abstol, 
        reltol = input$reltol,
        maxit.stagnate = input$maxit.stagnate
      )
    })
    names(values$PSO) <- outcomes  
    shinyjs::show(id = "PSO_save")
})

########################### run Virtual Twins  ####################


output$VTTabs <- renderUI({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  req(values$sid.data$contrast)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  outcomeTypes <- values$sid.data$outcomeTypes
  outcomes <- outcomes[outcomeTypes == "binary"]
  VTTabs <- 
    lapply(X = outcomes, FUN = function(y){
      tabPanel(title = y,
               dataTableOutput(outputId = paste("VT", y, sep = "_")))
    })
  do.call(tabsetPanel, VTTabs)
})

observe({
  req(values$sid.data$data)
  req(values$sid.data$outcomes)
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  outcomeTypes <- values$sid.data$outcomeTypes
  outcomes <- outcomes[outcomeTypes == "binary"]
  
  lapply(X = outcomes, FUN = function(y) {
    output[[paste("VT", y, sep = "_")]] <- renderDataTable({
      req(values$VT[[y]])
      return(values$VT[[y]])
    })
  })
})

shinyjs::onclick(id = "VT_save",
                 expr = {
                   req(values$VT)
                   for (y in names(values$VT)) {
                     rules <-
                       parseVTResults(VT_table = values$VT[[y]], 
                                      outcome = y, 
                                      param = values$VTparam)
                     values$sid.data$rules <- rbind(values$sid.data$rules, rules)
                   }
                   
                   shinyjs::hide(id = "VT_save")
                 })


onclick(id = "runVirtualTwins", expr = {
  req(values$sid.data$outcomes)
  req(values$sid.data$data)
  shinyjs::hide(id = "VT_save")
  outcomes <- colnames(values$sid.data$data)[values$sid.data$outcomes]
  outcomeTypes <- values$sid.data$outcomeTypes
  outcomes <- outcomes[outcomeTypes == "binary"]
  
  values$VT <- 
    lapply(X = outcomes, FUN = function(y){
      runVirtualTwins(
        subgroup.data = values$sid.data, 
        y_idx = y,
        forest.type = input$forest.type, 
        tree.type = input$tree.type,
        folds = input$folds, 
        sampsize = input$sampsize, 
        maxdepth = input$VTmaxdepth, 
        method = input$VTmethod, 
        interactions = input$interactions, 
        ntree = input$ntree)
    })
  names(values$VT) <- outcomes
  
  values$VTparam <- 
    list(
      forest.type = input$forest.type, 
      tree.type = input$tree.type,
      folds = input$folds, 
      sampsize = input$sampsize, 
      maxdepth = input$VTmaxdepth, 
      method = input$VTmethod, 
      interactions = input$interactions, 
      ntree = input$ntree
    )
    shinyjs::show(id = "VT_save")
})
