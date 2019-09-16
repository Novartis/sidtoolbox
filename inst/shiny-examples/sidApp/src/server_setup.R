output$featureSelectHelp <- renderUI({
  if (length(values$sid.data) > 0) {
    tags$div(
      tags$p(paste("Select the contrast, covariates, and outcomes. ",
                   "If this is a one arm study set contrast to empty, otherwise",
                   "select the control arm in the treatment column.",
                   "After selecting outcomes, please specify the type of each.",
                   "After setting all variables, click on"),
             tags$i("Save feature setting"),
             paste(".")
      )
    )
  } else {
    tags$div(
      tags$p(paste("You can start by loading data from file ",
                   "or simulating sample data and implanting subgroups.", 
                   "Simulation can be used for benchmarking purposes and features will be set automatically.")
      )
    )
  }
})
output$renderFeatureOutputs <- renderUI({
  # update feature panel
  if(length(values$sid.data) == 0) {
    return()
  }
  
  features <- colnames(values$sid.data$data)
  
  if("contrast" %in% names(values$sid.data)) {
    trt <- features[values$sid.data$contrast]
    controls <- unique(values$sid.data$data[, trt, with = FALSE])
    if ("control" %in% names(values$sid.data)) {
      control_selected <- values$sid.data$control
    } else {
      control_selected <- NULL
    }
  } else {
    trt <- ""
    controls <- c()
    control_selected <- NULL
  }

  if("covariates" %in% names(values$sid.data)) {
    covariates <- features[values$sid.data$covariates]
  } else {
    covariates <- c()
  }
  
  if("outcomes" %in% names(values$sid.data)) {
    outcomes <- features[values$sid.data$outcomes]
  } else {
    outcomes <- c()
  }
  
  tags$div(
    id = "featureSelectDiv",
    fluidRow(
      column(
        width = 6,
        selectInput(inputId = "selectContrast", 
                    label = "select contrast",
                    choices = c("", setdiff(features, c(covariates, outcomes))),
                    selected = trt)
      ),
      column(
        width = 6,
        selectInput(inputId = "selectControl", 
                    label = "control",
                    choices = controls,
                    selected = control_selected)
        )
    ),
    fluidRow(
      column(
        width = 4,
        multiInput(inputId = "selectCovariates", 
                   label = "select covariates",
                   choices = setdiff(features, c(trt, outcomes)),
                   selected = covariates)
      ),
      column(
        width = 4,
        multiInput(inputId = "selectOutcomes", 
                   label = "select outcomes",
                   choices = setdiff(features, c(trt, covariates)),
                   selected = outcomes)
      ),
      column(
        width = 4,
        uiOutput(outputId = "selectOutcomesTypes")
      )
    )
    
  )
})

output$selectOutcomesTypes <- renderUI({
  req(input$selectOutcomes)
  outcomes <- input$selectOutcomes
  req(!is.null(outcomes) && length(outcomes) > 0)
  req("data" %in% names(values$sid.data))
  data <- values$sid.data$data
  req(nrow(data) > 0 && ncol(data) > 0)
  idx <- 0
  ui <- lapply(X = outcomes, FUN = function(y){
    col <- data[, y, with = FALSE][[1]]
    idx <<- idx + 1
    if ("outcomeTypes" %in% names(values$sid.data) &&
        length(values$sid.data$outcomeTypes) >= idx) {
      guess_type <- values$sid.data$outcomeTypes[idx]
    } else if (is.factor(col) || length(levels(col)) == 2) {
      guess_type  <- "binary"
    } else if (length(unique(col)) == 2) {
      guess_type  <- "binary"
    } else if ((is.numeric(col) || is.integer(col)) && round(col) == col) {
      guess_type  <- "count"
    } else {
      guess_type <- "numeric"
    }
    OUTCOME_TYPES <- c("numeric", "count", "survival", "binary")
    selectInput(inputId = paste("type", y, sep = "_"), 
                label = paste("Type of", y), 
                choices = OUTCOME_TYPES, 
                selected = guess_type, 
                multiple = FALSE)
  })
  updateFeatureSelect()
  ui
})

onevent(
  event = "mouseleave",
  id = "selectContrast",
  expr = {
    trt <- input$selectContrast
    features <- colnames(values$sid.data$data)
    if (trt %in% features) {
      ctrl <- unique(as.vector(values$sid.data$data[, trt, with = FALSE][[1]]))
      updateSelectInput(
        session, 
        inputId = "selectControl", 
        choices = ctrl, selected = ctrl[1])
    } else {
      updateSelectInput(
        session, 
        inputId = "selectControl", 
        choices = c(), selected = c())
    }
    updateFeatureSelect()
  }
)

onevent(
  event = "mouseup",
  id = "selectCovariates",
  expr = {
    updateFeatureSelect()
  }
)
onevent(
  event = "mouseup",
  id = "selectOutcomes",
  expr = {
    updateFeatureSelect()
  }
)

updateFeatureSelect <- function () {
  features <- colnames(values$sid.data$data)
  updateSelectInput(session, 
                    inputId = "selectContrast", 
                    choices = c("", setdiff(features, c(input$selectCovariates, input$selectOutcomes))),
                    selected = input$selectContrast)

  updateMultiInput(session, 
                   inputId = "selectCovariates", 
                   choices = setdiff(features, c(input$selectContrast, input$selectOutcomes)),
                   selected = input$selectCovariates)
  
  updateMultiInput(session, 
                   inputId = "selectOutcomes", 
                   choices = setdiff(features, c(input$selectContrast, input$selectCovariates)),
                   selected = input$selectOutcomes)
}
    

onclick(
  id = "SaveFeatureSelect", 
  expr = {
    req(values$sid.data$data)
    data <- values$sid.data$data
    
    if(nrow(data) == 0 || ncol(data) == 0) {
      shinyjs::alert(text = "Error: dataset is empty.")
      return()
    }
    outcomes <- input$selectOutcomes
    covariates <- input$selectCovariates
    rules <- values$sid.data$rules
    trt <- input$selectContrast
    ctrl <- input$selectControl
    
    if (is.null(outcomes) || length(outcomes) == 0) {
      shinyjs::alert(text = "Error: outcomes are empty.")
      return()
    }
    if (is.null(covariates) || length(covariates) == 0) {
      shinyjs::alert(text = "Error: covariates are empty.")
      return()
    }
    
    outcome_types <- sapply(X = outcomes, FUN = function(y){
      input[[paste("type", y, sep = "_")]]
    })
    values$sid.data <-
      loadDataset(dataset = data, 
                  trt = trt, 
                  ctrl = ctrl,
                  covariates = covariates,
                  outcomes = outcomes,
                  outcomeTypes = outcome_types,
                  rules = rules)
})

shinyjs::onclick(
  id = "loadData", 
  expr = {
    values$sid.data <- list()
    tryCatch(
      expr = {
        print(as.character(input$dataPath$datapath))
        data <- fread(as.character(input$dataPath$datapath), stringsAsFactors = T)
        # remove missing data
        idx <- apply(X = data, MARGIN = 1, FUN = function(row){
          sum(is.na(row)) == 0
        })
        data <- data[idx,]
        showNotification(paste0("data loaded dim=", dim(data)))
        values$sid.data$data <- data
        shinyjs::show("SaveFeatureSelect")
      }, 
      error = function(e) {
        print(e)
      }
    )
  }
)

onclick(
  id = "simulateData",
  expr = {
    values$sid.data <- list()
    # parse covariates_normal areatext
    str_normal <- unlist(strsplit(
      x = str_replace_all(string = input$covariates_normal, 
                          pattern = "[()]", 
                          replacement = ""),
      split = "\n"))
    covariates_normal <-
      lapply(X = str_normal, FUN = function(str){
        as.numeric(unlist(strsplit(x = str, split = ",")))
    })

    # parse covariates_uniform areatext
    str_uniform <- unlist(strsplit(
      x = str_replace_all(string = input$covariates_uniform, 
                          pattern = "[()]", 
                          replacement = ""),
      split = "\n"))
    covariates_uniform <-
      lapply(X = str_uniform, FUN = function(str){
        as.numeric(unlist(strsplit(x = str, split = ",")))
      })

    # parse covariates_binomial areatext
    str_binomial <- unlist(strsplit(
      x = str_replace_all(string = input$covariates_binomial, 
                          pattern = "[()]", 
                          replacement = ""),
      split = "\n"))
    covariates_binomial <-
      lapply(X = str_binomial, FUN = function(str){
        as.numeric(unlist(strsplit(x = str, split = ",")))
      })

    # parse covariates_binary areatext
    covariates_binary <- 
      as.numeric(
        unlist(
          strsplit(x = input$covariates_binary, 
                   split = "\n")))

    # parse correlation areatext
    correlation <- 
      as.numeric(
        unlist(
          strsplit(x = input$correlation, 
                   split = "\n")))
    
    values$sid.data <- 
      simulateSubgroupData(
        N = input$N_samples,
        case = input$case, 
        overal_treatment_effect = input$overal_treatment_effect, 
        subgroup_enhanced_effect = input$subgroup_enhanced_effect, 
        subgroup_shift_effect = input$subgroup_shift_effect,
        subgroup_ratio = input$subgroup_ratio, 
        covariates_normal = covariates_normal,
        covariates_uniform = covariates_uniform, 
        covariates_binomial = covariates_binomial, 
        covariates_binary = covariates_binary, 
        treatment_p = input$treatment_p, 
        rule_depth = input$rule_depth,
        correlation = correlation, 
        SD_NOISE_RATIO = input$SD_NOISE_RATIO, 
        has_FP = input$has_FP
      )

    shinyjs::show("SaveFeatureSelect")
  }
)

output$featureSummary <- renderPrint({
  if("data" %in% names(values$sid.data) &&
     nrow(values$sid.data$data) > 0) {
    summary(values$sid.data$data)
  } else {
    "No data loaded."
  }
})

output$ruleSummary <- renderDataTable({
  # TODO fix this
  rules <- values$sid.data$rules
  req(nrow(rules) > 0)
  rules[rules$method %in% c("sim_TP", "sim_FP"), ]
})

