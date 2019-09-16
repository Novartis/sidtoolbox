navbarMenu(
  "Identify subgroups",
  tabPanel('Run TSDT',
           sidebarLayout(sidebarPanel(
             width = 3,
             actionButton(inputId = "runTSDT", label = "Run TSDT"),
             hidden(
               selectInput(inputId = "TSDT_filter", 
                           label = "Filter by", 
                           choices = c("Strong", "Moderate", "Weak", "Not Confirmed", "N/A"),
                           selected = c("Strong", "Moderate", "Weak", "Not Confirmed", "N/A"),
                           multiple = TRUE),
               actionButton(inputId = "TSDT_save",
                            label = "Save rules")
             ),
             hr(),
             fluidRow(
               column(
                 width = 8,
                 tags$b("max tree depth")
               ),
               column(
                 width = 4,
                 numericInput(inputId = "TSDTmaxdepth", 
                              label = "", 
                              value = 2, 
                              min = 1)
               )
             ),
             sliderInput(inputId = "min_subgroup_n_control", 
                         label = "min_subgroup_n_control", 
                         min = 0, max = 1, value = 0.05),
             sliderInput(inputId = "min_subgroup_n_trt", 
                         label = "min_subgroup_n_control", 
                         min = 0, max = 1, value = 0.05),
             selectInput(inputId = "desirable_response", 
                         label = "desirable_response", 
                         choices = c("increasing", "decreasing"), 
                         selected = "increasing"),
             fluidRow(
               column(
                 width = 8,
                 tags$b("No. of subsamples (bootstrapping)")
               ),
               column(
                 width = 4,
                 numericInput(inputId = "nsamples", 
                              label = "",
                              min = 1, value = 10)
               )
             ),
             fluidRow(
               column(
                 width = 8,
                 tags$b("No. of permutaions")
               ),
               column(
                 width = 4,
                 numericInput(inputId = "npermutations", 
                              label = "",
                              min = 1, value = 10)
               )
             )
           ),
           mainPanel(
             h3("Treatment-specific Subgroup Detection Tool (TSDT)"),
               uiOutput(outputId = "TSDTTabs")
           ))),
  tabPanel('Run Virtual Twins',
           sidebarLayout(
             sidebarPanel(
               width = 3,
               actionButton(inputId = "runVirtualTwins", label = "Run VirtualTwins"),
               hidden(
                 actionButton(inputId = "VT_save",
                              label = "Save rules")
               ),
               hr(),
               numericInput(inputId = "VTmaxdepth", 
                            label = "maximum depth of rule", 
                            value = 2, 
                            min = 1),
               checkboxInput(inputId = "interactions", 
                             label = "interactions", 
                             value = TRUE),
               selectInput(inputId = "VTmethod", 
                           label = "method to calculate effect size", 
                           choices = c("absolute", "relative", "logit"), 
                           selected = "logit"),
               selectInput(inputId = "sens", 
                           label = "desirable_response", 
                           choices = c("increasing", "decreasing"), 
                           selected = "increasing"),
               selectInput(inputId = "forest.type", 
                           label = "forest.type", 
                           choices = c("one", "double", "fold"), 
                           selected = "double"),
               selectInput(inputId = "tree.type", 
                           label = "tree.type", 
                           choices = c("reg", "class"), 
                           selected = "class"),
               numericInput(inputId = "folds", 
                            label = "No. of folds", 
                            value = 10, min = 1, 
                            max = 100),
               numericInput(inputId = "ntree", 
                            label = "No. of trees in random forest model", 
                            value = 1000, 
                            min = 1),
               sliderInput(inputId = "sampsize", 
                           label = "sampling ratio to balance data", 
                           value = 1, min = 0.5, max = 2)
             ),
             mainPanel(
               h3("Virtual Twins"),
               uiOutput(outputId = "VTTabs")
             ))),
  tabPanel('Run PSO',
           sidebarLayout(sidebarPanel(
             actionButton(inputId = "runPSO", label = "Run PSO"),
             hidden(
               actionButton(inputId = "PSO_save",
                            label = "Save rules")
             ),
             hr(),
             selectInput(inputId = "PSOmethod", 
                         label = "fitness method", 
                         choices = PSO_METHODS,
                         selected = PSO_METHODS[1]),
             conditionalPanel(
               condition = "input.PSOmethod == 'manual'",
               textAreaInput(inputId = "manualPSOfunction", 
                             label = "Enter manual function",
                             value = paste("fitness_function <- function(subgroup.data, y_idx, subgroup.data) {", 
                                           "# must return c(effect_size, p_value)",
                                           "}", sep = "\n")
                           )
             ),
             sliderInput(inputId = "PSOnmin", 
                         label = "minimum subgroup ratio", 
                         value = 0.1, 
                         min = 0, max = 1),
             numericInput(inputId = "PSOdepth", 
                         label = "maximum depth of rule", 
                         min = 0, max = 10, 
                         value = 2),
             selectInput(inputId = "direction", 
                         label = "direction of response", 
                         choices = c("increasing", "decreasing"), 
                         selected = "increasing"),
             numericInput(inputId = "swarm_size", 
                          label = "swarm size", 
                          value = 40, 
                          min = 10, 
                          max = 100),
             numericInput(inputId = "iterations", 
                          label = "iterations", 
                          value = 1000, 
                          min = 10, 
                          max = 1000), 
             numericInput(inputId = "abstol", 
                          label = "absolute convergance tolerance", 
                          value = -Inf, 
                          min = -Inf, 
                          max = 0),
             numericInput(inputId = "reltol", 
                          label = "restarting tolerance", 
                          value = 0),
             numericInput(inputId = "maxit.stagnate", 
                          label = "maximum iteration with no improvement", 
                          value = 100)
           ),
           mainPanel(
             h3("Partical Swarm Optimization (PSO)"),
             uiOutput(outputId = "PSOTabs")
           )))
) #navBarMenu
