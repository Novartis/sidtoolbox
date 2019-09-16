navbarMenu("Setup",
           useShinyjs(),
           tabPanel(title = "Setup subgroup data",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        id = "simulateDiv",
                        show = FALSE,
                        fileInput(
                          inputId = "dataPath",
                          label = "path to file",
                          placeholder = "",
                          multiple = FALSE
                        ),
                        actionButton(inputId = "loadData", label = "Load data"),
                        hr(),
                        actionButton(inputId = "simulateData",
                                     label = "Simulate data"),
                        fluidRow(column(width = 6,
                                        tags$b("No. of samples")),
                                 column(
                                   width = 6,
                                   numericInput(
                                     inputId = "N_samples",
                                     label = "",
                                     value = 2000,
                                     min = 100,
                                     step = 100
                                   )
                                 )),
                        fluidRow(column(
                          width = 12,
                          sliderInput(
                            inputId = "treatment_p",
                            label = "ratio of treatment arm",
                            min = 0,
                            max = 1,
                            value = 0.5
                          )
                        )),
                        fluidRow(column(width = 6,
                                        tags$b("simulation case")),
                                 column(
                                   width = 6,
                                   selectInput(
                                     inputId = "case",
                                     label = "",
                                     choices = SIMULATE_CASE,
                                     selected = SIMULATE_CASE[1],
                                     multiple = FALSE,
                                     selectize = TRUE
                                   )
                                 )),
                        fluidRow(column(
                          width = 6,
                          tags$b("background treatment effect")
                        ),
                        column(
                          width = 6,
                          numericInput(
                            inputId = "overal_treatment_effect",
                            label = "",
                            min = -10,
                            max = 10,
                            value = 1,
                            step = 1
                          )
                        )),
                        sliderInput(
                          inputId = "subgroup_ratio",
                          label = "subgroup ratio",
                          min = 0,
                          max = 1,
                          value = 0.1,
                          step = 0.1
                        ),
                        numericInput(
                          inputId = 'rule_depth',
                          label = "rule depth",
                          value = 2
                        ),
                        fluidRow(column(
                          width = 6,
                          tags$b("subgroup enhanced effect")
                        ),
                        column(
                          width = 6,
                          numericInput(
                            inputId = "subgroup_enhanced_effect",
                            label = "",
                            min = -10,
                            max = 10,
                            value = 1,
                            step = 1
                          )
                        )),
                        fluidRow(
                          column(
                            width = 6,
                            checkboxInput(
                              inputId = "has_FP",
                              label = "include false positive rule",
                              value = TRUE
                            )
                          ),
                          column(
                            width = 6,
                            conditionalPanel(
                              condition = "input.has_FP",
                              numericInput(
                                inputId = "subgroup_shift_effect",
                                label = "False positive shift",
                                min = -10,
                                max = 10,
                                value = 1,
                                step = 1
                              )
                            )
                          )
                        ),
                        
                        tags$b("Set covariates:"),
                        fluidRow(
                          column(
                            width = 6,
                            tags$b("Normal covariates"),
                            HTML("Enter (mean, sd) row by row:")
                          ),
                          column(
                            width = 6,
                            textAreaInput(
                              inputId = "covariates_normal",
                              label = "",
                              value = "(0,1)\n(1,2)"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            tags$b("Uniform covariates"),
                            HTML("Enter (min, max) row by row:")
                          ),
                          column(
                            width = 6,
                            textAreaInput(
                              inputId = "covariates_uniform",
                              label = "",
                              value = "(0,1)\n(1,2)"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            tags$b("Binomial covariates"),
                            HTML("Enter (size, probaility) row by row:")
                          ),
                          column(
                            width = 6,
                            textAreaInput(
                              inputId = "covariates_binomial",
                              label = "",
                              value = "(10,0.5)"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            tags$b("Binary covariates"),
                            HTML("Enter probaility row by row:")
                          ),
                          column(
                            width = 6,
                            textAreaInput(
                              inputId = "covariates_binary",
                              label = "",
                              value = "0.5\n0.6"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            tags$b("Correlated normal covariates"),
                            HTML("Enter ratio row by row. ")
                          ),
                          column(
                            width = 6,
                            textAreaInput(
                              inputId = "correlation",
                              label = "",
                              value = "0.2"
                            )
                          )
                        ),
                        sliderInput(
                          inputId = "SD_NOISE_RATIO",
                          label = "random noise",
                          min = 0,
                          max = 1,
                          value = 0.1
                        )
                      ),
                      mainPanel(
                        h4("Setup subgroup data"),
                        tabsetPanel(
                          id = "setupFeatures",
                          tabPanel(
                            title = "Select features",
                            htmlOutput(outputId = "featureSelectHelp"),
                            hidden(
                              actionButton(inputId = "SaveFeatureSelect",
                                           label = "Save feature settings")
                            ),
                            uiOutput(outputId = "renderFeatureOutputs")
                          ),
                          tabPanel(
                            "Summary of features",
                            verbatimTextOutput(outputId = "featureSummary")
                          ),
                          tabPanel(title = "Implanted subgroups",
                                   dataTableOutput(outputId = "ruleSummary"))
                        )
                      )
                    )))
