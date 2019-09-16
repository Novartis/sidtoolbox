navbarMenu(
  "Explore",
  tabPanel(title = 'Correlations',
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "correlationMethod",
                 label = "Correlation method",
                 choices = CORRELATION_METHOD,
                 selected = CORRELATION_METHOD[1],
                 multiple = FALSE
               )
             ),
             mainPanel(
               h3("Correlation"),
               HTML(paste("<p>Association rules help us find high level correlations in the data.", 
                          "The rules are controled by support (how large the corresponding subgroup is) and confidence (how many of the total subgroup size follow the rule).", 
                          "Support shows us how frequently the rule (left hand side) is in the data, similar to subgroup size. Confidence is the number of times the rule is correct (right hand side | left hand side), and thus is a measure of strength for the rule. The minimum and maximum depth of the rule can be set by min_order and max_order.",
                          "By filtering RHS (right hand side) for the outcome variables you can find immediate subgroups with highest support.</p>")),
               tabsetPanel(
                 id = "exploreCorrelation",
                 tabPanel("matrix",
                          plotOutput(outputId = "correlationMatrix")),
                 tabPanel("triangle",
                          plotOutput(outputId = "correlationTriangle")),
                 tabPanel("2x2",
                          plotOutput(outputId = "detailedChart"))
               )
             )
           )),
  tabPanel(title = 'Association Rules',
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 inputId = "support",
                 label = "support",
                 min = 0,
                 max = 1,
                 value = 0.1
               ),
               sliderInput(
                 inputId = "confidence",
                 label = "confidence",
                 min = 0,
                 max = 1,
                 value = 0.5
               ),
               numericInput(
                 inputId = "topN",
                 label = "No. of top rules",
                 value = 10,
                 min = 1
               ),
               selectInput(
                 inputId = "filterBy",
                 label = "measure to filter by",
                 choices = MEASURE_FILTER_BY,
                 selected = MEASURE_FILTER_BY[1]
               ),
               checkboxInput(
                 inputId = "onlyCovariates",
                 label = "Exclude outcomes",
                 value = TRUE
               )
             ),
             mainPanel(
               h3("Association Rules"),
               tabsetPanel(
                 id = "associationRules",
                 tabPanel("table",
                          dataTableOutput(outputId = "associationTable")),
                 tabPanel("associationGraph",
                          plotOutput(outputId = "associationGraph")),
                 #tabPanel("associationInteractive",
                 #          plotOutput(outputId = "associationInteractive")),
                 tabPanel(
                   "associationScatter",
                   plotOutput(outputId = "associationScatter")
                 ),
                 tabPanel("associationDepth",
                          plotOutput(outputId = "associationDepth"))
               )
             )
           )),
  tabPanel(title = 'Interaction trees',
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "testtype",
                 label = "test type",
                 choices = CORRELATION_INFERENCE_TEST_TYPES,
                 selected = CORRELATION_INFERENCE_TEST_TYPES[1]
               ),
               selectInput(
                 inputId = "teststat",
                 label = "test statistic",
                 choices = CORRELATION_INFERENCE_TEST_STATS,
                 selected = CORRELATION_INFERENCE_TEST_STATS[1]
               ),
               checkboxInput(
                 inputId = "stump",
                 label = "stump tree",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "treeSignificance",
                 label = "significance",
                 min = 0,
                 max = 0.5,
                 value = 0.025
               ),
               sliderInput(
                 inputId = "minLeafSize",
                 label = "minimum subgroup ratio",
                 min = 0,
                 max = 1,
                 value = 0.1
               ),
               numericInput(
                 inputId = "treemaxdepth",
                 label = "maximum depth of rule",
                 min = 1,
                 value = 4
               )
             ),
             mainPanel(
               h3("Conditional interaction trees"),
               uiOutput(outputId = "inferenceTree")
           )))
) #navBarMenu
