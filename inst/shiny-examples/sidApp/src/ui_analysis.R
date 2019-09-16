navbarMenu(
  "Analysis",
  tabPanel(title = 'Rules summary',
           sidebarLayout(
             sidebarPanel(
               uiOutput(outputId = "ruleOutcomesUI"),
              uiOutput(outputId = "ruleFeaturesUI"), 
              width = 3
             ),
             mainPanel(
               tabsetPanel(
                 id = "rulesAnalysis",
                 tabPanel("rules summary",
                          dataTableOutput(outputId = "rulesSummary")),
                 tabPanel("plot",
                          plotOutput(outputId = "plotRules", brush = "brushRules"),
                          dataTableOutput(outputId = "selectedRulesTable"))
               )
             )
           ))
) #navBarMenu
