
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(
  navbarPage (
    'sidApp',
    source(file.path("src/ui_about.R"),  local = TRUE)$value,
    source(file.path("src/ui_setup.R"),  local = TRUE)$value,
    source(file.path("src/ui_explore.R"),  local = TRUE)$value,
    source(file.path("src/ui_runSid.R"),  local = TRUE)$value,
    source(file.path("src/ui_analysis.R"),  local = TRUE)$value
    #source(file.path("src/ui_save.R"),  local = TRUE)$value
  )
)
