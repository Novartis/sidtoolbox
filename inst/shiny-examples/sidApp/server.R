server <- function(input, output, session) {
  source("src/server_setup.R",  local = TRUE)
  source("src/server_explore.R",  local = TRUE)
  source("src/server_runSid.R",  local = TRUE)
  source("src/server_analysis.R",  local = TRUE)
}
