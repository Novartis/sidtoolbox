#' NVS ASE App
#'
#' @export
runSidApp <- function() {
  appDir <- system.file("shiny-examples", "sidApp", package = "sidtoolbox")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
