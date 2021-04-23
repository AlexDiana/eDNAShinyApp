#' @export
runeDNAShinyApp <- function() {
  appDir <- system.file("shinyapp", package = "eDNAShinyApp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}