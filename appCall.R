#' Run a shiny app showing how the DTedit function works.
#'
#' @export
dtedit_demo2 <- function() {
  dir <- getwd()  #paste0(find.package('DTedit'), '/shiny_demo')
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}
