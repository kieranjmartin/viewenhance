#' Shiny view of data
#' @export
#' @import shiny
#' @import miniUI
#' @param datain Provide a data frame or a list of data frames to explore
#' @return A shiny box, which, when options are chosen, will put a View command into the console

viewenhanceAddin<- function(datain = NULL) {


  datalist <-datacheck(datain)

  ui <- gen_ui(datain, datalist)
  server <- server_in
  environment(server) <- environment()


  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  #note we suppress messages. We remove this when debugging :)
  #suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))
  runGadget(ui, server, viewer = viewer)

}
