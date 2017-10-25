#' colselecter_text
#'
#' Convenience function to clean code below
#' Designed to provide a text argument for the column selections
#' @param inval either Starting with or Ending with or Containing
colselecter_text <- function(inval){
  paste0("Select columns ",
         inval,
         ": (use & or |  for multiple term",
         "s (and/or will then be applied)",
         "and - for exclusion)")
}

#' Generate UI for the gadget.
#'
#' @param datain dataframe or list
#' @param datalist string with names of data frames
gen_ui <- function(datain, datalist){

 miniPage(
   tags$head(
     tags$style(HTML("
    div.filtertitle {

            color: black;
            font-size: 200%
            font-weight: bold;
            background-color: Azure ;



             }
    div.filters {

            color: black;
            text-align: center;
            background-color: Azure ;



             }
                     "))),
  gadgetTitleBar("Subset and select columns for a data.frame"),
  miniTabstripPanel(
    miniTabPanel("View and select data",
                 icon = icon("table"),
                 miniContentPanel(
                   stableColumnLayout(
                     selectInput('data','Select data frame', datalist),
                     selectInput('labelorname',
                                 'Display name or label',
                                 c('Name', 'Label'))),
                   stableColumnLayout(
                     "Table displays current filters/ selections, only first N columns shown\n"),
                   stableColumnLayout(
                     sliderInput('colno', 'Select number of columns shown', 1, 500, 50)),
                     dataTableOutput("t1"))),
    miniTabPanel("Apply filters to the data",
                 icon = icon("sliders"),
                 miniContentPanel(
                   stableColumnLayout(
                     textInput("subset", "Manually enter filters"),
                     div((textOutput("message")), class = 'filters')),
                   stableColumnLayout(
                     selectizeInput(
                       inputId = "columns_filter",
                       label = "Choose a  column to filter on",
                       choices = NULL,
                       multiple = FALSE,
                       selected = NULL),
                     selectizeInput(inputId = "Selection_type",
                                    label = "Choose how to filter",
                                    choices = NULL,
                                    multiple = FALSE,
                                    selected = NULL),
                     uiOutput('Select_value')),
                   stableColumnLayout(
                     actionButton('insertBtn', 'Add filter'),
                     actionButton('removeBtn', 'Remove filter'),
                     tags$body(tags$b('Filters Added:'),
                               (tags$div( id = 'placeholder', class = 'filters')))),
                   dataTableOutput("t2"))),
    miniTabPanel("Select columns to view",
                 icon = icon("sliders"),
                 miniContentPanel(
                   selectInput('labelorname',
                               'Select based on name or label',
                               choices = c('Name', 'Label')),
                   stableColumnLayout(selectizeInput(
                     inputId = "columns",
                     label = "Choose columns",
                     choices = NULL,
                     multiple = TRUE,
                     selected = NULL,
                     width = "100%",
                     size = NULL)),
                   stableColumnLayout(
                     selectInput('andor',
                                 'Column selection should be applied with logic: ',
                                 choices = c('AND', 'OR'),
                                 selected = 'AND')),
                   stableColumnLayout(
                     textInput("starts", colselecter_text('starting with')),
                     textInput("ends", colselecter_text('ending with')),
                     textInput("contains", colselecter_text('containing'))),
                     dataTableOutput("t3")))))
}
