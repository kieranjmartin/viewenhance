subsetAddin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  defaultData <- trim(text)

  # Generate UI for the gadget.
  ui <- miniPage(tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
  ),
    gadgetTitleBar("Subset a data.frame"),
    miniContentPanel(
      stableColumnLayout(
        textInput("data", "Data", value = defaultData),
        textInput("subset", "Subset Expression")),
      stableColumnLayout(
        textInput("starts", "Select columns starting with:"),
        textInput("ends", "Select columns ending with:"),
        textInput("contains", "Select columns containing:")
      ),
      uiOutput("pending"),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$data
      subsetString <- input$subset

      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)

      if (!(nzchar(subsetString) + nzchar(input$starts) +nzchar(input$ends)+
            nzchar(input$contains)))
        return(data)

      if ((nzchar(subsetString))) {
      # Try evaluating the subset expression within the data.
      condition <- try(parse(text = subsetString)[[1]], silent = TRUE)
      if (inherits(condition, "try-error"))
        return(errorMessage("expression", paste("Failed to parse expression '", subsetString, "'.")))
      }
        datnames <- names(get(input$data, envir = .GlobalEnv))
        if(nzchar(input$starts)){
          datnames <- datnames[str_detect(datnames,paste0('^',input$starts))]
        }
        if(nzchar(input$ends)){
          datnames <- datnames[str_detect(datnames,paste0(input$ends,'$'))]
        }
        if(nzchar(input$contains)){
          datnames <- datnames[str_detect(datnames,input$contains)]
        }
        if ((nzchar(subsetString))) {
      call <- as.call(list(
        as.name("subset.data.frame"),
        data,
        condition, datnames
      ))}else{
        call <- as.call(list(
          as.name("subset.data.frame"),
          data,
          select = datnames
        ))
      }

      eval(call, envir = .GlobalEnv)
      }

    )

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    output$output <- renderDataTable({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      data
    })

    # Listen for 'done'.
    observeEvent(input$done, {

      if(!nzchar(input$data))
      {

      }else
      {
        datnames <- names(get(input$data, envir = .GlobalEnv))

        code <- paste0("View(subset(",input$data)
      if (nzchar(input$subset)) {
        code <- paste0(code, ", ", subset = input$subset)
      }
        if(nzchar(input$starts)){
          datnames <- datnames[str_detect(datnames,paste0('^',input$starts))]
        }
        if(nzchar(input$ends)){
          datnames <- datnames[str_detect(datnames,paste0(input$ends,'$'))]
        }
        if(nzchar(input$contains)){
          datnames <- datnames[str_detect(datnames,input$contains)]
        }
        code <- paste0(code, ", ",
                       "select = c(",
                       paste(datnames, collapse=','),')))')
        rstudioapi::sendToConsole(code)
      }

      invisible(stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))

}
