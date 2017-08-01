exploredataAddin<- function() {
  datalist <- tryCatch(ls(envir = .GlobalEnv)[!unlist(lapply(lapply(mget( ls(envir = .GlobalEnv), envir = .GlobalEnv) , dim),is.null))],
                       error = function(e) stop("The global environment is empty!"))
  if (length(datalist) == 0)
  {
    stop("The global environment does not include any objects with dimensions!")
  }

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Subset a data.frame"),
    miniContentPanel(
      stableColumnLayout(
        selectInput('data','Select data frame', datalist),
        textInput("subset", "Subset Expression"),
        textOutput('message')),
      stableColumnLayout(uiOutput("colselect")),
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
    subflag = 0
    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$data
      subsetString <- input$subset



      data <- as.data.frame(get(dataString, envir = .GlobalEnv))

      if (!(nzchar(subsetString) + nzchar(input$starts) +nzchar(input$ends)+
            nzchar(input$contains))&is.null(input$columns))
        return(data)

      if ((nzchar(subsetString))) {
      # Try evaluating the subset expression within the data.
      condition <- try(parse(text = subsetString), silent = TRUE)
      tryme <- try({
        call <- as.call(list(as.name("subset.data.frame"),data,condition))
        eval(call, envir = .GlobalEnv)
        }, silent = TRUE)
      if (inherits(tryme, "try-error")){
        output$message <- renderText('Error in subset expression')
      }else{subflag = 1
      output$message <- renderText('Subset expression accepted')}
      }
        datnames <- names(as.data.frame(get(input$data, envir = .GlobalEnv)))
        if(nzchar(input$starts)){
          datnames <- datnames[str_detect(datnames,paste0('^',input$starts))]
        }
        if(nzchar(input$ends)){
          datnames <- datnames[str_detect(datnames,paste0(input$ends,'$'))]
        }
        if(nzchar(input$contains)){
          datnames <- datnames[str_detect(datnames,input$contains)]
        }
        if(!is.null(input$columns)){
          datnames <- datnames[datnames %in% input$columns]
        }
        if ((nzchar(subsetString)) & subflag == 1) {
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


        data <- as.data.frame(get(input$data, envir = .GlobalEnv) )
        datnames <- names(data)


        code <- paste0("View(subset(as.data.frame(",input$data,")")
        if (nzchar(input$subset))
            {
        condition <- try(parse(text = input$subset), silent = TRUE)
        tryme <- try({
          call <- as.call(list(as.name("subset.data.frame"),data,condition))
          eval(call, envir = .GlobalEnv)
        }, silent = TRUE)
        if (!inherits(tryme, "try-error")){
        code <- paste0(code, ", ", subset = input$subset)
        }
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
        if(!is.null(input$columns)){
          datnames <- datnames[datnames %in% input$columns]
        }
        code <- paste0(code, ", ",
                       "select = c(",
                       paste(datnames, collapse=','),')))')


        rstudioapi::sendToConsole(code)


      invisible(stopApp())
    })

    output$colselect <- renderUI({
      dataString <- input$data

      data <- as.data.frame(get(dataString, envir = .GlobalEnv))
      namelist <- names(data)
      selectInput("columns", "Choose columns", namelist, selected = NULL, multiple = TRUE,
                  selectize = TRUE, width = "100%", size = NULL)
    })

  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  #suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))
  runGadget(ui, server, viewer = viewer)

}
