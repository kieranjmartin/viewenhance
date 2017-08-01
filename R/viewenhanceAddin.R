viewenhanceAddin<- function() {
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
      stableColumnLayout(selectInput('andor', 'Column selection should be applied with logic: ', c('AND', 'OR'),
                                     'AND')),
      stableColumnLayout(
        textInput("starts", "Select columns starting with:"),
        textInput("ends", "Select columns ending with:"),
        textInput("contains", "Select columns containing:")
      ),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    codestatement <- reactive({
      data <- as.data.frame(get(input$data, envir = .GlobalEnv) )
      datnames <- names(data)

      code <- paste0("subset(as.data.frame(",input$data,")")
      if (nzchar(input$subset))
      {
        condition <- try(parse(text = input$subset), silent = TRUE)
        tryme <- try({
          call <- as.call(list(as.name("subset.data.frame"),data,condition))
          eval(call, envir = .GlobalEnv)
        }, silent = TRUE)
        if (inherits(tryme, "try-error")){
          output$message <- renderText('Error in subset expression')
        }else{
          output$message <- renderText('Subset expression accepted')
          code <- paste0(code, ", ", subset = input$subset)}
      }

      if(nzchar(input$starts)| nzchar(input$ends) | nzchar(input$contains) | !is.null(input$columns))
      {
        cond <- ''
        jointerm <- ifelse( input$andor =='AND', '&', '|')
        if(nzchar(input$starts)){
          cond <- paste0("startsWith(names(",input$data,"),'",input$starts,"')")
        }
        if(nzchar(input$ends)){
          eterm <- paste0("startsWith(names(",input$data,"),'",input$starts,"')")
          cond <- ifelse(cond=='',eterm,
                         paste0(cond,jointerm,eterm))
        }
        if(nzchar(input$contains)){
          eterm <- paste0("grepl('",input$contains,"',names(",input$data,"))")
          cond <- ifelse(cond=='',eterm,
                         paste0(cond,jointerm,eterm))
        }
        if(!is.null(input$columns)){
          eterm <- paste0("names(",input$data,")%in%c('",paste(input$columns, collapse = "','"),"')")
          cond <- ifelse(cond=='',eterm,
                         paste0(cond,jointerm,eterm))
        }
        code <- paste0(code, ", ","select = names(",input$data,")[",cond,"])")
      }else{
        code <- paste0(code,')')
      }
      code
    })



    output$output <- renderDataTable({
      data <- eval(parse(text=codestatement()), envir = .GlobalEnv)
      if (isErrorMessage(data))
        return(NULL)
      data
    })

    # Listen for 'done'.
    observeEvent(input$done, {
      rstudioapi::sendToConsole(paste0('View(',codestatement(),')'))
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
