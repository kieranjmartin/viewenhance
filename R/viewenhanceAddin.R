#' Shiny view of data
#' @export
#' @import shiny
#' @import miniUI
#' @param datain Provide a data frame to explore
#' @return A shiny box, which, when options are chosen, will put a View command into the console

viewenhanceAddin<- function(datain = NULL) {


  if (!is.null(datain)){
    if (is.data.frame(datain)){
      datalist <- deparse(substitute(datain))
    }else{
      stop("datain needs to be a data frame")
    }
  }else{
    #Check which objects in the name space have a dimension. If the environment is empty, return said error
    datalist <- tryCatch({
      ls(envir = .GlobalEnv)[unlist(lapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv) , is.data.frame))]},
      error = function(e) stop("There are no data frames in the global environment!"))
    #if there are no dimensional objects, error
    if (length(datalist) == 0)
    {
      stop("The global environment does not include any objects with dimensions!")
    }
  }

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Subset and select columns for a data.frame"),
    miniContentPanel(
      stableColumnLayout(
        selectInput('data','Select data frame', datalist),
        textInput("subset", "Subset Expression"),
        textOutput('message'),
        selectInput('labelorname', 'Select name or label', c('Name', 'Label'))),
      stableColumnLayout(
        selectizeInput(inputId = "columns_filter", label = "Choose a  column to filter on (note, only names can be used for this)",
                       choices = NULL, multiple = FALSE,
                       selected = NULL),
        selectizeInput(inputId = "Selection_type", label = "Choose how to filter",
                       choices = NULL, multiple = FALSE,
                       selected = NULL),
        uiOutput('Select_value')),
      stableColumnLayout(actionButton('insertBtn', 'Add filter'),
                         actionButton('removeBtn', 'Remove filter'),
                         tags$div('Filters are:' ,id = 'placeholder')),
      stableColumnLayout(selectizeInput(inputId = "columns", label = "Choose columns",
                                        choices = NULL, multiple = TRUE,
                                        selected = NULL,
                                        width = "100%", size = NULL)),
      stableColumnLayout(selectInput('andor', 'Column selection should be applied with logic: ', c('AND', 'OR'),
                                     'AND')),
      stableColumnLayout(
        textInput("starts", "Select columns starting with: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)"),
        textInput("ends", "Select columns ending with: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)"),
        textInput("contains", "Select columns containing: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)")
      ),
      stableColumnLayout("Table displays current filters/ selections, only first N columns shown\n"),
      stableColumnLayout(sliderInput('colno', 'Select number of columns shown', 1, 500, 50)),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {
    inserted <- c()
    filtered <- reactiveValues(filtered = c())
    observeEvent(input$insertBtn, {
      btn <- input$insertBtn
      id <- paste0('txt', btn)
      values <- input$selected_value
      N <- length (values)
      if (N>=4){
        values[4] <- '...'
      }
      if(N>1){
        M <- min(4, N)
        values <- paste(values[1:M], collapse =',')
        values <- paste0('(', values, ')')
      }
      insertUI(
        selector = '#placeholder',
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          tags$p(paste0(input$columns_filter,' ', input$Selection_type,' ', values)),
          id = id
        )
      )
      inserted <<- c(id, inserted)
      filtered$filtered <- c(paste0(input$columns_filter,' ', input$Selection_type,' ',
                                    input$selected_value),
                             filtered$filtered)
    })
    #this generates the code from user inputs
    codestatement <- reactive({
      if(!is.null(datain)){
        data <- datain
      }else{
        data <- data.frame(get(input$data, envir = .GlobalEnv) )
      }
      datnames <- names_label(data)
      code <- paste0("viewenhance::subset_lab(",input$data)

      if (nzchar(input$subset)| length(filtered$filtered>0))
      {
        if  (nzchar(input$subset)){
          subset_con <- paste(c(input$subset, filtered$filtered), collapse = '&')
        }else{
          subset_con <- paste(c(filtered$filtered), collapse = '&')
        }

        #this checks if the subset argument works, and updates the message if it does not
        condition <- try(parse(text = subset_con), silent = TRUE)
        tryme <- try({
          call <- as.call(list(as.name("subset.data.frame"), data, condition))
          eval(call, envir = .GlobalEnv)
        },
        silent = TRUE)


        if (inherits(tryme, "try-error")){
          output$message <- renderText('Error in subset expression')
        }else{
          output$message <- renderText('Subset expression accepted')
          code <- paste0(code, ",subset = ",  subset_con)}
      }

      #look to see if the user has set up restrictions for the column names

      if(nzchar(input$starts)| nzchar(input$ends) |
         nzchar(input$contains) | !is.null(input$columns)){

        #condition will be built up from the different user inputs
        #with & or | depending on user input

        cond <- ''
        jointerm <- ifelse( input$andor =='AND', '&', '|')
        scond <- ''
        econd <- ''
        ccond <-''


        if(nzchar(input$starts)){
          startvect <- strsplit(input$starts, "(&|\\|)")[[1]]
          special <- gregexpr("(&|\\|)", input$starts)[[1]]
          if (special[1] == -1){}else{
            schar <-character(length=length(special))
            for (i in 1:length(special))
            {
              schar[i] <- substr(input$starts,special[i], special[i])
            }
          }
          j<-0
          for (term in startvect){
            j<-j+1
            eterm<-ifelse(substr(term,1,1) =='-',
                          paste0("!startsWith(names_label(",input$data,",'",
                                 input$labelorname,"'),'",
                                 substr(term,2,nchar(term)),"')"),
                          paste0("startsWith(names_label(",input$data,",'",
                                 input$labelorname,"'),'",term,"')"))
            scond <- ifelse(scond=='',eterm,
                            paste0(scond,schar[j-1],eterm))
          }
          scond <- ifelse(scond=='',scond,paste0('(',scond,')'))
        }
        cond <- scond

        if(nzchar(input$ends)){
          endvect <- strsplit(input$ends, "(&|\\|)")[[1]]
          special <- gregexpr("(&|\\|)", input$ends)[[1]]
          if (special[1] == -1){}else{
            schar <-character(length=length(special))
            for (i in 1:length(special))
            {
              schar[i] <- substr(input$ends,special[i], special[i])
            }
          }
          j <- 0
          for (term in endvect){
            j<- j + 1
            eterm<-ifelse(substr(term,1,1) =='-',
                          paste0("!endsWith(names_label(",input$data,",'",
                                 input$labelorname,"'),'",
                                 substr(term,2,nchar(term)),"')"),
                          paste0("endsWith(names_label(",input$data,",'",
                                 input$labelorname,"'),'",term,"')"))
            econd <- ifelse(econd=='',eterm,
                            paste0(econd,schar[j-1],eterm))
          }
          econd <- ifelse(econd=='',econd,paste0('(',econd,')'))
          cond <-ifelse(cond == '', econd, paste0(cond, jointerm, econd))
        }


        if(nzchar(input$contains)){
          convect <- strsplit(input$contains, "(&|\\|)")[[1]]
          special <- gregexpr("(&|\\|)", input$contains)[[1]]

          if (special[1] == -1){}else{
            schar <-character(length=length(special))
            for (i in 1:length(special))
            {
              schar[i] <- substr(input$contains,special[i], special[i])
            }
          }
          j <- 0
          for (term in convect){
            j<- j + 1
            eterm<-ifelse(substr(term,1,1) =='-',
                          paste0("!grepl('",substr(term,2,nchar(term)),
                                 "',names_label(",input$data,",'",
                                 input$labelorname, "'))"),
                          paste0("grepl('",term,"',names_label(",input$data,",'",
                                 input$labelorname,"'))"))
            ccond <- ifelse(ccond=='',eterm,
                            paste0(ccond,schar[j-1],eterm))
          }
          ccond <- ifelse(ccond=='',ccond,paste0('(',ccond,')'))
          cond <-ifelse(cond == '', ccond, paste0(cond, jointerm, ccond))
        }


        condsave <- ifelse(cond=='',T,cond)
        if(!is.null(input$columns)){
          eterm <- paste0("names_label(",input$data,",'",
                          input$labelorname,"')%in%c('",paste(input$columns, collapse = "','"),"')")
          cond <- ifelse(cond=='',eterm,
                         paste0(cond,jointerm,eterm))
        }

        #finally append the condition to the code statement
        codesave <- paste0(code, ", ","select = ",condsave,")")
        code <- paste0(code, ", ","select = ",cond,")")
        return(list(code=code,codesave=codesave))
      }else{
        code <- paste0(code,')')
        return(list(code=code,codesave=code))
      }
    })


    #data view in shiny environment
    output$output <- renderDataTable({
      data <- eval(parse(text=codestatement()$code), envir = .GlobalEnv)
      if (isErrorMessage(data))
        return(data.frame(x = c('Failure. Code string is ',
                                codestatement()$code)))
      N <- min(input$colno, dim(data)[2])
      if (N>0){
        datout <- data[,seq(1,N), drop = FALSE]
        names(datout) <- names_label(datout, input$labelorname)
        datout
      }else
      {
        data
      }

    })

    # Listen for 'done'. If so, output the code wrapped in a View() statement into the console
    observeEvent(input$done, {
      rstudioapi::sendToConsole(paste0('View(',codestatement()$code,')'))
      invisible(stopApp())
    })


    #get the available columns from the chosen data source

    observe({
      dataString <- input$data
      if(!is.null(datain)){
        data <- datain
      }else{
        data <- data.frame(get(dataString, envir = .GlobalEnv))
      }
      namelist <- names_label(data,input$labelorname)
      outlist <- sort(namelist)
      updateSelectizeInput(session = session,
                           inputId = 'columns',
                           choices = outlist ,
                           server = TRUE)


    })
    observe({
      dataString <- input$data
      if(!is.null(datain)){
        data <- datain
      }else{
        data <- data.frame(get(dataString, envir = .GlobalEnv))
      }
      namelist <- names(data)
      outlist <- sort(namelist)
      updateSelectizeInput(session = session,
                           inputId = 'columns_filter',
                           choices = outlist ,
                           server = TRUE)
    })
    observe({
      dataString <- input$data
      if(!is.null(datain)){
        data <- datain
      }else{
        data <- data.frame(get(dataString, envir = .GlobalEnv))
      }
      if (is.character(data[[input$columns_filter]])|is.factor(data[[input$columns_filter]])){
        selectlist <- c('==', '!=', '%in%', '%not in%')
      }else{
        selectlist <- c('<', '<=', '==','!=', '>=', '>', '%in%', '%not in%')
      }

      updateSelectizeInput(session = session,
                           inputId = 'Selection_type',
                           choices = selectlist ,
                           selected = '=',
                           server = TRUE)
    })

    output$Select_value <- renderUI({
      dataString <- input$data
      if(!is.null(datain)){
        data <- datain
      }else{
        data <- data.frame(get(dataString, envir = .GlobalEnv))
      }
      values <- unique(data[[input$columns_filter]])
      if (is.character(values) | is.factor(values)){
        values <- paste0("'", values, "'")
      }
      if (input$Selection_type %in% c('%in%', '%not in%')){
        selectInput('selected_value','Select values to filter on', values,
                    multiple = TRUE)
      }else{
        selectInput('selected_value','Select values to filter on', values,
                    multiple = FALSE)
      }
    })





    observeEvent(input$removeBtn, {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[1])
      )
      inserted <<- inserted[-1]
      filtered$filtered <- filtered$filtered[-1]
    })





  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  #note we suppress messages. We remove this when debugging :)
  suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))
  #runGadget(ui, server, viewer = viewer)

}
