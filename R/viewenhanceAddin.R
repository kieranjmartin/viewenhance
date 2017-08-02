#This is the only function in this package. It is designed to be called via the add in, and consequently takes no
#arguments!
#It builds a small shiny app for interactively viewing your data, and stores your filter/subset arguments
#into a string which is then pushed to console when finished

viewenhanceAddin<- function() {

  #Check which objects in the name space have a dimension. If the environment is empty, return said error
  datalist <- tryCatch({
    ls(envir = .GlobalEnv)[!unlist(lapply(lapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv) , dim),is.null))]},
                       error = function(e) stop("The global environment is empty!"))
  #if there are no dimensional objects, error
  if (length(datalist) == 0)
  {
    stop("The global environment does not include any objects with dimensions!")
  }

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Subset and select columns for a data.frame"),
    miniContentPanel(
      stableColumnLayout(
        selectInput('data','Select data frame', datalist),
        textInput("subset", "Subset Expression"),
        textOutput('message')),
      stableColumnLayout(uiOutput("colselect")),
      stableColumnLayout(selectInput('andor', 'Column selection should be applied with logic: ', c('AND', 'OR'),
                                     'AND')),
      stableColumnLayout(
        textInput("starts", "Select columns starting with: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)"),
        textInput("ends", "Select columns ending with: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)"),
        textInput("contains", "Select columns containing: (use & or |  for multiple terms (and/or will then be applied) and - for exclusion)")
      ),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    #this generates the code from user inputs
    codestatement <- reactive({
      data <- as.data.frame(get(input$data, envir = .GlobalEnv) )
      datnames <- names(data)

      code <- paste0("subset(as.data.frame(",input$data,")")
      if (nzchar(input$subset))
      {
        #this checks if the subset argument works, and updates the message if it does not
        condition <- try(parse(text = input$subset), silent = TRUE)
        tryme <- try({
          call <- as.call(list(as.name("subset.data.frame"),data,condition))
          eval(call, envir = .GlobalEnv)
        },
        silent = TRUE)


        if (inherits(tryme, "try-error")){
          output$message <- renderText('Error in subset expression')
        }else{
          output$message <- renderText('Subset expression accepted')
          code <- paste0(code, ", ", subset = input$subset)}
      }

      #look to see if the user has set up restrictions for the column names

      if(nzchar(input$starts)| nzchar(input$ends) | nzchar(input$contains) | !is.null(input$columns))
      {

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
               paste0("!startsWith(names(",input$data,"),'",
                      substr(term,2,nchar(term)),"')"),
               paste0("startsWith(names(",input$data,"),'",term,"')"))
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
                          paste0("!endsWith(names(",input$data,"),'",
                                 substr(term,2,nchar(term)),"')"),
                          paste0("endsWith(names(",input$data,"),'",term,"')"))
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
                          paste0("grepl('",substr(term,2,nchar(term)),"',names(",input$data,"))"),
                          paste0("grepl('",term,"',names(",input$data,"))"))
            ccond <- ifelse(ccond=='',eterm,
                           paste0(ccond,schar[j-1],eterm))
          }
          ccond <- ifelse(ccond=='',ccond,paste0('(',ccond,')'))
          cond <-ifelse(cond == '', ccond, paste0(cond, jointerm, ccond))
        }



        if(!is.null(input$columns)){
          eterm <- paste0("names(",input$data,")%in%c('",paste(input$columns, collapse = "','"),"')")

          cond <- ifelse(cond=='',eterm,
                         paste0(cond,jointerm,eterm))
        }

        #finally append the condition to the code statement

        code <- paste0(code, ", ","select = names(",input$data,")[",cond,"])")
      }else{
        code <- paste0(code,')')
      }
      code
    })


    #data view in shiny environment

    output$output <- renderDataTable({
      data <- eval(parse(text=codestatement()), envir = .GlobalEnv)
      if (isErrorMessage(data))
        return(NULL)
      data
    })

    # Listen for 'done'. If so, output the code wrapped in a View() statement into the console
    observeEvent(input$done, {
      rstudioapi::sendToConsole(paste0('View(',codestatement(),')'))
      invisible(stopApp())
    })


    #get the available columns from the chosen data source

    output$colselect <- renderUI({
      dataString <- input$data


      if (input$andor == 'AND'){
        namelist <- names(eval(parse(text=codestatement()), envir = .GlobalEnv))
      }else{
        data <- as.data.frame(get(dataString, envir = .GlobalEnv))
        namelist <- names(data)
      }

      selectInput("columns", "Choose columns", sort(namelist), selected = NULL, multiple = TRUE,
                  selectize = TRUE, width = "100%", size = NULL)
    })

  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  #note we suppress messages. We remove this when debugging :)
  suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))
  #runGadget(ui, server, viewer = viewer)

}
