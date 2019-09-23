#' Server code for the gadget.
#' @param datain data entered
#' @param list_true whether list_true is true or not
#' @param datalist String of data frames selected
#' @param location location app is running in
server_in <- function(datain, list_true, datalist, location){
server <- function(input, output, session) {

  #set up some values which will be updated over time
  inserted <- c()
  filtered <- reactiveValues(filtered = c())

  #add filters. This observes the button being updated, and updates the values
  #in filters and passes a value to UI
  observeEvent(input$insertBtn, {

    btn <- input$insertBtn
    id <- paste0('txt', btn)
    values <- input$selected_value
    N <- length (values)

    #if too many values, reduce down to 3
    if (N>=4){
      values[4] <- '...'
    }
    if(N>1){
      M <- min(4, N)
      values <- paste(values[1:M], collapse =',')
      values <- paste0('(', values, ')')
    }
    #add ui element
    insertUI(selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
             ui = tags$div(tags$p(paste0(input$columns_filter,
                                         ' ',
                                         input$Selection_type,
                                         ' ',
                                         values)),
             id = id
      )
    )
    #store values in filtered
    inserted <<- c(id, inserted)
    if (input$Selection_type %in% c('%in%', '%not in%'))
    {
      selectedvalues <- paste(input$selected_value, collapse =',')
    filtered$filtered <- c(paste0(input$columns_filter,
                                  ' ',
                                  input$Selection_type,
                                  ' c(',
                                  selectedvalues,')'),
                           filtered$filtered)
    }else{
      filtered$filtered <- c(paste0(input$columns_filter,
                                    ' ',
                                    input$Selection_type,
                                    ' ',
                                    input$selected_value),
                             filtered$filtered)
    }
  })
  #remove filters

  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[1])
    )
    inserted <<- inserted[-1]
    filtered$filtered <- filtered$filtered[-1]
  })

  #this generates the code from user inputs
  codestatement <- reactive({
    #begin by determining data source
    if(!is.null(datain)){
      if(list_true){
        data <- data.frame(get(strsplit(input$data,
                                        split="$",
                                        fixed=T)[[1]][2],
                               datain))
      }else{
        data <- datain
      }
    }else{
      data <- data.frame(get(input$data, envir = .GlobalEnv) )
    }

    datnames <- names_label(data)
    code <- paste0("viewenhance::subset_lab(",input$data)

    #if there is a subset argument, we add it here

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
        output$message <- renderText(paste0('Filtering expression is causing an error!',
                                     'Currently this is ',
                                     subset_con))
      }else{
        output$message <- renderText('Filtering expression accepted')
        code <- paste0(code, ",subset = ",  subset_con)}
    }

    #look to see if the user has set up restrictions for the column names

    if(nzchar(input$starts)| nzchar(input$ends) |
       nzchar(input$contains) | !is.null(input$columns)){

      #condition will be built up from the different user inputs
      #with & or | depending on user input

      jointerm      <- ifelse( input$andor =='AND', '&', '|')
      starts_cond   <- ''
      ends_cond     <- ''
      contains_cond <- ''
      all_cond <- ''


      if(nzchar(input$starts)){
        starts_cond <- get_columnselect_text('startsWith', input$starts,
                              input$labelorname, input$data)
        all_cond <- starts_cond
      }

      if(nzchar(input$ends)){
        ends_cond <- get_columnselect_text('endsWith', input$ends,
                             input$labelorname, input$data)
        all_cond <- ifelse(all_cond == '',
                           ends_cond,
                           paste0(all_cond, jointerm, ends_cond))

      }


      if(nzchar(input$contains)){
        contains_cond <- get_columnselect_text('grepl', input$contains,
                                               input$labelorname, input$data,
                                               TRUE)
        all_cond <- ifelse(all_cond == '',
                           contains_cond,
                           paste0(all_cond, jointerm, contains_cond))
      }

      condsave <- ifelse(all_cond=='', T, all_cond)
      if(!is.null(input$columns)){
        col_term <- paste0("names_label(",
                        input$data,
                        ",'",
                        input$labelorname,
                        "') %in% c('",
                        paste(input$columns,
                              collapse = "','"),
                        "')"
                        )
        all_cond <- ifelse(all_cond == '',
                           col_term,
                       paste0(all_cond, jointerm, col_term))
      }

      #finally append the condition to the code statement
      codesave <- paste0(code, ", ", "select = ", condsave,")")
      code <- paste0(code, ", ", "select = ", all_cond ,")")
      return(list(code=code, codesave=codesave))
    }else{
      code <- paste0(code,')')
      return(list(code=code,codesave=code))
    }
  })
  #get data based on current conditions
  current_data <- reactive({
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
  #data view in shiny environment
  output$t1 <- renderDataTable(current_data())
  output$t2 <- renderDataTable(current_data())
  output$t3 <- renderDataTable(current_data())



  #get the available columns from the chosen data source

  observe({
    dataString <- input$data
    if(!is.null(datain)){
      if(list_true){
        data <- data.frame(get(strsplit(input$data, split="$", fixed=T)[[1]][2]
                               , datain) )
      }else{
        data <- datain
      }
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

  #update columns to filter on based on current data selection

  observe({
    dataString <- input$data
    if(!is.null(datain)){
      if(list_true){
        data <- data.frame(get(strsplit(input$data, split="$", fixed=T)[[1]][2]
                               , datain) )
      }else{
        data <- datain
      }
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

  #get seleciton value based on whether data is character or numeric

  observe({
    dataString <- input$data
    if(!is.null(datain)){
      if(list_true){
        data <- data.frame(get(strsplit(input$data, split="$", fixed=T)[[1]][2]
                               , datain) )
      }else{
        data <- datain
      }
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

  #get list of unique values from selected column

  output$Select_value <- renderUI({
    dataString <- input$data
    if(!is.null(datain)){
      if(list_true){
        data <- data.frame(get(strsplit(input$data, split="$", fixed=T)[[1]][2]
                               , datain) )
      }else{
        data <- datain
      }
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



  # Listen for 'done'. If so, output the code wrapped in a View() statement into the console
  observeEvent(input$done, {
    rstudioapi::sendToConsole(paste0('viewenhance::View(',codestatement()$code,', replace = FALSE)'))
    invisible(stopApp())
  })
  
  observeEvent(input$restart, {
    
    
    if(location == "dialog"){
      rstudioapi::sendToConsole(
        paste0("viewenhance::viewenhanceAddin(datain = ",
               datalist,
               ", location = 'pane')")
      )
    }else{
      rstudioapi::sendToConsole(
        paste0("viewenhance::viewenhanceAddin(datain = ",
               datalist,
               ", location = 'dialog')")
      )
    }

    invisible(stopApp())

  })


}
server
}

