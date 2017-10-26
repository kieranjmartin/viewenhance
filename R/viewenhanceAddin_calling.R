#' datacheck
#'
#' Check whether an input has been given. If it hasn't, look in global environment
#' To see if there are any data frames.
#' @param datain a data frame or list
#' @return A character list of data frames

datacheck <- function(datain)
{
  if (!is.null(datain)){
    if (is.data.frame(datain)){
      datalist <- deparse(substitute(datain))
      list_true <- FALSE
    }else if (is.list(datain)){
      datframes <- datain[unlist(Map(is.data.frame, datain))]
      if (length(datframes) == 0){
        stop("None of the objects in the inputted list are data frames")
      }
      list_true <- TRUE
      datalist <- paste0(deparse(substitute(datain)),'$', names(datframes))
    }else{
      stop("datain needs to be a data frame or a list containing data frames")
    }
  }else{
      list_true = TRUE
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
  return(list(datalist = datalist, list_true = list_true))
}

#' Column text selector
#'
#' Handles the text string given by user for starts with/ ends with/ contains
#'
#'@param expression what function is being checked
#'@param intext the text to search over
#'@param labelorname the input value, label or name
#'@param dataname name of data frame
#'@param contains if expression is contains, this is TRUE
get_columnselect_text <- function(expression, intext,
                                  labelorname, dataname,
                                  contains = FALSE){

  #split the text into the individual units, split by & or |
  startvect <- strsplit(intext, "(&|\\|)")[[1]]

  # get all the & and |s locations
  special <- gregexpr("(&|\\|)", intext)[[1]]
  #if any exist, create schar with the special characters added
  if (special[1] == -1){
  schar <- NULL
  }else{
  schar<- as.character(unlist(Map(substr, intext, special, special)))
  }

  get_terms <- function(expression, term, labelorname, dataname,
                        contains){
    if (substr(term,1,1) =='-')
    {
      add <- '!'
      term <- substr(term,2,nchar(term))
    }else{
      add <- ''
    }
    if (contains)
    {
      paste0(add, expression, "('", term, "', names_label(", dataname,
             ",'", labelorname, "'))"
      )

    }else{
      paste0(add, expression, "(names_label(", dataname,
             ",'", labelorname, "'),'", term, "')")
    }
  }
  full_terms <- as.character(
    unlist(Map(get_terms, expression, startvect, labelorname, dataname, contains))
  )
  if( !is.null(schar)){
    schar<- c(schar, '')
    full_terms <- as.vector(rbind(full_terms, schar))
    full_terms <- paste(full_terms, collapse = ' ')
  }
 paste0('(',full_terms,')')
}

