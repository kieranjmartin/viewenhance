
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

