#' Shiny view of data
#' @export
#' @import shiny
#' @import miniUI
#' @param datain Provide a data frame or a list of data frames to explore
#' @return A shiny box, which, when options are chosen, will put a View command into the console

viewenhanceAddin<- function(datain = NULL) {


    if (!is.null(datain)){
        if(is.character(datain)){
            datalist <- datain
            datain   <- tryCatch({get(datain, envir = .GlobalEnv)},
                                 error = function(e){stop(paste0("Provided data frame name ",
                                                                 datain,
                                                                 " does not exist in global enviornment"))})
            list_true <- FALSE
        }
        else if (is.data.frame(datain)){

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

    ui <- gen_ui(datain, datalist)
    server <- server_in(datain, list_true)
    environment(server) <- environment()


    # Use a modal dialog as a viewr.
    viewer <- dialogViewer("Subset", width = 1000, height = 800)
    #note we suppress messages. We remove this when debugging :)
    #suppressMessages(suppressWarnings(runGadget(ui, server, viewer = viewer)))
    runGadget(ui, server, viewer = viewer)

}
