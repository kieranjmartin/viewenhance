#'View
#'
#'Replaces View as given by R Studio.
#'This automatically opens the viewer app.
#'Disable this by setting replace = F
#'
#'@export
#'@importFrom utils View
#'@param datain a data frame or list of data frames
#'@param replace Set equal to false to use normal view
#'@return Either the View, or the app View from viewenhanceAddin

View <- function(datain, replace = T){
    all_names <- ls(envir = .GlobalEnv)
    dataname <- deparse(substitute(datain))
    if (replace == T & is.data.frame(datain) & (dataname %in% all_names)[1]){
       viewenhanceAddin(dataname)
    }else{
       if((dataname %in% all_names)[1]){
        as.environment("package:utils")$View(datain, dataname)
        }else{
            as.environment("package:utils")$View(environment()$datain)
        }
    }
}
