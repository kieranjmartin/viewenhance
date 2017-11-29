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
    if (replace == T){
        if (is.data.frame(datain)){
            all_names <- ls(envir = .GlobalEnv)
            dataname <- deparse(substitute(datain))
            if((dataname %in% all_names)[1]){
                viewenhanceAddin(dataname)
            }else{
                dataname <- deparse(substitute(datain))
                as.environment("package:utils")$View(datain, dataname)
            }
        }else{
            dataname <- deparse(substitute(datain))
            as.environment("package:utils")$View(datain, dataname)
        }
    }else{
        dataname <- deparse(substitute(datain))
        as.environment("package:utils")$View(datain, dataname)
    }
}


