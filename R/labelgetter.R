#' name look
#'
#' This is a function which either gives the names of an object
#' or the labels, depending on the inputs given
#'
#' @export
#' @param datain inputted data set
#' @param label_name whether you want the names or labels
#' @return a vector of labels or names depending on the value of label_name

names_label <- function(datain, label_name = 'Name'){
  if (!is.data.frame(datain)){
    stop('datain should be a data frame')
  }
  if (label_name == 'Name'){
    names(datain)
  }else if (label_name == 'Label'){
    attsout <- Map(attr_getter, datain)
    attsout <- Map(nullreplacer, attsout, names(datain))
    as.character(unlist(attsout))
  }else{
      stop('label_name should be Label or Name')
    }
}
