#' Subset data and save attributes
#' @export
#' @param x A data frame or matrix
#' @param subset A logical vector
#' @param select A character vector with column names
#' @return The subsetted data set
#' @examples
#' subset(airquality, Temp > 80, select = c(Ozone, Temp))
#' subset(airquality, Day == 1, select = -Temp)
#' subset(airquality, select = Ozone:Wind)
#'
#' with(airquality, subset(Ozone, Temp > 80))
#'
#' ## sometimes requiring a logical 'subset' argument is a nuisance
#' nm <- rownames(state.x77)
#' start_with_M <- nm %in% grep("^M", nm, value = TRUE)
#' subset(state.x77, start_with_M, Illiteracy:Murder)
#' # but in recent versions of R this can simply be
#' subset(state.x77, grepl("^M", nm), Illiteracy:Murder)
#'
subset_lab <- function(x, ...){

  if(is.data.frame(x))
  {
    attsave <- lapply(x, attributes)
    outdat <-subset(x, ...  )
  }else{
    x<-data.frame(x)
    attsave <- lapply(x, attributes)
    attsave <- lapply(x, attributes)
    outdat <- subset(data.frame(x), ...)
  }
  N_att <- names(attsave)[names(attsave) %in% names(outdat)]

  for (att in N_att){
    attributes(outdat[[att]]) <- attsave[[att]]
  }
 outdat
}
