#' Subset data and save attributes.
#' Subset data and save attributes.
#' Note that for large data sets this will take a long time,
#' so by default this is only reapplied for the first 1000 columns
#' @export
#' @param x A data frame or matrix.
#' @param ... Other arguments passed to subset. In particular, we give subset
#'      and select arguments here (see examples and subset help for more)
#' @param col_lim A limit on the number of columns the attributes are applied to.
#' @return The subsetted data set
#' @examples
#' subset_lab(airquality, Temp > 80, select = c(Ozone, Temp))
#' subset_lab(airquality, Day == 1, select = -Temp)
#' subset_lab(airquality, select = Ozone:Wind)
#'
#' with(airquality, subset(Ozone, Temp > 80))
#'
#' ## sometimes requiring a logical 'subset' argument is a nuisance
#' nm <- rownames(state.x77)
#' start_with_M <- nm %in% grep("^M", nm, value = TRUE)
#' subset_lab(state.x77, start_with_M, Illiteracy:Murder)
#' # but in recent versions of R this can simply be
#' subset_lab(state.x77, grepl("^M", nm), Illiteracy:Murder)
#'
subset_lab <- function(x, ..., col_lim = 1000){

  if(is.data.frame(x))
  {
    attsave <- lapply(x, attributes)
    outdat <-subset(x, ...  )
  }else{
    x<-data.frame(x)
    attsave <- lapply(x, attributes)
    outdat <- subset(data.frame(x), ...)
  }
  N_att <- min(length(names(attsave)[names(attsave) %in% names(outdat)]), col_lim)
  if (N_att>0){
    names_replace <- names(outdat)[1:N_att]
  for (att in names_replace){
    attributes(outdat[[att]]) <- attsave[[att]]
  }
  }
  outdat
}

