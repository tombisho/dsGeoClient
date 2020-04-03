##' @title DOes nothing
##' @description This function is similar to something
##'
##' @param x name of the thing
##' @param datasources ....
##'
##' @return a character vector of measured variables.
##'
##' @export
##' @examples
##'

ds.nothing <- function(x, datasources=NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }


  cally <- paste0("nothing(", x, ")")
  ans <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(ans)

}
