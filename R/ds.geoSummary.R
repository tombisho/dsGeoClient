#' #' @title Wrapper for \code{gBuffer()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{gBuffer()} function from the
#' sp and rgeos packages
#' @details See the \code{gBuffer()} function from rgeos package for more details
#' @param input name of a spatial object on the server side to which the buffer will be applied
#' @param datasources name of a
#' @return an object of class SpatialPolygonsDataFrame or SpatialPolygons,
#' depending on the class of input
#' @author Bishop, T.
#' @export
#'
ds.geoSummary = function(input=NULL, datasources=NULL) {

  ##################################################################################
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(input)){
    stop("Please provide the name of an sp object as defined in package sp!", call.=FALSE)
  }

  calltext <- call("geoSummaryDS", input)
  ans = DSI::datashield.aggregate(datasources, calltext)

  return(ans)



}

