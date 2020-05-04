#' #' @title Wrapper for \code{gBuffer()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{gBuffer()} function from the
#' sp and rgeos packages
#' @details See the \code{gBuffer()} function from rgeos package for more details
#' @param input name of a spatial object on the server side to which the buffer will be applied
#' @param by_id Logical determining if the function should be applied across subgeometries
#' (TRUE) or the entire object (FALSE)
#' @param ip_width Distance from original geometry to include in the new geometry.
#' Negative values are allowed. Either a numeric vector of length 1 when byid is
#' FALSE; if byid is TRUE: of length 1 replicated to the number of input geometries,
#'  or of length equal to the number of input geometries
#' @param newobj.name a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame
#' followed by the suffix '.buff'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return an object of class SpatialPolygonsDataFrame or SpatialPolygons,
#' depending on the class of input
#' @author Bishop, T.
#' @export
#'
ds.gBuffer = function(input=NULL, by_id=FALSE, ip_width=NULL, newobj.name=NULL, datasources=NULL) {

  ##################################################################################
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(input)){
    stop("Please provide the name of an sp object as defined in package sp!", call.=FALSE)
  }

  # check if a valid width has been provided
  if(is.null(ip_width)){
    stop("Please provide a valid width for the buffer!", call.=FALSE)
  }

  if(!is.numeric(ip_width)){
    stop("Buffer width is not numeric!", call.=FALSE)
  }

  # # if the input object is not a spatial object stop
  # if(typ != 'SpatialPointsDataFrame' & typ != 'SpatialPoints' &
  #    typ != 'SpatialLinesDataFrame' & typ != 'SpatialLines'&
  #    typ != 'SpatialPolygonDataFrame' & typ != 'SpatialPolygons'){
  #   stop("The input object must be of type SpatialXXXXX(DataFrame)", call.=FALSE)
  # }

  if(is.null(newobj.name)){
    newobj.name <- paste0(input,".buff")
  }

  calltext <- call("gBufferDS", input, by_id, ip_width )
  DSI::datashield.assign(datasources, newobj.name, calltext)



}

