#' @title Wrapper for \code{geometry()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{geometry()} function from the
#' sp and rgeos packages
#' @details See the \code{geometry()} function from rgeos package for more details
#' @param input_x name of a spatial object on the server side to which the geometry will be applied
#' @param newobj.name a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame
#' followed by the suffix '.proj'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return an object of class SpatialPolygonsDataFrame or SpatialPolygons,
#' depending on the class of input
#' @author Bishop, T.
#' @export
#'
ds.geometry = function(input_x=NULL,  newobj.name=NULL, datasources=NULL) {

  ##################################################################################
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(input_x)){
    stop("Please provide the names of sp objects as defined in package sp!", call.=FALSE)
  }

  if(is.null(newobj.name)){
    newobj.name <- paste0(input_x,".geom")
  }

  calltext <- call("geometryDS", input_x)
  DSI::datashield.assign(datasources, newobj.name, calltext)

}

