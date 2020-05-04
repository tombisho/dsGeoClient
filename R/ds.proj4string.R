#' @title Wrapper for \code{proj4string()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{gBuffer()} function from the
#' sp and rgeos packages
#' @details See the \code{gBuffer()} function from rgeos package for more details
#' @param input name of a spatial object on the server side to which the buffer will be applied
#' @param projStr projStr
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
ds.proj4string = function(input=NULL, projStr = NULL, newobj.name=NULL, datasources=NULL) {

  ##################################################################################
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(input)){
    stop("Please provide the name of an sp object as defined in package sp!", call.=FALSE)
  }

  # check if a valid width has been provided
  if(is.null(projStr)){
    stop("Please provide a valid width for the buffer!", call.=FALSE)
  }

  if(!is.character(projStr)){
    stop("projstr is not character!", call.=FALSE)
  }

  if(is.null(newobj.name)){
    newobj.name <- paste0(input,".proj")
  }

  calltext <- call("proj4stringDS", input, projStr)
  DSI::datashield.assign(datasources, newobj.name, calltext)

}

