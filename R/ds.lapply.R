#' @title Wrapper for \code{lapply()} function
#' @description This function is a wrapper for the \code{lapply()} function from the
#' sp and rgeos packages
#' @details See the \code{lapply()} function for more details
#' @param input name of a list
#' @param fun the function to apply as a character
#' @param newobj.name a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame
#' followed by the suffix '.list'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing
#' @author Bishop, T.
#' @export
#'
ds.lapply = function(input=NULL,fun = NULL, newobj.name=NULL, datasources=NULL) {

  ##################################################################################
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(input)){
    stop("Please provide the name of a list!", call.=FALSE)
  }

  if(is.null(newobj.name)){
    newobj.name <- paste0(input,".list")
  }

  calltext <- call("lapplyDS", input, fun)
  DSI::datashield.assign(datasources, newobj.name, calltext)

}

