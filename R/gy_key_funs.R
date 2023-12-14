#' Obtain key functions
#'
#' @param type type of key function (only "identity" is currently supported)
#'
#' @export
gy_key_funs <- function(type){

  stopifnot(type=="identity")
  rv <- list(type="identity")
  class(rv) <- "gy_key_funs"

  return(rv)
}
