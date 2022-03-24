#' Title
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
gy_key_funs <- function(type){

  stopifnot(type=="identity")
  rv <- list(type="identity")
  class(rv) <- "gy_key_funs"

  return(rv)
}
