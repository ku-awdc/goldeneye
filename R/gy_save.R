#' @name gy_save
#' @title Save and load encrypted files
#'
#' @param ... one or more R objects to encrypt
#' @param list alternative method to provide R objects to encrypt as a character vector
#' @param file the name of the encrypted file to create/load (a .rdg file extension is recommended)
#' @param users a character vector of (other) users within your current user group for whom the encrypted file will be decryptable. Alternatively, this can be a vector of paths/urls to public keys, or a mixture of the two.
#' @param local_user should the current user also be able to decrypt thw file?
#' @param comment an optional comment that will be sent (unencrypted) along with the
#' @param overwrite if a file with the same name already exists, should it be overwritten?
#' @param funs optional additional encryption steps: this must be the output of a call to \code{\link{gy_key_funs}}
#' @param method the serialisation method to use (currently either 'qs' or 'base')

#' @rdname gy_save
#' @export
gy_save <- function(..., list=character(), file=stop("file must be specified (.rdg file extension is recommended)"), users=character(0), local_user=TRUE, comment = "", overwrite=FALSE, funs = list(type="identity"), method="qs"){

  names <- as.character(substitute(list(...)))[-1L]
  if(length(names)==0) stop("No objects passed to be saved", call.=FALSE)
  list <- c(list, names)
  objects <- mget(list, inherits=TRUE)
  names(objects) <- list

  file <- eval(file)
  attr(file, "gy_type") <- "gy_save"
  gy_saveRDS(objects, file=file, users=users, local_user=local_user, comment=comment, overwrite=overwrite, funs=funs, method=method)

}


#' @rdname gy_save
#' @export
gy_load <- function(file=stop("file must be specified (.rdg file extension is recommended)")){

  fcon <- readRDS(file)
  if(fcon$metadata$gy_type != "gy_save") warning("Archive was created using ", fcon$metadata$gy_type, " not gy_save")

  objects <- gy_readRDS(file)
  stopifnot(inherits(objects, "list"))

  for(i in seq_len(length(objects))){
    assign(names(objects)[i], objects[[i]], envir=parent.frame())
  }

  invisible(names(objects))

}
