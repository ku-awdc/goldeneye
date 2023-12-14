#' @name gf_saveRDS
#' @title Save and read encrypted RDS
#'
#' @param object the object to encrypt
#' @param file the filename to which the encrypted object will be saved
#' @param users one or more users who will be authorised to decrypt the file
#' @param local_user should the local user be able to decrypt the file?
#' @param comment a comment to include
#' @param overwrite should the file be overwritten, if it exists?
#' @param ascii argument passed to saveRDS
#' @param funs argument passed to saveRDS
#' @param ... arguments passed to gy_serialise
#' @param run_custom should any custom decryption functions be run automatically?
#'
#' @rdname gy_saveRDS
#' @export
gy_saveRDS <- function(object, file=stop("file must be specified (.rdg file extension is recommended)"), users=character(0), local_user=TRUE, comment = "", overwrite=FALSE, ascii = FALSE, funs = list(type="identity"), ...){

  if(file.exists(file) && !overwrite) stop("Specified file exists: use overwrite=TRUE if necessary", call.=FALSE)

  if("gy_type" %in% names(attributes(file))){
    gy_type <- attr(file, "gy_type", TRUE)
  }else{
    gy_type <- "gf_saveRDS"
  }

  ## Serialise and encrypt:
  ser_obj <- gy_serialise(object, ...)
  enc_obj <- gy_encrypt(ser_obj, users=users, local_user=local_user, comment = comment, funs = funs)

  ## Add the gy_type:
  enc_obj$metadata$gy_type <- gy_type

  ## And save:
  saveRDS(enc_obj, file=file, ascii=ascii, compress=FALSE)

}


#' @rdname gy_saveRDS
#' @export
gy_readRDS <- function(file=stop("file must be specified (.rdg file extension is recommended)"), run_custom=TRUE){

  ## Read file:
  enc_obj <- readRDS(file)

  ## Unencrypt and deserialise:
  ser_obj <- gy_decrypt(enc_obj, run_custom=run_custom)
  object <- gy_deserialise(ser_obj)

  return(object)

}
