#' @name gy_zip
#' @title Zip and encrypt external files using goldeneye
#' @param input_files one or more files to add to the zip archive
#' @param file the filename to which the encrypted object will be saved
#' @param compression_level compression level to use on a scale of 1-9 (passed to \code{\link[zip]{zip}})
#' @param users a character vector of (other) users within your current user group for whom the encrypted file will be decryptable. Alternatively, this can be a vector of paths/urls to public keys, or a mixture of the two.
#' @param local_user should the current user also be able to decrypt thw file?
#' @param comment an optional comment that will be sent (unencrypted) along with the file
#' @param funs optional additional encryption steps: this must be the output of a call to \code{\link{gy_key_funs}}
#' @param directory the directory to which the decrypted/unzipped files will be saved
#' @param unzip should the decrypted files also be unzipped?
#' @param overwrite should the file be overwritten, if it exists?
#' @param run_custom should any custom decryption functions be run automatically?
#'
#' @importFrom zip zip unzip zip_list
#' @importFrom base64enc base64encode base64decode
#'
#' @rdname gy_zip
#' @export
gy_zip <- function(input_files, file=stop("file must be specified (.rfg file extension is recommended)"), compression_level=5, users=character(0), local_user=TRUE, comment="", funs = gy_key_funs("identity")){

  stopifnot(all(file.exists(input_files)))

  ## Create the ZIP and encode:
  tmpfl <- tempdir(check=TRUE)
  zip(file.path(tmpfl, "gyarchive.zip"), files=input_files, recurse=TRUE, compression_level=compression_level, include_directories=TRUE, mode="mirror")
  b64enc <- base64encode(file.path(tmpfl, "gyarchive.zip"))
  file.remove(file.path(tmpfl, "gyarchive.zip"))

  ## Serialise and encrypt:
  ser_obj <- gy_serialise(b64enc, method="qs")
  enc_obj <- gy_encrypt(ser_obj, users=users, local_user=local_user, comment = comment, funs = funs)

  ## Add the gy_type:
  enc_obj$metadata$gy_type <- "gy_zip"

  ## And save:
  saveRDS(enc_obj, file=file, ascii=FALSE, compress=FALSE)

  invisible(file)
}

#' @rdname gy_zip
#' @export
gy_unzip <- function(file=stop("file must be specified (.rfg file extension is recommended)"), directory=getwd(), unzip=TRUE, overwrite=FALSE, run_custom=TRUE){

  stopifnot(file.exists(file))

  ## Read file:
  enc_obj <- readRDS(file)

  ## Unencrypt and deserialise:
  ser_obj <- gy_decrypt(enc_obj, run_custom=run_custom)
  object <- gy_deserialise(ser_obj)

  ## De-encode and save to file:
  tmpfl <- tempdir(check=TRUE)
  writeBin(base64decode(object), file.path(tmpfl, "archive.zip"))

  file_list <- zip_list(file.path(tmpfl, "archive.zip"))$filename
  ## NULL unzip means if only 1 file
  if(is.null(unzip)){
    unzip <- length(file_list)==1L
  }

  if(unzip){
    ## Find non-existing files:
    to_extract <- !file.exists(file.path(directory, file_list))
    if(!overwrite){
      if(all(!to_extract)) stop("No files were extracted as all files exist already - set overwrite=TRUE to overwrite files")
      if(any(!to_extract)) warning("One or more files were not extracted as the file(s) already exist - set overwrite=TRUE to overwrite files")
    }else{
      if(any(file.exists(file.path(directory, file_list)))) cat("Note: one or more files were over-written")
    }
    unzip(file.path(tmpfl, "archive.zip"), files=file_list[to_extract], overwrite=overwrite, junkpaths=FALSE, exdir=directory)
    file.remove(file.path(tmpfl, "archive.zip"))

    invisible(data.frame(File=file_list, Extracted=to_extract))

  }else{

    file.copy(file.path(tmpfl, "archive.zip"), file.path(directory, "archive.zip"))
    file.remove(file.path(tmpfl, "archive.zip"))

    invisible("archive.zip")

  }


}
