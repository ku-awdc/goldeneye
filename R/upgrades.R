upgrade_user <- function(local, path){

  if(numeric_version(local[["versions"]][["actual"]]) < "0.5.0"){

    stop("Please run the function goldfinger::upgrade_to_goldeneye() before proceeding", call.=FALSE)

  }

  return(local)

}



upgrade_encrypt <- function(object){

  # For potentially very old save versions:
  if(!is.null(object$metadata$package_version) && numeric_version(object$metadata$package_version) < 0.3){
    stop("Upgrading from version 1 or version 2 saves is not yet implemented", call.=FALSE)
    # Probably need to decrypt here and then re-encrypt using the new function??
  }
  if(!inherits(object, "goldeneye")) stop("The object to be decrypted must have been created using gy_encrypt", call.=FALSE)
  stopifnot(!is.null(object$metadata$versions) && numeric_version(object$metadata$versions["actual"]) >= 0.4)

  if(numeric_version(object$metadata$versions["actual"]) < "0.5.0"){
    ## Do something to upgrade if necessary
  }

  return(object)
}
