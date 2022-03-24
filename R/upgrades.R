upgrade_user <- function(local, path){

  if(numeric_version(local[["versions"]][["actual"]]) < "0.4.1"){

    stop("Please update the goldfinger package and then run the function goldfinger::gy_check() before proceeding", call.=FALSE)

  }

  if(numeric_version(local[["versions"]][["actual"]]) < "0.5.0"){

    ## Upgrade from goldfinger:
    stopifnot("user" %in% names(local), "goldfinger" %in% local$groups)
    local$groups$goldfinger$user <- local$user
    local$setup_date <- as.Date(local$versions["date_time"])
    names(local$setup_date) <- NULL

    # NB: don't remove the local$user until goldfinger is fully migrated
    # local$user <- NULL
    # Once this is activated also re-order the names as so:
    epn <- c("name", "email", "setup_date", "versions", "public_curve", "public_ed", "salt", "encr_curve", "encr_ed", "groups")

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
