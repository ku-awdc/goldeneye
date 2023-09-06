get_localuser <- function(){
  if(is.null(package_env$currentlocal)) gy_profile()

  local <- package_env$currentlocal
  local$group <- package_env$currentgroup
  local$user_since <- local$setup_date
  local$member_since <- as.Date(NA_character_)
  local$version <- local$versions[["actual"]]

  return(local)
}


# This function only gets called to set up a new user for a group:
refresh_users <- function(weblink, setup=FALSE, silent=FALSE, fail=TRUE){

  stopifnot(is.character(weblink), length(weblink)==1, !is.na(weblink))
  if(!str_detect(weblink, "#")) stop("Invalid setup link provided (no #)", call.=FALSE)
  if(!str_detect(weblink, "^https://") && !str_detect(weblink, "^file:///")) stop("Invalid setup link provided (not a URL)", call.=FALSE)

  weblink <- str_split(weblink, "#")[[1]]
  if(!length(weblink)==3) stop("Invalid setup link provided (cannot split twice on #)", call.=FALSE)

  if(!silent) cat("Downloading user list...\n")

  if(str_detect(weblink[1], "^https://")){

    ## TODO: use a url connection instead??
    tmpfl <- tempdir(check=TRUE)
    ss <- try({
      download.file(weblink[1], file.path(tmpfl, "users.gyg"), quiet=TRUE, mode="wb")
    }, silent=TRUE)
    on.exit(unlink(file.path(tmpfl, "users.gyg")))
    if(inherits(ss, "try-error")){
      if(fail){
        stop("Unable to download the user list: no internet connection?", call.=FALSE)
      }else{
        warning("Unable to download the user list: no internet connection?", call.=FALSE)
        invisible(NULL)
      }
    }

    info <- readRDS(file.path(tmpfl, "users.gyg"))
  }else{
    con <- file(weblink[1], open="rb")
    info <- readRDS(con)
    close(con)
  }

  check_version(attr(info$verification, "versions", exact = TRUE))

  public_ed <- info[["users"]][["public_ed"]]
  public_curve <- info[["users"]][["public_curve"]]

  if(setup){
    ## If this is a setup run then save the administrators public ed key:
    if(!weblink[3] %in% names(public_ed)) stop("Invalid admin username", call.=FALSE)
    admin_ed <- public_ed[[weblink[3]]]
  }else{
    admin_ed <- get_localuser()$groups[[get_localuser()$group]]$admin_ed
  }

  ## Verify the downloaded user:
  gy_verify(info$users, info$verification, public_ed=admin_ed, silent=TRUE)

  ## Decrypt and extract user information:
  user_info <- unserialize(data_decrypt(info[["users"]][["user_info"]], hash(charToRaw(weblink[2]))))
  # And message:
  if("message" %in% names(info[["users"]])){
    message <- unserialize(data_decrypt(info[["users"]][["message"]], hash(charToRaw(weblink[2]))))
  }else{
    message <- "Please email the file '{path}' to the group administrator"
  }

  stopifnot(all(names(user_info) %in% names(public_curve)))
  stopifnot(all(names(public_curve) %in% names(user_info)))
  stopifnot(all(names(user_info) %in% names(public_ed)))
  stopifnot(all(names(public_ed) %in% names(user_info)))

  un <- names(user_info)
  names(un) <- un
  users <- lapply(un, function(x){
    # Upgrade from goldfinger:
    if(!all(c("member_since","user_since") %in% names(user_info[[x]]))){
      user_info[[x]]$member_since <- as.Date(user_info[[x]]$date_time)
      user_info[[x]]$user_since <- as.Date(user_info[[x]]$date_time)
      user_info[[x]]$date_time <- NULL
    }
    c(user_info[[x]], list(public_curve=public_curve[[x]], public_ed=public_ed[[x]]))
  })

  ## Cache within environment:
  package_env$webcache[[info$group]] <- users

  keys <- list(users=users, weburl=weblink[1], webpwd=weblink[2], admin_user=weblink[3], admin_ed=admin_ed, group=info$group, usernames=info$users$usernames, message=message)

  invisible(keys)
}

# Function called repeatedly in a session:
get_users <- function(all_users=FALSE, group=package_env$currentgroup, refresh=FALSE, silent=FALSE){

  # Load user file if necessary:
  gy_check()

  if(!all_users){
    ll <- get_localuser()
    local <- list(ll[c("name","email","user","version","user_since","member_since","public_curve","public_ed")])
    names(local) <- local[[1]][["user"]]
    return(local)
  }

  if(refresh || is.null(package_env) || is.null(package_env$webcache[[group]])){
    refresh_users(get_localuser()[["groups"]][[group]][["weblink"]], setup=FALSE, silent=silent, fail=TRUE)
  }

  en <- c("name","email","user","version","user_since","member_since","public_curve","public_ed")
  lapply(package_env$webcache[[group]], function(x) stopifnot(all(names(x) %in% en), length(x) == length(en)))

  return(package_env$webcache[[group]])
}

check_version <- function(versions, local_versions=get_versions()){

  stopifnot(is.character(versions))
  stopifnot(all(c("type","date_time","minimum","actual","sodium","qs","rcpp","R") %in% names(versions)))
  stopifnot(versions["type"] %in% c("generic","decrypt","verify","deserialise"))

  if(numeric_version(versions["minimum"]) > numeric_version(local_versions["actual"])){
    type <- versions["type"]
    if(type=="decrypt"){
      msg <- "Decrypting this file requires an update of "
    }else if(type=="verify"){
      msg <- "Verification of this file requires an update of "
    }else if(type=="deserialise"){
      msg <- "Deserialisation of this file requires an update of "
    }else{
      if(!type=="generic"){
        warning("Unrecognised type in version check")
      }
      # NB: this includes downloading the users profile
      msg <- "You need to update "
    }
    cat("ERROR:  ", msg, "the goldeneye package (you have version ", local_versions["actual"], " but version ", versions["minimum"], " or later is required). To update the package run the following code:\n\ninstall.packages('goldeneye', repos=c('https://cran.rstudio.com/', 'https://ku-awdc.github.io/drat/'))\n", sep="")

    stop("Package update required", call.=FALSE)
  }

  invisible(TRUE)

}

get_versions <- function(...){
  retval <- c(package_env$versions, date_time=as.character(Sys.time()), ...)
  if(!"minimum" %in% names(retval)) retval <- c(retval, minimum="0.6.0-0")
  if(!"type" %in% names(retval)) retval <- c(retval, type="generic")
  check_version(retval, local_versions=retval)
  return(retval)
}



get_gykey <- function(username, salt, key_encr){

  ## TODO: limit the number of times this can fail using an env
  decrfun <- function(pass){
    pass_key <- hash(charToRaw(str_c(salt,pass)))
    data_decrypt(key_encr, pass_key)
  }

  tryCatch(
    decrfun(key_get("goldeneye", username=username)),
    error=function(e){
      tryCatch(key_delete("goldeneye", username=username), error=function(e) { })
      key_set_with_value("goldeneye", username, getPass(msg="Password:  "))
      decrfun(key_get("goldeneye", username=username))
    }
  )
}
