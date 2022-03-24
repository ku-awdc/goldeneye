#' @name gy_join_group
#' @title Join an existing goldeneye user group via a weblink
#'
#'
#' @param weblink the weblink as provided by the group administrator
#' @param user the username that you want to use for this group
#'
#' @importFrom stringr str_glue
#'

#' @rdname gy_join_group
#' @export
gy_join_group <- function(weblink=NULL, user=NULL, make_default=FALSE){

  # Check that we have a valid user profile:
  local <- gy_check()
  # TODO: catch an error here and give message to run gy_setup?

  if(is.null(package_env$currentfile)){
    stop("An unexpected error occured when retrieving the filename of your user profile - please report this error to the package maintainer")
  }

  if(is.null(weblink)){
    ## First ask for web link and password for users file
    weblink <- readline(prompt="Setup link:  ")
    # Should be in the format https://*link*#*password*#*admin*
  }

  # Test validity and obtain current user information:
  keys <- refresh_users(weblink, setup=TRUE, silent=TRUE)
  existingusernames <- tolower(keys$usernames)

  # Check we are not already a member:
  if(keys[["group"]] %in% names(local$groups)){
    stop(str_c("You are already a member of the '", keys[["group"]], "' group"), call.=FALSE)
  }
  ## TODO: gy_leave_group to allow removal of group info

  ## Then ask for username:
  email <- local$email
  # For old goldfinger profiles:
  if(!is.null(user)){
    tuser <- user
  }else if(!is.null(local$user)){
    tuser <- local$user
  }else if(!is.na(local$groups$default_group)){
    if(!default_group %in% names(local$group)){
      stop("An unexpected error occured with the groups element of your user profile: please contact the package maintaner with this error message for help")
    }
    tuser <- local$groups[[local$groups$default_group]][["user"]]
  }else{
    tuser <- tolower(str_remove(email, "@.*"))
  }

  chkuser <- function(user, err=FALSE){
    msg <- ""
    if(tolower(user)=="local_user") msg <- ("The username 'local_user' cannot be used")
    if(tolower(user)=="all") msg <- ("The username 'all' cannot be used")
    if(tolower(user)=="admin") msg <- ("The username 'admin' cannot be used")
    if(gsub("[[:alnum:]]","",user)!="") msg <- ("The username can only contain ASCII letters and numbers")
    if(tolower(user) %in% existingusernames) msg <- str_c("The username '", tolower(user), "' is already taken within this group: to re-use your own username please contact the group admin")
    if(err && msg!="") stop(msg, call.=FALSE)
    invisible(msg)
  }
  #chkuser(tuser, err=TRUE)

  ## Update the storage file:
  local$versions <- get_versions(type="generic")

  # Allow a single profile file to contain multiple groups:
  newgroups <- list(ngp=list(user=tuser, weblink=weblink, admin_ed=keys[["admin_ed"]]))
  names(newgroups) <- keys[["group"]]
  local$groups <- c(local$groups, newgroups)

  saveRDS(local, file=package_env$currentfile, compress=FALSE)

  rv <- gy_profile(package_env$currentfile, silent=TRUE)

  cat("Your profile file has been updated to add you to group '", keys[["group"]], "'\n", sep="")

  ## Get stuff to send back to the group owner:
  public_save <- local[c("user", "name", "email", "setup_date", "versions", "public_curve", "public_ed")]
  public_save$user <- tuser
  public_encry <- data_encrypt(serialize(public_save, NULL), hash(charToRaw(keys$webpwd)))

  pfilen <- str_c("gy_", keys[["group"]], "_", tuser, "_public.gyu")
  saveRDS(public_encry, file=pfilen, compress=FALSE)

  msg <- str_glue(keys$message, .envir = as.environment(list(path = pfilen)))
  if(isTRUE(getOption("goldeneye_redact"))){
    msg <- gsub("matthewdenwood", "***", msg)
  }else{
    pfilen <- file.path(getwd(), pfilen)
  }

  cat(msg, "\nNOTE: in sending this file, you consent to your name and email address (as given above)\nbeing stored and made available in encrypted form via:\n", keys$weburl, "\n", sep="")

  gy_set_group(keys[["group"]], make_default=make_default)
  invisible(gy_profile(package_env$currentfile, silent=TRUE))

}

#' @rdname gy_join_group
#' @export
gy_set_group <- function(group, make_default=FALSE, silent=FALSE){

  stopifnot(is.character(group), length(group)==1L)
  local <- gy_check()

  if(!is.na(group) && !group %in% names(local[["groups"]])){
    stop("You are not a member of that group")
  }

  package_env$currentgroup <- group

  if(make_default && (is.na(group) || group!=local$groups$default_group)){
    local$groups$default_group <- group
    saveRDS(local, file=package_env$currentfile, compress=FALSE)
  }

  if(!silent) cat("Your group has been set to '", group, "'\n", sep="")

  invisible(group)
}

#/' @rdname gy_join_group
#/' @export
gy_leave_group <- function(group){

  stopifnot(is.character(group), length(group)==1L, !is.na(group))
  local <- gy_check()

  if(!group %in% names(local[["groups"]])){
    stop("You are not a member of that group")
  }

  local$groups <- local$groups[!names(local$groups)%in%group]
  local$versions <- get_versions(type="generic")

  saveRDS(local, file=package_env$currentfile, compress=FALSE)

  if(package_env$currentgroup==group || package_env$defaultgroup==group){
    gy_set_group(NA_character_, make_default=package_env$defaultgroup==group, silent=TRUE)
  }

  cat("You have been removed from the group '", group, "'\n", sep="")
  invisible(gy_profile(package_env$currentfile))

}
