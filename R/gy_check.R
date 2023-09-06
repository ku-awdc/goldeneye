#' @name gy_profile
#' @title Set a goldeneye profile and display user information
#'
#' @param path path to goldeneye profile
#' @param silent option to suppress output to screen
#'

#' @rdname gy_profile
#' @export
gy_profile <- function(path = getOption('goldeneye_path'), silent=FALSE){

  # For backwards compatibility:
  if(is.null(path)) path <- getOption('goldfinger_path')

  if(is.null(path)) stop("Path to the goldeneye user file not found: set options(goldeneye_path='...') and try again", call.=FALSE)
  if(!file.exists(path)) stop("No goldeneye user file found at ", path)

  local <- gy_check(upgrade_user(readRDS(path), path), silent=silent)

  # We should be guaranteed that this function is run to change path:
  package_env$currentfile <- path
  package_env$currentlocal <- local

  # Other functions may be used to change the default group:
  if(is.na(package_env$currentgroup)){
    package_env$currentgroup <- local$groups$default_group
  }

  if(!silent) cat("User set to '", local$name, "' (from file '", path, "')\n", sep="")

  invisible(local)
}

#' @rdname gy_profile
#' @export
gy_info <- function(silent=FALSE){

  redact <- getOption('goldeneye_redact')
  if(is.null(redact)) redact <- FALSE
  stopifnot(is.logical(redact) && length(redact)==1 && !is.na(redact))

  local <- gy_check(silent=TRUE)

  if(!silent) cat("User profile for '", local$name, "':\n", sep="")
  if(redact){
    if(!silent) cat("Email: '**@**.**'\n", sep="")
  }else{
    if(!silent) cat("Email: '", local$email, "'\n", sep="")
  }
  stopifnot(length(local$groups)>=1L)
  if(length(local$groups)==1L){
    if(!silent) cat("You are not a member of any user groups\n", sep="")
  }else if(length(local$groups)==2L){
    if(!silent) cat("Member of '", names(local$groups)[2], "' group with the following users:\n", sep="")
    ss <- try({
    users <- get_users(all_users=TRUE, group=names(local$groups)[2], refresh=FALSE, silent=TRUE)
    })
    if(inherits(ss, "try-error")){
      if(!silent) cat("\t[Unable to download user list]\n", sep="")
    }else{
      usrs <- t(vapply(users, function(x){
        c(unlist(x[c("user","name","email")]), member_since=as.character(x$member_since))
      }, character(4))) %>%
        as_tibble() %>%
        mutate(member_since=as.Date(member_since)) %>%
        arrange(member_since)
      if(redact) usrs$email <- "**@**.**"
      print(usrs)
    }

  }else{
    if(!silent) cat("Member of the following member groups:\n", sep="")
    for(gp in 2L:length(local$groups)){
      isdefault <- names(local$groups)[gp] == local$groups$default_group
      iscurrent <- names(local$groups)[gp] == package_env$currentgroup
      if(isdefault && iscurrent){
        cat("\t'", names(local$groups)[gp], "' (default and current group)\n", sep="")
      }else if(isdefault){
        cat("\t'", names(local$groups)[gp], "' (default group)\n", sep="")
      }else if(iscurrent){
        cat("\t'", names(local$groups)[gp], "' (current group)\n", sep="")
      }else{
        cat("\t'", names(local$groups)[gp], "'\n", sep="")
      }
    }
    if(!silent) cat("The current group '", package_env$currentgroup, "' has the following users:\n", sep="")
    ss <- try({
      users <- get_users(all_users=TRUE, group=package_env$currentgroup, refresh=FALSE, silent=TRUE)
    })
    if(inherits(ss, "try-error")){
      if(!silent) cat("\t[Unable to download user list]\n", sep="")
    }else{
      usrs <- t(vapply(users, function(x){
        c(unlist(x[c("user","name","email")]), member_since=as.character(x$member_since))
      }, character(4))) %>%
        as_tibble() %>%
        mutate(member_since=as.Date(member_since)) %>%
        arrange(member_since)
      if(redact) usrs$email <- "**@**.**"
      print(usrs)
    }

  }

  invisible(local)
}

# Don't export this:  user can call gy_profile or gy_info instead
gy_check <- function(local=NULL, silent=FALSE){

  if(is.null(local)) local <- package_env$currentlocal
  if(is.null(local)) local <- gy_profile()
  if(is.character(local)) local <- gy_profile(local)

  group <- package_env$currentgroup

  ## Check naming is OK:
  lcn <- names(local)
  epn <- c("name", "email", "setup_date", "versions", "public_curve", "public_ed", "salt", "encr_curve", "encr_ed", "groups")

  # For compatibility with older goldfinger saves that also had user:
  if(!length(lcn)%in%(length(epn)+c(0,1)) || !all(epn %in% lcn)){
    stop("An unexpected error occured while processing the user file - please contact the package author", call.=FALSE)
  }
  if(!is.list(local[["groups"]]) || !"default_group" %in% names(local[["groups"]])){
    stop("An unexpected error occured while processing the groups element of the user file - please contact the package author", call.=FALSE)
  }

  ## Obtain the private curve key:
  private_curve <- get_gykey(local$email, local$salt, local$encr_curve)
  public_curve <- local$public_curve
  public_test <- pubkey(private_curve)
  if(!identical(public_curve, public_test)) stop("Something went wrong: the public curve key cannot be regenerated", call.=FALSE)

  ## Obtain the private ed key:
  private_ed <- get_gykey(local$email, local$salt, local$encr_ed)
  public_ed <- local$public_ed
  public_test <- sig_pubkey(private_ed)
  if(!identical(public_ed, public_test)) stop("Something went wrong: the public ed key cannot be regenerated", call.=FALSE)

  invisible(local)
}
