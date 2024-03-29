#' Obtain a list of users for a given group
#'
#' @param group the group for which to return users (defaults to the active group)
#' @param refresh should the list be refreshed from the remote/online location?
#'
#' @export
gy_users <- function(group=NULL, refresh=FALSE){

  if(is.null(package_env$currentlocal)) gy_profile(silent=TRUE)
  if(is.null(group)) group <- package_env$currentgroup

  stopifnot(is.character(group), length(group)==1L, !is.na(group))
  local <- gy_check()

  redact <- getOption('goldeneye_redact')
  if(is.null(redact)) redact <- FALSE
  stopifnot(is.logical(redact) && length(redact)==1 && !is.na(redact))

  users <- get_users(all_users=TRUE, group=group, refresh=refresh)

  ## TODO: prettier printing

  usrs <- t(vapply(users, function(x){
    c(unlist(x[c("user","name","email")]), member_since=as.character(x$member_since))
    }, character(4))) %>%
    as_tibble() %>%
    mutate(member_since=as.Date(.data$member_since)) %>%
    arrange(.data$member_since)

  if(redact) usrs$email <- "**@**.**"

  return(usrs)
}
