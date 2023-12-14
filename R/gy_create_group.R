#/' Create a new goldeneye user group
#'
#/' @param group
#/' @param weblink
#/' @param password
#'
#/' @export
gy_create_group <- function(group, weblink, password){

  browser()

  stop("Function not yet implemented")

  group <- tolower(group)
  if(group=="default") stop("The group name 'default' is reserved", call.=FALSE)
  ## Get local user info (will be admin):
  user <- get_localuser()


  weblink <- readRDS(getOption("goldeneye_path"))$groups$goldfinger$weblink
  webinfo <- refresh_users(weblink)
  user_info <- webinfo$users
  names(user_info)

  user_info <- user_info["md"]
  usernames <- unique(c(webinfo$usernames, names(user_info)))
  usernames <- "md"

  ## There are two types of public key, neither of which need to be encrypted:
  public_curve <- lapply(user_info, function(x) x$public_curve)
  public_ed <- lapply(user_info, function(x) x$public_ed)
  #public_ed$md <- readRDS('~/Documents/Personal/goldfinger_md.gyp')$public_ed
  #stopifnot(identical(readRDS('~/Documents/Personal/goldfinger_md.gyp')$public_curve, public_curve$md))


  user_info <- lapply(user_info, function(x){
    x$user_since <- x$setup_date
    x$member_since <- Sys.Date()
    x <- x[!names(x)%in%c("public_key","public_ed","public_curve","date_time")]
    stopifnot(inherits(x$user_since, "Date"), inherits(x$member_since, "Date"))
    x
  })

  #webpwd <- paste(sample(c(letters,LETTERS,0:9),10,TRUE), collapse="")

  msg <- "Please email the file '{path}' to:  matthewdenwood+demo22@gmail.com"

  users <- list(usernames=usernames, user_info=sodium::data_encrypt(serialize(user_info, NULL), sodium::hash(charToRaw(webpwd))), message=sodium::data_encrypt(serialize(msg, NULL), sodium::hash(charToRaw(webpwd))), public_curve=public_curve, public_ed=public_ed)

  verification <- gy_sign(users)

  versions <- attr(verification, "versions")
  versions["type"] <- "generic"
  versions["minimum"] <- "0.5.0-1"
  attr(verification, "versions") <- versions
  stopifnot(gy_verify(users, verification, silent=TRUE))
  attr(verification, "user") <- NULL

  keys <- list(group="demo22", users=users, verification=verification)
  saveRDS(keys, "demo22.gyg", compress=FALSE)

  webpwd <- NULL
  weblink <- str_c("https://www.costmodds.org/rsc/goldeneye/demo22.gyg#",webpwd,"#md")


}
