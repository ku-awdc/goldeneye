#' @name gy_encrypt
#' @title Encrypt and decrypt a pre-serialised object using goldeneye
#' @param object the serialised object to encrypt
#' @param users a character vector of (other) users within your current user group for whom the encrypted file will be decryptable. Alternatively, this can be a vector of paths/urls to public keys, or a mixture of the two.
#' @param local_user should the current user also be able to decrypt thw file?
#' @param comment an optional comment that will be sent (unencrypted) along with the file
#' @param funs optional additional encryption steps: this must be the output of a call to \code{\link{gy_key_funs}}
#'
#' @rdname gy_encrypt
#' @export
gy_encrypt <- function(object, users=character(0), local_user=TRUE, comment = "", funs = gy_key_funs("identity")){

  if(!is.raw(object)) stop("The object argument must be a single serialised object", call.=FALSE)

  # Changed argument name:
  user <- users

  ser_method <- attr(object, "ser_method", exact=TRUE)
  if(is.null(ser_method)){
    ser_method <- "custom"
  }
  ser_versions <- attr(object, "versions", exact=TRUE)
  if(is.null(ser_versions)){
    ser_versions <- get_versions(type="deserialise")
  }

  localuser <- get_localuser()

  ## TEMPORARY HACK
  if(is.list(user)){
    ## Users can be specified as a named list of keys (names are emails)

    if(local_user){
      lu <- list(key = localuser$public_curve)
      names(lu) <- localuser$email
      user <- c(user, lu)
    }

    keys <- user
    user <- names(keys)

  }else{
    ## Shortcut for all users in the current group:
    if("all" %in% user){
      user <- unique(c(user, names(keys)))
      user <- user[!user %in% c("all","local_user")]
    }
    if("local_user" %in% user) stop("Invalid user 'local_user' - please use your true username", call.=FALSE)

    if(local_user){
      ## Remove duplicate user if there to avoid confusion:
      user <- user[!user %in% localuser]
      user <- c(user, "local_user")
    }

    ## Ensure we have at least one user:
    if(length(user)==0L) stop("No decrypt users specified!")

    ## Get public keys for all specified users:
    keys <- get_public_keys(user, type="curve")
    user <- names(keys)

  }


  ## Get private and public keys for this user:
  private_key <- get_gykey(localuser$email, localuser$salt, localuser$encr_curve)
  public_key <- localuser$public_curve
  public_test <- pubkey(private_key)
  if(!identical(public_key, public_test)) stop("Something went wrong: the public curve key cannot be regenerated", call.=FALSE)

  ## Generate a symmetric encryption key:
  sym_key <- keygen()

  ## Process the encr_fun types:
  if(!is.list(funs) || !"type" %in% names(funs)){
    stop("The funs argument must be a list, with first element 'type'", call.=FALSE)
  }
  if(".x" %in% names(funs)) stop("The name .x is reserved and cannot be in funs", call.=FALSE)
  type <- funs[["type"]]

  # Currently only two supported options:
  ## TODO: more types, and make run_custom=FALSE default
  if(type=="identity"){
    # Nothing to do here:
    funs <- serialize(sym_key, NULL)
  }else if(type=="custom"){
    if(!all(c("encr_fun","decr_fun") %in% names(funs))) stop("For custom funs you must supply both encr_fun and decr_fun")
    encr_fun <- funs$encr_fun
    if(!is.function(encr_fun)) stop("The encr_fun supplied must be a function", call.=FALSE)
    if(!length(formals(encr_fun))==1) stop("The encr_fun supplied must be a function that takes a single argument", call.=FALSE)
    decr_fun <- funs$decr_fun
    if(!is.function(decr_fun)) stop("The decr_fun supplied must be a function", call.=FALSE)
    if(!length(formals(decr_fun))==1) stop("The decr_fun supplied must be a function that takes a single argument", call.=FALSE)
  }

  ## Check encr_fun and decr_fun are symmetric:
  if(type!="identity"){
    funs$.x <- encr_fun(sym_key)
    if(!identical(sym_key, decr_fun(funs[[".x"]]))) stop("The provided encr_fun and decr_fun are not symmetric", call.=FALSE)
    funs <- serialize(funs, NULL)
  }

  ## Encrypt this for each user:
  decrypt_key <- lapply(user, function(u){
    public <- if(u=="local_user") localuser$public_curve else keys[[u]]

    rand <- sample.int(length(funs))
    key_rand <- funs[rand]
    reorder <- order(rand)
    stopifnot(all(key_rand[reorder]==funs))

    keyval <- list(user = ifelse(u=="local_user", hashify(localuser$email), u),
                   key_rand = key_rand,
                   reorder = reorder
    )
    class(keyval) <- "goldeneye_symkey"

    auth_encrypt(serialize(keyval, NULL), private_key, public)
  })
  user[user=="local_user"] <- hashify(localuser$email)
  names(decrypt_key) <- user

  ## Encrypt the objects themselves:
  object_encr <- data_encrypt(object, sym_key)
  # Add the serialization method as an attribute:
  attr(object_encr, "ser_method") <- ser_method
  # Add the serialization versions as an attribute:
  attr(object_encr, "versions") <- ser_versions

  ## Package the metadata:
  metadata <- list(creator=hashify(localuser$email), public_curve=localuser$public_curve, comment=comment, versions=get_versions(type="decrypt"), date_time=Sys.time())

  ## And return:
  retval <- list(metadata=metadata, decrypt=decrypt_key, object_encr=object_encr)
  class(retval) <- c("goldeneye","list")
  return(retval)

}


#' @rdname gy_encrypt
#' @export
gy_decrypt <- function(object, run_custom = TRUE){

  ## See if we are dealing with an old save format, and if so then upgrade:
  object <- upgrade_encrypt(object)
  versions <- object$metadata$versions
  if(is.null(versions)) stop("The versions element is missing")
  check_version(versions)

  ## Determine the local user:
  localuser <- get_localuser()

  ## Get private and public keys for this user:
  private_key <- get_gykey(localuser$email, localuser$salt, localuser$encr_curve)
  public_key <- localuser$public_curve
  public_test <- pubkey(private_key)
  if(!identical(public_key, public_test)) stop("Something went wrong: the public curve key cannot be regenerated", call.=FALSE)

  ## Find the relevant decrypt key:
  if(! hashify(localuser$email) %in% names(object$decrypt)){
    stop("You are not authorised to decrypt this file", call.=FALSE)
  }
  crypt <- object$decrypt[[hashify(localuser$email)]]

  key <- find_key(object$metadata$creator, type="curve")
  if(length(key)>0L){
    if(!identical(object$metadata$public_curve, key)){
      stop("The data has been tampered with", call.=FALSE)
    }
  }else{
    if(object$metadata$creator != hashify(localuser$email)){
      message("The user that sent this file is not registered with any of your groups: it is therefore not possible to verify that the data has not been tampered with")
    }
  }

  ser_method <- attr(object$object_encr, "ser_method", exact=TRUE)
  if(is.null(ser_method)){
    warning("The provided object did not have a serialization method attribute - assuming that this is base::serialize")
    ser_method <- "base"
  }
  versions <- attr(object$object_encr, "versions", exact=TRUE)
  if(is.null(versions)) stop("The versions attribute is missing")
  check_version(versions)

  uncrypt <- unserialize(auth_decrypt(crypt, private_key, object$metadata$public_curve))
  stopifnot(inherits(uncrypt, "goldeneye_symkey"))
  stopifnot(uncrypt$user == localuser$email)

  # Unserialise:
  funs <- unserialize(uncrypt$key_rand[uncrypt$reorder])
  if(is.raw(funs)){
    type <- "identity"
  }else{
    stopifnot("type" %in% names(funs))
    type <- funs[["type"]]
  }

  if(type=="identity"){
    # If the key is just a key:
    sym_key <- funs
  }else if(type=="custom"){
    stopifnot("decr_fun" %in% names(funs))

    # If the key is a function then only run it if we have permission:
    # (as we cannot vouch for potential side effects):
    if(!run_custom){
      stop("The decryption algorithm requires running a function:  if you trust the source of the file then try again with the argument run_custom=TRUE", call.=FALSE)
    }
    sym_key <- funs[["decr_fun"]](funs[[".x"]])
  }else{
    stop("The decryption key/function is invalid", call.=FALSE)
  }

  # Decrypt:
  object <- data_decrypt(object$object_encr, sym_key)

  # Add ser_method and versions:
  attr(object, "ser_method") <- ser_method
  attr(object, "versions") <- versions

  return(object)

}


#' @rdname gy_encrypt
#' @export
get_public_keys <- function(users, type="curve"){

  local <- gy_check()

  stopifnot(is.character(users), all(!is.na(users)))
  stopifnot(is.character(type), all(!is.na(type)), length(type)==1, type%in%c("curve","ed"))

  # Remove the local_user and re-add later:
  local_user <- FALSE
  if("local_user" %in% users){
    local_user <- TRUE
    users <- users[users!="local_user"]
  }

  # Separate into local file, web file, and group:
  islocal <- grepl("^file://", users) | file.exists(users)
  isweb <- grepl("^http", users)
  stopifnot(all((islocal+isweb) %in% c(0,1)))
  isgp <- !(islocal | isweb)
  keys <- vector("list", length=length(users)+local_user)
  keynames <- character(length(users)+local_user)
  users <- list(local = users[islocal], web = users[isweb], group = users[isgp])
  keyind <- 1

  if(length(users$group) > 0L){
    if(is.na(package_env$currentgroup) && any(!grepl(":", users$group, fixed=TRUE))){
      stop("No group is currently set", call.=FALSE)
    }
    # Explicitly add the group:
    users$group[!grepl(":", users$group, fixed=TRUE)] <- str_c(package_env$currentgroup, ":", users$group[!grepl(":", users$group, fixed=TRUE)])

    ## Check the desired user(s) are available:
    allgps <- unique(gsub(":[[:alnum:]]*$", "", users$group))
    if(!all(allgps %in% names(local$groups)) || "default_group" %in% allgps){
      stop("One or more specified group is invalid", call.=FALSE)
    }

    for(i in seq_along(users$group)){
      gp <- gsub(":[[:alnum:]]*$", "", users$group[i])
      usr <- gsub("^[[:alnum:]]*:", "", users$group[i])
      pkeys <- get_users(all_users=TRUE, group=gp, refresh=FALSE, silent=TRUE)

      if(!usr %in% names(pkeys)){
        stop(str_c("User '", usr, "' is not a member of group '", gp, "'"))
      }
      if(type=="curve"){
        keys[[keyind]] <- pkeys[[usr]][["public_curve"]]
      }else if(type=="ed"){
        keys[[keyind]] <- pkeys[[usr]][["public_ed"]]
      }else{
        stop("Unhandled type '", type, "'")
      }
      keynames[keyind] <- hashify(pkeys[[usr]][["email"]])
      keyind <- keyind+1
    }
  }

  if(length(users$local) > 0L){
    users$local <- gsub("^file://","",users$local)
    for(i in seq_along(users$local)){
      if(!file.exists(users$local[i])){
        stop("The specified key file '", users$local[i], "' was not found", call.=FALSE)
      }
      nk <- readRDS(users$local[i])
      if(!inherits(nk, "goldeneye_public")){
        stop("The specified key file '", users$local[i], "' is invalid", call.=FALSE)
      }
      nk <- unserialize(nk)

      if(type=="curve"){
        keys[[keyind]] <- nk$public_curve
      }else if(type=="ed"){
        keys[[keyind]] <- nk$public_ed
      }else{
        stop("Unhandled type '", type, "'")
      }
      keynames[keyind] <- hashify(nk$email)
      keyind <- keyind+1
    }
  }

  if(length(users$web) > 0L){
    for(i in seq_along(users$web)){
      urp <- url(users$web[i])
      ss <- try({
        nk <- readRDS(urp)
      })
      close(urp)
      if(inherits(ss, "try-error")){
        stop("The specified key file '", users$web[i], "' could not be read", call.=FALSE)
      }
      if(!inherits(nk, "goldeneye_public")){
        stop("The specified key file '", users$web[i], "' is invalid", call.=FALSE)
      }
      nk <- unserialize(nk)

      if(type=="curve"){
        keys[[keyind]] <- nk$public_curve
      }else if(type=="ed"){
        keys[[keyind]] <- nk$public_ed
      }else{
        stop("Unhandled type '", type, "'")
      }
      keynames[keyind] <- hashify(nk$email)
      keyind <- keyind+1
    }
  }

  if(local_user){
    pkeys <- get_users(all_users=FALSE, refresh=FALSE, silent=TRUE)
    stopifnot(length(pkeys)==1L)
    if(type=="curve"){
      keys[[keyind]] <- pkeys[[1L]][["public_curve"]]
    }else if(type=="ed"){
      keys[[keyind]] <- pkeys[[1L]][["public_ed"]]
    }else{
      stop("Unhandled type '", type, "'")
    }
    keynames[keyind] <- "local_user"
    keyind <- keyind+1
  }

  if(any(table(keynames)>1L)){
    stop("One or more user is duplicated", call.=FALSE)
  }

  names(keys) <- keynames

  return(keys)

}

find_key <- function(email, type="curve"){

  stopifnot(is.character(email), length(email)==1, !is.na(email))

  gps <- names(gy_check()$groups)
  allkeys <- lapply(gps[gps!="default_group"], function(x){
    ak <- get_users(all_users=TRUE, group=x, refresh=FALSE, silent=TRUE)
    we <- which(sapply(ak, function(x) x$email) == email)
    if(length(we)>0L){
      if(type=="curve"){
        rv <- ak[[we]][["public_curve"]]
      }else if(type=="ed"){
        rv <- ak[[we]][["public_ed"]]
      }else{
        stop("Unrecognised type")
      }
    }else{
      rv <- NULL
    }
    return(rv)
  })

  if(length(allkeys)>1L){
    # TODO: verify that all matched keys are the same
  }
  allkeys <- allkeys[[1L]]

  return(allkeys)
}

# hashify <- function(x) str_c(as.character(hash(charToRaw(x), size=16)), collapse="")
hashify <- function(x) x
