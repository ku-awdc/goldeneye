#' Setup a new goldeyene encryption profile
#'
#' @param name your full name
#' @param email your email address
#' @param filename the filename to which the profile will be saved
#' @param path the path in which to save the profile
#' @param append_Rprofile should the R profile file be appended to automatically load this profile when R is restarted?
#' @param silent option to suppress output
#'
#' @importFrom stringr str_remove str_c
#' @importFrom getPass getPass
#' @importFrom keyring key_set_with_value key_get key_list key_delete
#' @importFrom sodium hash keygen pubkey sig_keygen sig_pubkey data_encrypt data_decrypt sig_sign sig_verify simple_encrypt simple_decrypt auth_encrypt auth_decrypt
#' @importFrom rstudioapi selectDirectory isAvailable
#'
#' @export
gy_setup <- function(name=NULL, email=NULL, filename=NULL, path=NULL, append_Rprofile=NULL, silent=FALSE){

  if(!silent) cat("#### Setup goldeneye profile ####\n")

  ## First ask for name and email:
  if(is.null(name)) name <- readline(prompt="Name:  ")
  if(is.null(email)) email <- readline(prompt="Email:  ")

  ## Then password:
  ## [Note: to set password externally, just set it using keyring]
  if(email %in% key_list("goldeneye")[,"username"]){
    if(!silent) cat("Note: Re-using existing keyring password associated with email address '", email, "'\n", sep="")
    pass <- key_get("goldeneye", username=email)
  }else{
    repeat{
      pass <- getPass(msg="Password:  ", noblank = TRUE)
      pass2 <- getPass(msg="Password (confirm):  ", noblank = TRUE)
      if(pass==pass2) break
      cat("Error:  passwords do not match!  Try again...\n")
    }
    # Store the password:
    key_set_with_value("goldeneye", email, pass)
  }

  ## File locations:
  repeat{
    if(is.null(filename)){
      filename <- readline(prompt=str_c("User file (leave blank to accept goldeneye_private.gyp):  "))
    }
    if(is.null(filename) || filename=="") filename <- "goldeneye_private.gyp"

    if(is.null(path) || !dir.exists(path)){
      cat("Please select a location to store this file...\n")
      # rstudioapi version:
      if(isAvailable("1.1.288")){
        Sys.sleep(1)
        path <- selectDirectory()
      }else{
        repeat{
          path <- readline(prompt="Directory to use:  ")
          if(dir.exists(path)) break
          cat("Error: directory not found ... please try again\n")
        }
      }
    }

    if(!file.exists(file.path(path, filename))) break
    cat("Error:  file already exists, enter a new filename\nor manually delete the old file before proceeding\n")
    filename <- NULL
    path <- NULL
  }

  # Generate and store a salt:
  salt <- str_c(sample(c(letters,LETTERS,0:9),6),collapse="")
  # Convert to symmetric encryption key:
  sym_key <- hash(charToRaw(str_c(salt,pass)), size=32)

  ## Set up asymmetric curve25519 key pair for encryption:
  private_curve <- keygen()
  public_curve <- pubkey(private_curve)
  # Then encrypt the private curve key:
  encr_curve <- data_encrypt(private_curve, sym_key)
  stopifnot(identical(private_curve, data_decrypt(encr_curve, sym_key)))

  ## Set up asymmetric ed25519 key pair for signing:
  private_ed <- sig_keygen()
  public_ed <- sig_pubkey(private_ed)
  # Then encrypt the private ed key:
  encr_ed <- data_encrypt(private_ed, sym_key)
  stopifnot(identical(private_ed, data_decrypt(encr_ed, sym_key)))

  ## Tests:
  msg <- serialize("test", NULL)
  tt <- sig_sign(msg, private_ed)
  stopifnot(sig_verify(msg, tt, public_ed))
  tt <- simple_encrypt(msg, public_curve)
  stopifnot(identical(msg, simple_decrypt(tt, private_curve)))

  ## Create the storage file:
  versions <- get_versions(type="generic")
  private_save <- list(name=name, email=email, setup_date=Sys.Date(), versions=versions, public_curve=public_curve, public_ed=public_ed, salt=salt, encr_curve=encr_curve, encr_ed=encr_ed, groups=list(default_group=NA_character_))
  saveRDS(private_save, file=file.path(path, filename), compress=FALSE)

  if(!silent) cat("#### Setup complete ####\n")

  ## Add the path to the storage file to the user's Rprofile:

  if(!base::isFALSE(append_Rprofile)){
    rprofline <- str_c("options(goldeneye_path='", file.path(path, filename), "')\n")
    eval(parse(text=rprofline))
    if(!isTRUE(append_Rprofile)){
      cat("In order for goldeneye to work between R sessions, you need\nto add the following line to your R profile:\n", rprofline, "\n")
      ok <- readline(str_c("To do this automatically (for '", file.path("~", ".Rprofile"), "') type y:  "))
      if(tolower(ok)=="y"){
        append_Rprofile <- TRUE
      }
    }

    if(isTRUE(append_Rprofile)){
      cat("\n\n## Added by the goldeneye package on ", as.character(Sys.Date()), ":\n", rprofline, "\n\n", sep="", file=file.path("~", ".Rprofile"), append=TRUE)
      if(!silent) cat("R profile file appended\n")
    }
  }

  invisible(gy_profile(file.path(path, filename)))

}
