## The required packages must be supplied like this on the second line of the file as it is used to check the packages are installed before launching:
packages <- c("goldeneye","shiny","shinythemes")

library("goldeneye")
library("keyring")
library("tidyverse")
library("shiny")
library("shinythemes")

## Set up default user if possible:
password_text <- "Please enter your password"
default <- ""
default_password <- ""
profile_choices <- c(`Select Existing` = "select", `Create New` = "new")
profile_selected <- character(0)

if(!is.null(getOption('goldeneye_path')) && file.exists(getOption('goldeneye_path'))){
  default <- readRDS(getOption('goldeneye_path'))$email
  stopifnot(!is.null(default))
  profile_choices <- c(`Default` = "default", `Select Other` = "select", `Create New` = "new")
  names(profile_choices) <- c(str_c("Saved: ", default), "Select Other", "Create New")
  profile_selected <- "default"

  keys <- key_list("goldeneye")
  if(default %in% keys$username){
    default_password <- key_get("goldeneye", default)
    password_text <- "Password retrieved from keyring"
  }
}
