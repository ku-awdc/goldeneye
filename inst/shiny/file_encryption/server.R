## Statuses are:
## 0: nothing
## 1: type selected - validate password
## 2: create profile
## 3: profile created - download ready
## 4: setup verified (and profile downloaded)


function(input, output, session) {

  tempdir <- tempfile(pattern = "goldeneye", tmpdir = tempdir(check=TRUE))
  dir.create(tempdir)

  rv <- reactiveValues(
    profile_file = getOption('goldeneye_path'),
    status = if_else(profile_selected=="default", 1L, 0L),
    password_feedback = password_text,
    current_profile = "",
    profile_file_feedback = "Please select your private goldeneye key file",
    files_ready = 0,
    user_keys = list(),
    process_feedback = "",
    user_feedback = "Encrypting for self decrypt only",
    user_list = list(),
    user_length = -1,
    decrypt_feedback = "No file selected",
    decrypt_ready = 0,
    decrypt_filename = "",
    user_info = list()
  )

  add_user <- function(name, email){

    if(name!="" && email!=""){
      rv$user_list <- c(rv$user_list, list(c(name, email)))
    }

    if(input$user_self){
      rv$user_feedback <- str_c("The specified file(s) will be decrypted for self decryption (by ", rv$user_info$email, ") ")
    }else{
      rv$user_feedback <- str_c("The specified file(s) will be decrypted for self decryption (by ", rv$user_info$email, ") ")
    }
    if(length(rv$user_list)==0L){
      rv$user_feedback <- str_c(rv$user_feedback, " only<br>")
    }else{
      rv$user_feedback <- str_c(rv$user_feedback, " and the following users:<br>")
      for(i in seq_along(rv$user_list)){
        rv$user_feedback <- str_c(rv$user_feedback, "<br>", rv$user_list[[i]][1], " : ", rv$user_list[[i]][2])
      }
    }
  }

  observe({
    input$profile_file

    if(!is.null(input$profile_file)){

      path <- input$profile_file$datapath[1]
      prf <- readRDS(path)
      if(!all(c("name","email","salt","encr_curve") %in% names(prf))){
        rv$profile_file_feedback <- "ERROR: invalid goldeneye key file"
      }else{
        ss <- try({
          gy_profile(path)
        })
        if(inherits(ss,"try-error")){
          rv$profile_file_feedback <- "ERROR: there was a problem reading the goldeneye key file"
        }else{
          rv$profile_file_feedback <- "Private key file read successfully"
          rv$status <- 1L
          rv$profile_file <- path

          keys <- keyring::key_list("goldeneye")
          if(prf$email %in% keys$username){
            default_password <- key_get("goldeneye", prf$email)
            updateTextInput(session, "password_hide", value=default_password)
            updateTextInput(session, "password_show", value=default_password)
            rv$password_feedback <- "Password retrieved from keyring"
          }
        }
      }
    }
  })
  output$profile_file_feedback <- renderText(rv$profile_file_feedback)

  observe({
    input$password_show
    updateTextInput(session, "password_hide", value=input$password_show)
  })

  observe({
    input$password_hide
    updateTextInput(session, "password_show", value=input$password_hide)
  })

  observe({
    input$password_verify_show
    updateTextInput(session, "password_verify_hide", value=input$password_verify_show)
  })

  observe({
    input$password_verify_hide
    updateTextInput(session, "password_verify_show", value=input$password_verify_hide)
  })

  observe({
    input$password_new_show
    updateTextInput(session, "password_new_hide", value=input$password_new_show)
  })

  observe({
    input$password_new_hide
    updateTextInput(session, "password_new_show", value=input$password_new_hide)
  })

  observe({
    input$profile

    if(!is.null(input$profile) && (rv$current_profile != input$profile)){
      rv$current_profile <- input$profile
      rv$status <- case_when(
        input$profile=="default" ~ 1L,
        input$profile=="new" ~ 2L,
        TRUE ~ 0L
      )
    }
    #print(rv$status)

  })

  output$password_feedback <- renderText(str_c("<br>",rv$password_feedback))

  observeEvent(input$check_password, {

    if(!file.exists(rv$profile_file)){
      rv$password_feedback <- str_c("Error: profile file not found!")
    }else{
      ## Check validity of profile:
      ss <- try({
        prf <- readRDS(rv$profile_file)
      })
      if(inherits(ss,"try-error") || !"email"%in%names(prf)){
        rv$password_feedback <- str_c("Error: profile file invalid!")
      }else{
        ## Attempt to retrieve key as a test:
        if(input$password_hide!=input$password_show) stop("Hidden and shown passwords not equal!")
        pass_key <- sodium::hash(charToRaw(str_c(prf$salt,input$password_hide)))
        ss <- try({
          test <- sodium::data_decrypt(prf$encr_curve, pass_key)
        })
        if(inherits(ss,"try-error")){
          rv$password_feedback <- str_c("Error: password invalid!")
        }else{
          rv$password_feedback <- str_c("Password validated!")
          rv$status <- 4L
          if(input$password_hide!=keyring::key_get("goldeneye",prf$email)){
            keyring::key_set_with_value("goldeneye", prf$email, input$password_hide)
          }
          rv$user_info <- gy_profile(rv$profile_file)
        }
      }
    }

  })

  observeEvent(input$create_profile, {

    rv$status <- 2L

    if(input$setup_name==""){
      rv$setup_feedback <- "ERROR: please enter your name"
    }else if(input$setup_email=="" || !grepl("@", input$setup_email)){
      rv$setup_feedback <- "ERROR: please enter a valid email address"
    }else if(input$password_new_hide!=input$password_verify_hide){
      rv$setup_feedback <- "ERROR: passwords don't match!"
    }else{
      if(input$password_new_hide!=input$password_new_show){
        stop("Hidden and shown passwords (new) not equal!")
      }
      if(input$password_verify_hide!=input$password_verify_show){
        stop("Hidden and shown passwords (verify) not equal!")
      }
      ss <- try({
        keyring::key_set_with_value("goldeneye", input$setup_email, input$password_new_hide)

        gy_setup(name=input$setup_name, email=input$setup_email, filename="goldeneye_private.gyp", path=tempdir, append_Rprofile=FALSE, silent=TRUE)
      })
      if(inherits(ss, "try-error")){
        rv$setup_feedback <- "ERROR: an unknown error occured"
      }else{
        rv$setup_feedback <- "Profile created successfully"
        rv$status <- 3L
        #print(rv$status)
        rv$user_info <- gy_profile(file.path(tempdir, "goldeneye_private.gyp"))
      }
    }
  })
  output$setup_feedback <- renderText(str_c("<br>", rv$setup_feedback))

  output$save_profile <- downloadHandler(
    filename = function() "goldeneye_private.gyp",
    content = function(file) {
      success <- file.copy(file.path(tempdir, "goldeneye_private.gyp"), file)
      if(!success) stop("Error copying private key file")
      rv$status <- 4L
    }
  )

  output$download_public <- downloadHandler(
    filename = function() "goldeneye_public.gyu",
    content = function(file) {
      gy_public_file(file.path(tempdir, "goldeneye_public.gyu"))
      success <- file.copy(file.path(tempdir, "goldeneye_public.gyu"), file)
      if(!success) stop("Error copying public key file")
    }
  )

  observe({
    input$encrypt_files

    if(is.null(input$encrypt_files)){
      rv$files_ready <- -1
    }else{
      rv$files_ready <- nrow(input$encrypt_files)

      ## Hack to re-print the rv stuff:
      if(length(rv$user_list) != rv$user_length){
        add_user("","")
        rv$user_length <- length(rv$user_list)
      }
    }
  })

  observeEvent(input$process_online, {

    tmpfl <- tempfile(pattern="user", tmpdir=tempdir)
    ss <- try({
      download.file(gsub(" ", "", input$user_online), tmpfl, quiet=TRUE, mode="wb")
    })
    if(inherits(ss,"try-error")){
      rv$process_feedback <- "ERROR:  problem downloading from the specified link"
    }else{
      ss <- try({
        info <- unserialize(readRDS(tmpfl))
      })
      if(inherits(ss,"try-error") || !all(c("email","name","public_curve")%in%names(info))){
        rv$process_feedback <- "ERROR:  problem reading the specified key file"
      }else{
        newkey <- list(info$public_curve)
        names(newkey) <- info$email
        rv$user_keys <- c(rv$user_keys, newkey)
        updateTextInput(session, "user_online", value="")

        add_user(info$name, info$email)
      }
    }
    unlink(tmpfl)
  })

  observeEvent(input$process_local, {

    ss <- try({
      info <- unserialize(readRDS(input$user_local$datapath))
    })
    if(inherits(ss,"try-error") || !all(c("email","name","public_curve")%in%names(info))){
      rv$process_feedback <- "ERROR:  problem reading the specified key file"
    }else{
      newkey <- list(info$public_curve)
      names(newkey) <- info$email
      rv$user_keys <- c(rv$user_keys, newkey)
      add_user(info$name, info$email)
      updateTextInput(session, "user_online", value="")
    }

  })

  output$encrypt_download <- downloadHandler(
    filename = function() "encrypted_files.rfg",
    content = function(file) {
      cwd <- getwd()
      on.exit(setwd(cwd))
      setwd(tempdir)

      ## First create files with correct names:
      file.copy(input$encrypt_files$datapath, input$encrypt_files$name, overwrite = TRUE)
      gy_zip(input$encrypt_files$name, file="encrypted_files.rfg", users=rv$user_keys, local_user=input$user_self)
      unlink(input$encrypt_files$name)
      success <- file.copy("encrypted_files.rfg", file)
      if(!success) stop("Error copying private key file")
    }
  )

  observe({
    input$decrypt_file

    if(is.null(input$decrypt_file)){
      rv$decrypt_ready <- 0
    }else{
      ss <- try({
        prf <- readRDS(input$decrypt_file$datapath)
      })
      if(inherits(ss,"try-error") || !inherits(prf, "goldeneye") || prf$metadata$gy_type != "gy_zip"){
        rv$decrypt_feedback <- "ERROR:  specified file is not valid"
      }else{

        ## Then extract the files:
        cwd <- getwd()
        on.exit(setwd(cwd))
        setwd(tempdir)
        ss <- try({
          ff <- gy_unzip(input$decrypt_file$datapath, unzip=NULL, overwrite=TRUE, run_custom = TRUE)
        })
        if(inherits(ss,"try-error")){
          rv$decrypt_feedback <- "ERROR:  unable to decrypt specified file"
          validate("ERROR:  unable to decrypt specified file")
        }
        if(is.data.frame(ff)) ff <- ff$File

        rv$decrypt_filename <- ff
        rv$decrypt_ready <- 1
        rv$decrypt_feedback <- "File(s) decrypted and ready to save"
      }
    }
  })

  output$decrypt_download <- downloadHandler(
    filename = function() rv$decrypt_filename,
    content = function(file) {
      stopifnot(file.exists(file.path(tempdir, rv$decrypt_filename)))
      success <- file.copy(file.path(tempdir, rv$decrypt_filename), file)
      if(!success) stop("Error copying decrypted file")
    }
  )

  output$process_feedback <- renderText(rv$process_feedback)
  output$user_feedback <- renderText(rv$user_feedback)
  output$decrypt_feedback <- renderText(rv$decrypt_feedback)
  output$decrypt_ready <- renderText(rv$decrypt_ready)

  fluidPage({
    output$files_ready <- renderText(rv$files_ready)
    output$status <- renderText(rv$status)
  })
  outputOptions(output, 'status', suspendWhenHidden=FALSE)
  outputOptions(output, 'files_ready', suspendWhenHidden=FALSE)
  outputOptions(output, 'decrypt_ready', suspendWhenHidden=FALSE)

}

