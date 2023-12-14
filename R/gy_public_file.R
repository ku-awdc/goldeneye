#' Extract the public key file
#'
#' @param file filename to write to
#' @param silent do this quietly?
#'
#' @export
gy_public_file <- function(file="goldeneye_public.gyu", silent=FALSE){

  local <- gy_check()

  local$versions <- get_versions(type="generic")
  public_save <- local[c("name","email","versions","public_curve","public_ed")]
  public_save$user_since <- local$setup_date
  public_encry <- serialize(public_save, NULL)
  class(public_encry) <- "goldeneye_public"

  pfilen <- file
  saveRDS(public_encry, file=pfilen, compress=FALSE)

  if(!isTRUE(getOption("goldeneye_redact"))){
    pfilen <- file.path(getwd(), pfilen)
  }
  if(!silent) cat("The following file contains your public keys:\n    '", pfilen, "'\nNOTE: this file is safe to place online or to send over email to your collaborators\n", sep="")

  invisible(pfilen)

}
