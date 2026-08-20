
# IMPORTANT!
# all file paths are for my computer and need to be updated if run somewhere else!

# test shiny app (folder with app.R)
#runApp("../Goldeneye")


# export with shinylive
shinylive::export(
  appdir = "../Goldeneye",
  destdir = "site"
)

# testrun
httpuv::runStaticServer("site")

# test decrypting the testrun
# readRDS("../Goldeneye/datathon.txt.rds") |>
#   data_decrypt(key = hash(charToRaw("Hallo1Mein!Schatz"))) |>
#   writeBin(con = "../Goldeneye/test.txt")
