library("base64enc")
library("zip")

files <- c("notebooks/cars.rds", "notebooks/base64enc.R")
zip("notebooks/archive.zip", files, mode="mirror")
enc <- base64encode("notebooks/archive.zip")
saveRDS(enc, "notebooks/enc.rds")

unenc <- readRDS("notebooks/enc.rds")
writeBin(base64decode(unenc), "notebooks/unarchive.zip")
unzip("notebooks/unarchive.zip", exdir="notebooks")
