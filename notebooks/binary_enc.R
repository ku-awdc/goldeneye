files <- c("DESCRIPTION", "NAMESPACE")

files |>
  set_names() |>
  lapply(function(ff) readBin(ff, "raw", file.size(ff)*1.1)) ->
  dd

dd |> object.size()
serialize(dd, NULL) |> object.size()
qs::qserialize(dd) |> object.size()

library("archive")
writeBin(dd[[1]], archive_write("notebooks/archive.zip", files[1]))
writeBin(dd[[2]], archive_write("notebooks/archive.zip", files[2]))
## Note: this only does 1 file at a time; we need archive_write_files

archive("notebooks/archive.zip")

?archive::archive_write

