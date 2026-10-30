# Test shiny app:
shiny::runApp("proof_of_concept")

# Export with shinylive:
shinylive::export(
  appdir = "proof_of_concept",
  destdir = "proof_of_concept/site",
  subdir = "app1"
)

# Another app:
shinylive::export(
  appdir = "proof_of_concept",
  destdir = "proof_of_concept/site",
  subdir = "app2"
)

# Test:
httpuv::runStaticServer("proof_of_concept/site/")
# Then go to e.g. http://127.0.0.1:7446/app1/

