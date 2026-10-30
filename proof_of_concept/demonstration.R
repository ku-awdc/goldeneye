# Test shiny app:
shiny::runApp("proof_of_concept")


# Export with shinylive:
shinylive::export(
  appdir = "proof_of_concept",
  destdir = "proof_of_concept/site"
)

# Test:
httpuv::runStaticServer("proof_of_concept/site")
