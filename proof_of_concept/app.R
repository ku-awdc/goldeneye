library(shiny)
library(sodium)

# according to shiny documentation this is needed for chromium-based browsers to be able to correctly download the file
downloadLink <- function(...) {
  tag <- shiny::downloadLink(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  h3("File encryption with sodium"),

  fileInput(
    "file",
    "Choose file"
  ),

  passwordInput(
    "password",
    "Encryption password"
  ),

  actionButton(
    "encrypt",
    "Encrypt"
  ),

  br(),
  br(),

  downloadButton(
    "download",
    "Download encrypted file"
  )
)

server <- function(input, output, session) {
  
  encrypted_file <- reactive({

    req(input$file)
    req(input$password)

    # Read uploaded file as raw bytes
    raw_data <- readBin(
      input$file$datapath,
      what = "raw",
      n = file.info(input$file$datapath)$size
    )

    # encrypt
    encrypted <- data_encrypt(
      raw_data,
      key = hash(charToRaw(input$password))
    )
    
    # save as temporary file
    out <- tempfile(fileext = ".rds")
    encrypted |>
      saveRDS(file = out)
    
    if(file.exists(out)) {
      out
    } else {
      stop("Something went wrong!")
    }

  }) |>
    bindEvent(input$encrypt)

  output$download <- downloadHandler(
    filename = function() {
      paste0(
        basename(input$file$name),
        ".rds"
      )
    },

    content = function(file) {
      file.copy(
        from = encrypted_file(),
        to = file
      )
    }
  )
}

shinyApp(ui, server)
