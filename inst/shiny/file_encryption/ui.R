fluidPage(
	theme = shinytheme("readable"),

	tags$head(includeHTML("head.html")),
	#	tags$header(hr()),

	navbarPage("goldeneye",

		## INSTRUCTIONS
		tabPanel("Instructions",
			includeMarkdown("instructions.md")
		),
		## /INSTRUCTIONS


		## PROFILE
		tabPanel("Profile",

			## Profile selection row:
			fluidRow(
				column(12L,
					radioButtons("profile",
						"Select goldeneye profile to use:",
						choices = profile_choices,
						selected = profile_selected,
						inline = FALSE
					)
				)
			),

		  ## Profile file upload (optional):
		  conditionalPanel(
		    "input.profile == 'select'",
		    fluidRow(
		      column(12L,
		        fileInput("profile_file", "Choose profile file:",
		          multiple = FALSE,
		          accept = NULL #c(".gyp")
		        ),
		        htmlOutput("profile_file_feedback")
		      )
		    ),
		  ),

		  ## Password (not for new profile):
		  conditionalPanel(
		    "output.status == 1",
		    fluidRow(
		      column(6L,
		        conditionalPanel(
		          "input.show_password == false",
		          passwordInput("password_hide",
		            "Input password:",
		            value = default_password
		          )
		        ),
		        conditionalPanel(
		          "input.show_password == true",
		          textInput("password_show",
		            "Input password:",
		            value = default_password
		          )
		        ),
		        checkboxInput("show_password",
		          "Show password?",
		          value = FALSE
		        )
		      ),
		      column(6L,
		        actionButton("check_password",
		          "Check Password"
		        ),
		        htmlOutput("password_feedback", width="100%")
		      )
		    )
		  ),

		  ## New profile:
		  conditionalPanel(
		    "input.profile == 'new' && output.status == 2",
		    fluidRow(
		      column(6L,
		        textInput("setup_name",
		          "Enter your full name:"
		        ),
		        textInput("setup_email",
		          "Enter your email address:"
		        )
		      ),
		      column(6L,
		        conditionalPanel(
		          "input.show_new_password == false",
		          passwordInput("password_new_hide",
		            "Type password:",
		            value = ""
		          )
		        ),
		        conditionalPanel(
		          "input.show_new_password == true",
		          textInput("password_new_show",
		            "Type password:",
		            value = ""
		          )
		        ),
		        conditionalPanel(
		          "input.show_new_password == false",
		          passwordInput("password_verify_hide",
		            "Re-type password:",
		            value = ""
		          )
		        ),
		        conditionalPanel(
		          "input.show_new_password == true",
		          textInput("password_verify_show",
		            "Re-type password:",
		            value = ""
		          )
		        ),
		        checkboxInput("show_new_password",
		          "Show password?",
		          value = FALSE
		        )
		      )
		    ),
		    fluidRow(
		      column(12L,
	          actionButton("create_profile",
	            "Create Profile"
		        )
		      )
		    )
		  ),

		  ## Profile feedback:
		  conditionalPanel(
		    "input.profile == 'new'",
		    fluidRow(
		      column(12L,
		        htmlOutput("setup_feedback", width="100%")
		      )
		    )
		  ),

		  ## Download profile:
		  conditionalPanel(
		    "input.profile == 'new' && output.status == 3",
		    fluidRow(
		      column(12L,
		        downloadButton("save_profile",
		          "Download Private Key File"
		        )
		      )
		    )
		  ),

		  ## Final validation text and download public file:
		  conditionalPanel("output.status == 4",
  		  fluidRow(
  		    column(12L,
  		      h5("Profile and password are valid", width="100%")
  		    )
  		  ),
		    fluidRow(
		      column(12L,
		        downloadButton("download_public",
		          "Download Public Key File"
		        )
		      )
		    )
		  )
		),
		## /PROFILE


		## ENCRYPT
		tabPanel("Encrypt File(s)",

		  conditionalPanel(
		    "output.status < 4",
		    h5("Please create or upload a valid profile file and verify your password")
		  ),

		  conditionalPanel(
		    "output.status == 4",

		    fluidRow(
		      column(12L,
		        fileInput("encrypt_files",
		          "Select the file(s) to encrypt",
		          multiple=TRUE
		        )
		      )
		    ),

		    conditionalPanel("output.files_ready > 0",

		      fluidRow(
		        column(6L,
		          textInput("user_online",
		            "Enter weblink for public key"
		          ),
		          actionButton("process_online",
		            "Add Weblink"
		          )
		        ),
		        column(6L,
		          fileInput("user_local",
		            "Upload public key file"
		          ),
		          actionButton("process_local",
		            "Add File"
		          )
		        )
		      ),

		      fluidRow(
		        column(12L,
		          checkboxInput("user_self",
		            "Allow self decryption?",
		            value = TRUE
		          ),

		          hr(),
		          htmlOutput("process_feedback", width="100%"),
		          htmlOutput("user_feedback", width="100%"),
		          hr(),

		          downloadButton("encrypt_download",
		            "Encrypt and Save"
		          )
		        )
		      )
		    )
		  )
		),
		## /ENCRYPT


		## DECRYPT
		tabPanel("Decrypt File(s)",

		  conditionalPanel(
		    "output.status < 4",
		    h5("Please create or upload a valid profile file and verify your password")
		  ),

		  conditionalPanel(
		    "output.status == 4",

		    fileInput("decrypt_file",
		      "Upload file to decrypt",
		      multiple=FALSE
		    ),

		    conditionalPanel(
		      "output.decrypt_ready == 1",
		      downloadButton("decrypt_download",
		        "Save File"
		      )
		    ),

		    htmlOutput("decrypt_feedback")
			)
		)
	),

# 	tags$footer(HTML("
#                     <!-- Footer -->
#                            <footer class='page-footer font-large indigo'>
#                            <!-- Copyright -->
#                            <div class='footer-copyright text-center py-3'>© 2018 Copyright:
#                            <a href='https://mdbootstrap.com/education/bootstrap/'> MDBootstrap.com</a>
#                            </div>
#                            <!-- Copyright -->
#
#                            </footer>
#                            <!-- Footer -->")
# 		)

	tags$footer(hr())
)
