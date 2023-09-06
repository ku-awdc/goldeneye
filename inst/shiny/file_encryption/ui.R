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
		        )
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
		    "input.profile == 'new'",
		    fluidRow(
		      column(6L,
		        conditionalPanel(
		          "input.show_new_password == false",
		          passwordInput("password_new_hide",
		            "Select password:",
		            value = ""
		          )
		        ),
		        conditionalPanel(
		          "input.show_new_password == true",
		          textInput("password_new_show",
		            "Select password:",
		            value = ""
		          )
		        ),
		        checkboxInput("show_new_password",
		          "Show password?",
		          value = FALSE
		        )
		      ),
		      column(6L,
		        conditionalPanel(
		          "input.show_new_password == false",
		          passwordInput("password_verify_hide",
		            "Verify password:",
		            value = ""
		          )
		        ),
		        conditionalPanel(
		          "input.show_new_password == true",
		          textInput("password_verify_show",
		            "Verify password:",
		            value = ""
		          )
		        )
		      )
		    ),
		    fluidRow(
		      column(12L,
		        actionButton("create_profile",
		          "Create Profile"
		        ),
		        htmlOutput("profile_feedback", width="100%")
		      )
		    )
		  ),

		  ## Final validation text and download public file:
		  conditionalPanel("output.status == 2",
  		  fluidRow(
  		    column(12L,
  		      h5("Profile and password are valid", width="100%")
  		    )
  		  ),
		    fluidRow(
		      column(12L,
		        actionButton("download_public",
		          "Download Public Key File"
		        )
		      )
		    )
		  )
		),
		## /PROFILE


		## PARAMETERS
		tabPanel("Parameters",

			## Row for pre-selection and host:
			fluidRow(
				column(colwidth,
					radioButtons("parameterType",
						"Select scenario:",
						#choices = c(`Guidelines: clinical` = "clinical", `Guidelines: research` = "research", `Custom` = "custom"),
						choices = c(`Guidelines: clinical` = "clinical", `Guidelines: research` = "research"),
						selected = "waavp", #character(0),
						inline = FALSE
					)
				),

				conditionalPanel("input.parameterType == 'clinical' || input.parameterType == 'research'",
					column(colwidth,
						selectInput("waavpSetup", "Species and anthelmintic:",
							choices = waavp_choices,
							selected = NULL,
							selectize = FALSE
						)
					)
				),
				conditionalPanel("input.parameterType == 'custom'",
					column(colwidth,
						selectInput("hostSpecies", "Host species:",
							choices = host_choices,
							selected = NULL,
							selectize = FALSE
						)
					)
				)
			),

			## Optional rows for custom parameters:
			conditionalPanel("input.parameterType == 'custom'",
				fluidRow(
					## TODO


					## Custom settings for custom option:
					conditionalPanel("input.parameterType == 'custom'",

						## Select input for host (always displayed):
						selectInput("hostSpecies", "Host species:",
							choices = list(
								`*SELECT*` = "INVALID",
								Ruminants = list(`Cattle` = "cattle", `Sheep` = "sheep", `Goats` = "goats"),
								Equine = list(`Horses (adults)` = "horse_adult", `Horses (foals)` = "Horses (foals)", `Donkeys (adults)` = "donk_adult", `Donkeys (foals)` = "donk_foal"),
								`Swine` = "pigs",
								`Other` = "other"
							),
							selected = NULL,
							selectize = FALSE
						),
						## Text input for host (if hostSpecies->other)
						conditionalPanel(
							condition = "input.hostSpecies == 'other'",
							textInput("host_other", "Enter host species:")
						),

						## Select input for parasite (if ! hostSpecies->other)
						conditionalPanel("input.hostSpecies != 'other'",
							## Note: choices are reactive
							selectInput("parasiteSpecies", "Parasite species:",
								choices = NULL,
								selectize = FALSE
							)
						),
						## Text input for parasite (if hostSpecies->other or parasiteSpecies->other)
						conditionalPanel("input.hostSpecies == 'other' || input.parasiteSpecies == 'other'",
							textInput("parasite_other", "Enter parasite species:")
						),

						## Expected variabilities x3 - NOTE: reactive
						numericInput("k1", "", "", min=0, max=20, step=0.1),
						numericInput("k2", "", "", min=0, max=20, step=0.1),
						numericInput("kc", "", "", min=0, max=20, step=0.1),


						## Select input for anthelmintic - note: choices are reactive
						selectInput("anthelmintic", "Anthelmintic class:",
							choices = NULL,
							selectize = FALSE
						),
						## Text input for anthelmintic (if anthelmintic->other)
						conditionalPanel("input.anthelmintic == 'other'",
							textInput("anthelmintic_other", "Enter anthelmintic class:")
						),

						## Text input for parasite (if hostSpecies->other or parasiteSpecies->other)
						conditionalPanel("input.hostSpecies == 'other' || input.parasiteSpecies == 'other'",
							## Note: choices are reactive
							textInput("parasite_other", "Enter parasite species:")
						),


						## Target, Lower

						## Button input for pre-selection:
						## alpha level
						numericInput("alphaLevel",
							"Desired type 1 error:",
							value = 0.05,
							min = 0,
							max = 0.5,
							step = 0.005
						)

					)

				)
			),

			## Common rows for multiplication factor:
			conditionalPanel("input.parameterType == 'clinical' || input.parameterType == 'research' || input.parameterType == 'custom'",
				conditionalPanel("input.design == 'paired'",
					fluidRow(
						column(colwidth,
							div(strong("Multiplication factor (pre-treatment):")),
							numericInput("mf_pre",
								"",
								NULL,
								min=0, max=100, step=1
							)
						),
						column(colwidth,
							checkboxInput("mfp_fixed",
								"Post- same as pre-treatment",
								value=TRUE
							),
							conditionalPanel("input.mfp_fixed == false",
								numericInput("mf_post",
									"",
									NULL,
									min=0, max=100, step=1
								)
							)
						)
					)
				),

				conditionalPanel("input.design == 'unpaired'",
					fluidRow(
						column(colwidth,
							div(strong("Multiplication factor (controls):")),
							numericInput("mf_ctl",
								"",
								NULL,
								min=0, max=100, step=1
							)
						),
						column(colwidth,
							checkboxInput("mfu_fixed",
								"Treatment same as control",
								value=TRUE
							),
							conditionalPanel("input.mfu_fixed == false",
								numericInput("mf_txt",
									"",
									NULL,
									min=0, max=100, step=1
								)
							)
						)
					)
				),

				## Row for optional info:
				fluidRow(
					column(colwidth,
						textInput("region",
							"Country/region (optional):",
							value = ""
						),
					),
					column(colwidth,
						textInput("identifier",
							"Study identifier (optional):",
							value = ""
						)
					)
				)
			)
		),
		## /PARAMETERS


		## RESULTS
		tabPanel("Results",

			## First some feedback text to say what is missing:
			conditionalPanel("output.status < 3",
				fluidRow(
					column(colwidth*2,
						htmlOutput('status_feedback', width="100%")
					)
				)
			),

			## Then a button to calculate:
			conditionalPanel("output.status == 3",
				fluidRow(
					column(colwidth*2,
						actionButton("calculate",
							"Click to Calculate",
							icon = NULL
						)
					)
				)
			),

			## Then results and download tools:
			conditionalPanel("output.status == 4",
				fluidRow(
					column(colwidth*2,
						htmlOutput("result_summary", width="100%"),
						hr(),
						selectInput("downloadType",
							"Report download format:",
							choices = c(`PDF` = "pdf", `Microsoft Word` = "word"),
							selected="pdf"
						),
						downloadButton("downloadReport", "Download report")
					)
				)
			),

			## TODO: icon - https://fontawesome.com/icons or https://getbootstrap.com/docs/3.3/components/#glyphicons

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
