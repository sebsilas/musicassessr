psychTestR::make_test(
  psychTestR::join(
    psychTestR::one_button_page("Hi"),

    psychTestR::one_button_page(shiny::tags$div(
      musicassessr::set_musicassessr_state(state = "test"),
      shiny::tags$h1("UPEI 2021 Testing"),
      shiny::tags$p("This is a protocol for the UPEI 2021 singing study.")
    )),

    microphone_calibration_page(),

    get_instrument_range_pages("microphone", TRUE),

    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$p("Please use the latest version of Google Chrome to run these tests.
                      It can be downloaded", shiny::tags$a("here",
                                                           href = "https://www.google.com/intl/en_uk/chrome/", target = "_blank"), " and takes a couple of minutes to install."),
      shiny::tags$p("After downloading and installing, reopen the URL in Chrome and proceed there.")

    )),

    psychTestR::module("setup_questions",

                       psychTestR::NAFC_page(label = "chrome",
                                             choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                                             prompt = "Are you running this page in the latest version of Google Chrome?",
                                             on_complete = musicassessr::have_requirements),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),

                       psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("For best results please: "),
                                                                   shiny::tags$ul(
                                                                     shiny::tags$li("Close all tabs and windows other than this one."),
                                                                     shiny::tags$li("Quit other apps that are running, and pause any app or file downloads.")))),

                       psychTestR::one_button_page(shiny::tags$p(style = "text-align: left;", "This group of music tests has been recently developed, and the researchers have not been able to test is out on every computer.
          It is possible that the program will stop working on your computer.  If this happens you may see “Aw Snap” and a “Reload” button.  Press the “Reload” button, and in most cases, the program will start up where it left off. You may be asked to enter your number-letter code again.
          When it says 'Resuming ongoing testing session. Please click OK to confirm.' click OK, and the page should reload where you were.
          If however the “Reload” option is not available,  please e-mail ", shiny::tags$strong("silass@stud.hmtm-hannover.de"), "with a copy to ", shiny::tags$strong("airs@upei.ca"), " and state that Session 2 could  not be completed.  You will be contacted and provided the opportunity to do the test in the research lab space.
                                      Whether or not you complete the entire set of test, you will receive a credit point for going as far as the program would allow, provided you screenshot and document the error.")),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),


                       psychTestR::NAFC_page(label = "computer_type",
                                             prompt = "Which type of computer you are using?",
                                             choices = c("Laptop", "Desktop")),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),


                       psychTestR::NAFC_page(label = "computer_type2",
                                             prompt = "Which type of computer you are using?",
                                             choices = c("Mac",
                                                         "PC  (e.g., Dell, Hewlitt Packard, Lenova, Asus… any non-Mac computer).")),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),

                       psychTestR::text_input_page(
                         label = "computer_make_model",
                         prompt = "If you know the exact name, and model number of your computer please provide the information."),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),

                       psychTestR::NAFC_page(label = "headphone_type",
                                             prompt = "Please identify which kind of headphones you are using",
                                             choices = c("Over the ear", "Inserted in the ear")),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),

                       psychTestR::text_input_page(
                         label = "headphone_make_model",
                         prompt = "If you know the exact name, and model number of your headphones please provide the information."),

                       psychTestR::elt_save_results_to_disk(complete = FALSE),


                       psychTestR::get_p_id(prompt = shiny::tags$div(
                         shiny::tags$p("Please provide your participation identifier below created from:"),
                         shiny::tags$ul(
                           shiny::tags$li("1st 3 letters of the first name of your parent, guardian, or relative"),
                           shiny::tags$li("Day of your birthday (2 numbers – 01 to 31)"),
                           shiny::tags$li("1st 3 letters of the street you lived on growing up"),
                           shiny::tags$br(),
                           shiny::tags$p("For example: joh11tav"))))
    ), # end setup_questions module

    psychTestR::final_page("End")
  ),
  opt = psychTestR::test_options(
    title = "Hello World",
    admin_password = "demo",
    additional_scripts = musicassessr::musicassessr_js('test')
  )
)


# shiny::runApp("test_apps/hello_world/app.R")
# shinyloadtest::record_session("http://127.0.0.1:7999")
