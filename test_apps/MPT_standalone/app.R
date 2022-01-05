#mpt::standalone_mpt()


##

psychTestR::make_test(
  psychTestR::join(
    mpt::mpt(),
    psychTestR::final_page("The end")
    ),
  opt = psychTestR::test_options(
    title = "MPT",
    admin_password = "demo",
    additional_scripts = musicassessr_js('test')
  )
)




# shiny::runApp("test_apps/MPT_standalone/app.R")
# shinyloadtest::record_session("http://127.0.0.1:7999")
