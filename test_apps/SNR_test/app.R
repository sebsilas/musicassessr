library(psychTestR)
library(dplyr)

make_musicassessr_test(

  title = "Test SNR",

  admin_password = "blah",

  elts = function() {
    psychTestR::join(
      psychTestR::one_button_page("Test SNR")
    )
    },

  opt = musicassessr_opt(setup_options = setup_pages_options(SNR_test = TRUE))

)






