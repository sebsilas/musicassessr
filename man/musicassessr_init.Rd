% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/musicassessr_init.R
\name{musicassessr_init}
\alias{musicassessr_init}
\title{Initiate a musicassessr test}
\usage{
musicassessr_init(
  app_name = "",
  experiment_id = NULL,
  experiment_condition_id = NULL,
  user_id = NULL,
  asynchronous_api_mode = FALSE,
  instrument_id = NULL,
  inst = NULL,
  default_range = set_default_range("Piano"),
  username = NULL,
  get_user_info = TRUE,
  redirect_on_failure_url = "https://www.google.com/",
  async_success_msg = if (is.scalar.character(username))
    paste0(psychTestR::i18n("Hello"), " ", username, "!") else
    psychTestR::i18n("lets_proceed"),
  use_presigned_url = TRUE
)
}
\arguments{
\item{use_presigned_url}{}
}
\description{
Initiate a musicassessr test
}
