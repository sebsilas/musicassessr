library(MST)
testApp <- function() {
  MST::MST(aws_credentials = list("api_url" = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                             "bucket_name" = "shinny-app-source-41630",
                             "bucket_region" = "us-east-1",
                             "identity_pool_id" = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                             "destination_bucket" = "shinny-app-destination-41630"),
      num_items = list("long_tones" = 3L,
                       "arrhythmic" = 5L,
                       "rhythmic" = 5L),
      demo = FALSE)
}

testApp()

