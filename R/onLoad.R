.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "musicassessr-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "musicassessr") # path to resource in your package
  )
  shiny::addResourcePath(
    prefix = "item_banks", # custom prefix that will be used to reference your directory
    directoryPath = system.file("extdata", package = 'Berkowitz')
  )
}
