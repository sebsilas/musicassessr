
source('play_words.R')

# constants

stimuli.carousel <- function(slide_length = 200) {

  # slide_length in ms

  tags$script(HTML(paste0('var slideIndex = 0;
  carousel(length);

  function carousel(slideLength) {
    var i;
    var x = document.getElementsByClassName("slides");
    for (i = 0; i < x.length; i++) {
      x[i].style.display = "none";
    }
    slideIndex++;
    x[slideIndex-1].style.display = "block";
    setTimeout(carousel,', slide_length, ');
  }')))

}




# functions

wrap.char.slides <- function(chars) {

  tags$p(chars, class = "slides")

}

wrap.img.slide <- function(image_url = 'https://picsum.photos/200') { # dummy random image
  tags$img(src = image_url, class = "slides")

}


create.dummy.image.slides <- function(no_of_images) {
  # for testing: create slides for N number of images
  # it won't work if you use the same url for images, the image url has to be unique
  # N.B: this method creates images of 1px different size each
  # i.e each image will be 1px bigger than the last
  res <- lapply(200:(200+no_of_images), function(x) wrap.img.slide(paste0('https://picsum.photos/', x)) )
  tagList(res)
}


create.image.slides <- function(image_list) {
  # for testing: create slides for N number of images
  # it won't work if you use the same url for images, the image url has to be unique
  # N.B: this method creates images of 1px different size each
  # i.e each image will be 1px bigger than the last
  res <- lapply(image_list, wrap.img.slide)
  tagList(res)
}


prepare.char.stimuli <- function(stimuli_vector) {
  res <- lapply(stimuli_vector, wrap.char.slides)
  tagList(res)
}


present_stimuli_characters_visual <- function(stimuli, slide_length = 500) {
    div(
      tags$style('.slides {display:none; font-size: 30px;}'),
      prepare.char.stimuli(stimuli),
      stimuli.carousel(slide_length = slide_length)
    )
}


present_stimuli_characters_auditory <- function(stimuli, page_type = "one_button_page", rate = 1, page_title) {
  # return a list, the play_text_page type has  has to be built differently to the other stimuli
  # because it is a custom made page which uses a reactive page (same with record_midi_page)
  list("present_stimuli_characters_auditory" = TRUE,
        "stimuli" = stimuli,
       "underlying_page_type" = page_type,
       "play_text_page_title" = page_title,
       rate = 1)
}

present_stimuli_characters <- function(stimuli, display_modality, page_type = "one_button_page",
                                       slide_length = 500, rate = 1, page_title) {

  if(display_modality == "visual") {
    return_stimuli <- present_stimuli_characters_visual(stimuli, slide_length)
  }
  else {
    return_stimuli <- present_stimuli_characters_auditory(stimuli, page_type, rate, page_title)
  }

}


present_stimuli_images <- function(stimuli, slide_length = 500) {
    div(
      tags$style('.slides {display:none;}'),
      create.image.slides(stimuli),
      stimuli.carousel(slide_length = slide_length)
    )
}
