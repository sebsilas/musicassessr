

#asdw <- itembankr::WJD("ngram") %>% dplyr::slice(100) %>% dplyr::pull(melody)

present_stimuli_audio_WJD <- function(pattern, ...) {
  # pattern should be a relative pattern
  search_res <- DTL_similarity_search_results(search_patterns = pattern)
  random_sample <- sample(1:nrow(search_res), 1)
  res <- search_res %>% dplyr::slice(random_sample)
  audio_url <- get_wjd_mp3_url(res)
}


# p <- itembankr::WJD("phrases")
#
# d2 <- itembankr::WJD("phrases")[998, ]
#
# da2 <- DTL_similarity_search_results(search_patterns = d2$orig_abs_melody, database_names = "wjazzd", transformation = "pitch")
#
# d3 <- itembankr::WJD("phrases")[998, ]
# da3 <- DTL_similarity_search_results(search_patterns = "61,62", transformation = "pitch")

# da4 <- da3 %>% dplyr::filter(database_name == "omnibook")
# dd <- da4[1, ]
# dd2 <- get_wjd_mp3_url(dd)
#
# da5 <- da3 %>% dplyr::filter(database_name == "wjazzd")
# da6 <- da5[1, ]
# dd3 <- get_wjd_mp3_url(da6)


#present_stimuli_audio_WJD(t)

