
messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

pattern_to_vec <- function(pattern, as_int = F, keep_list = FALSE) {
  ret <- strsplit(pattern, ",")
  if(length(pattern) == 1 && !keep_list)
    ret <- ret[[1]]
  if(as_int){
    ret <- lapply(ret, as.integer)
  }
  ret
}


DTL_similarity_search <- function(search_pattern = "1,2,1,2,1,2,1,2",
                                  transformation = "interval",
                                  database_names = "dtl,wjazzd,omnibook",
                                  metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                  filter_category = "0",
                                  minimum_similarity = 1.0,
                                  max_edit_distance = NA,
                                  max_length_difference = 0) {
  url <- suppressWarnings(httr::modify_url("https://staging-dtl-pattern-api.hfm-weimar.de/", path = "/patterns/similar"))
  if(is.na(max_edit_distance)){
    max_edit_distance <- purrr::map_int(pattern_to_vec(search_pattern, keep_list = T), length) %>% min()
  }
  messagef("[DTL API] Starting search for %s", search_pattern)
  resp <- suppressWarnings(httr::POST(url, body = list( n_gram = search_pattern,
                                                        transformation = transformation,
                                                        database_names = database_names,
                                                        metadata_filters = metadata_filters,
                                                        filter_category = filter_category,
                                                        minimum_similarity = minimum_similarity,
                                                        max_edit_distance = max_edit_distance,
                                                        max_length_difference = max_length_difference, filter_category = 0),
                                      encode = "form"))
  #browser()
  #print(httr::content(resp, "text"))
  if (httr::http_error(resp)) {
    messagef(
      "[DTL API]  Similarity Search  request failed [%s]\n%s\n<%s>",
      httr::status_code(resp),
      "",#parsed$message,
      ""#parsed$documentation_url
    )
    return(NULL)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  messagef("[DTL API] Retrieved search ID %s of for pattern %s", parsed$search_id, search_pattern)
  parsed$search_id
}

DTL_get_results <- function(search_id) {
  print(search_id)
  url <- suppressWarnings(httr::modify_url("http://staging-dtl-pattern-api.hfm-weimar.de/", path = "/patterns/get"))
  #messagef("[DTL API] Retrieving results for search_id %s",  search_id)
  resp <- suppressWarnings(httr::GET(url, query = list(search_id  = search_id)))
  if (httr::http_error(resp)) {
    messagef(
      "[DTL API]  Similarity Search  request failed [%s]\n%s\n<%s>",
      httr::status_code(resp),
      "",#parsed$message,
      ""#parsed$documentation_url
    )
    return(NULL)
  }
  print(httr::content(resp, "text"))
  #browser()

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  messagef("[DTL API] Retrieved %s lines for search_id %s", length(parsed), search_id)
  purrr::map_dfr(parsed, function(x){
    if(is.null(x$within_single_phrase)){
      x$within_single_phrase <- FALSE
    }
    #browser()
    tibble::as_tibble(x) %>% dplyr::mutate(melid = as.character(melid))
  })
}

DTL_similarity_search_results <- function(search_patterns = "1,2,1,2,1,2,1,2",
                                          transformation = "interval",
                                          database_names = "dtl,wjazzd,omnibook",
                                          metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                          filter_category = "0",
                                          minimum_similarity = 1.0,
                                          max_edit_distance = NA,
                                          max_length_difference = 0) {
  results <- tibble::tibble()
  if(is.na(max_edit_distance)){
    max_edit_distance <- purrr:::map_int(pattern_to_vec(search_patterns, keep_list = T), length) %>% min()
  }
  for(pattern in search_patterns){

    search_id <- DTL_similarity_search(pattern,
                                       transformation,
                                       database_names,
                                       metadata_filters,
                                       filter_category,
                                       minimum_similarity,
                                       max_edit_distance = max_edit_distance,
                                       max_length_difference = max_length_difference)
    if(is.null(search_id)){
      print('is.null search id')
      next
    }

    ret <- DTL_get_results(search_id)

    if(!is.null(ret) && nrow(ret) > 0){
      ret$search_pattern <- pattern
    }
    results <- dplyr::bind_rows(results, ret)


  }
  #browser()
  if(nrow(results))
    results %>% dplyr::distinct(melid, start, length, .keep_all = T)
}

DTL_similarity_search_results_fast <- function(search_patterns = "1,2,1,2,1,2,1,2",
                                               transformation = "interval",
                                               database_names = "dtl,wjazzd,omnibook",
                                               metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                               filter_category = "0",
                                               minimum_similarity = 1.0,
                                               max_edit_distance = NA,
                                               max_length_difference = 0) {

  if(is.na(max_edit_distance)){
    max_edit_distance <- purrr::map_int(pattern_to_vec(search_patterns, keep_list = TRUE), length) %>% min()
  }
  future::plan(future::multisession)

  results <- furrr:::future_map_dfr(search_patterns, function(pattern){


    search_id <- DTL_similarity_search(pattern,
                                       transformation,
                                       database_names,
                                       metadata_filters,
                                       filter_category,
                                       minimum_similarity,
                                       max_edit_distance = max_edit_distance,
                                       max_length_difference = max_length_difference)
    if(is.null(search_id)){
      return(tibble::tibble())
    }
    ret <- DTL_get_results(search_id)
    if(!is.null(ret) && nrow(ret) > 0 )ret$search_pattern <- pattern
    ret
  })

  #browser()
  results %>% dplyr::distinct(melid, start, length, .keep_all = TRUE)
}


# res <- DTL_similarity_search_results_fast()
# res <- DTL_similarity_search_results()
# res <- DTL_similarity_search()
#

get_wjd_mp3_url <- function(res) {

  mp3_url <- paste0('https://staging-dtl-pattern-api.hfm-weimar.de/static/audio/n_grams/',
                    res$database_name,'/',
                    res$melid, '/',
                    res$melid, '_',
                    res$start, '_',
                    res$length, '.mp3')

}

# search_res <- DTL_similarity_search_results()
#
# search_res$url <- search_res %>% dplyr::rowwise() %>% get_wjd_mp3_url()
#
# audio_url <- get_wjd_mp3_url(search_res[1, ])
#
# random_sample <- sample(1:nrow(search_res), 1)
# res <- search_res %>% dplyr::slice(random_sample)



