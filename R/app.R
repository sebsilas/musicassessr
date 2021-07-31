
t <- function() {
  make_aws_credentials_global(list(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                                   bucket_name = "shinny-app-source-41630",
                                   bucket_region = "us-east-1",
                                   identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                                   destination_bucket = "shinny-app-destination-41630"))

  psychTestR::make_test(psychTestR::new_timeline(psychTestR::join(

    psychTestR::one_button_page(shiny::tags$div(
                            musicassessr_js_scripts(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                            bucket_name = "shinny-app-source-41630",
                            bucket_region = "us-east-1",
                            identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                            destination_bucket = "shinny-app-destination-41630"))),
    microphone_calibration_page(),
    musicassessr::fake_range(),

    microphone_type_page(),

    play_long_tone_record_audio_page(60),

    play_long_tone_record_audio_page(60),

    rhythmic_melody_trials(itembankr::Berkowitz, 2L, 2L),


    arrhythmic_melody_trials(itembankr::Berkowitz, 2L, 2L),

    #musicassessr::long_tone_trials(num_items = 2L, num_examples = 2L, feedback = FALSE),


    #get_SNR_pages(),

    #
    #
    #
    # play_long_tone_record_audio_page(note = 60, page_type = "record_audio_page"),
    #
    #
    #
    # play_melody_until_satisfied_loop(melody = "60,61,62,63",
    #                                  var_name = "melody",
    #                                  max_goes = 3,
    #                                  page_type = "record_audio_page",
    #                                  page_text = "Sing it",
    #                                  get_answer = musicassessr::get_answer_null,
    #                                  play_button_text = "Play"),

    # musicassessr::fake_range(),
    #
    # psychTestR::code_block(function(state, ...) {
    #
    #   span <- psychTestR::get_global("span", state)
    #
    #   # sample arrhythmic
    #   arrythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank("main"), span_max = span)
    #   arrhythmic_sample <- musicassessr::item_sampler(arrythmic_item_bank_subset, num_items$arrhythmic)
    #
    #   psychTestR::set_global("arrhythmic_melody", arrhythmic_sample, state)
    #
    #   # sample rhythmic
    #   rhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank("phrases"), span_max = span)
    #   rhythmic_sample <- musicassessr::item_sampler(rhythmic_item_bank_subset, num_items$rhythmic)
    #   psychTestR::set_global("rhythmic_melody", rhythmic_sample, state)
    #
    # }),

    # instructions
    #MST_instructions(),
    #
    # example protocol
    #MST_example_protocol(),


    # arrhythmic
    # musicassessr::build_multi_page_play_melody_until_satisfied_loop(
    #   n_items = num_items$arrhythmic,
    #   var_name = "arrhythmic_melody",
    #   page_type = "record_audio_page",
    #   max_goes = 3,
    #   page_text = psychTestR::i18n("sing_melody_trial"),
    #   get_answer = musicassessr::get_answer_store_async,
    #   rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
    #   arrhythmic = TRUE
    # ),

    psychTestR::final_page("The end")
    ), dict = psychTestR::i18n_dict$new(musicassessr_dict_df)), opt = psychTestR::test_options(title = "test", admin_password = "demo", demo = TRUE))
}

#t()

