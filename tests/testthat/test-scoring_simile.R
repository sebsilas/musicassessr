test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



# Why is there a 0 for the first value of ioi?...

# melody_1 <- read_melody('/Users/sebsilas/Downloads/K_S/HBD_test/seb.csv', style = "tony")
# melody_2 <- read_melody('/Users/sebsilas/Downloads/K_S/HBD_test/sylvia.csv', style = "tony")


# ...once figured that out, implement tests for...

# da <- opti3_df(melody_1, melody_2)
# da1 <- opti3_df(melody_1, melody_2, only_winner = FALSE)
#
# da2 <- opti3(melody_1$note, melody_1$onset, melody_2$note, melody_2$onset)
#
# da3 <- opti3(melody_1$note, melody_1$onset,
#              melody_2$note, melody_2$onset, return_components = TRUE)
#
# da4 <- opti3(melody_1$note, melody_1$onset,
#              melody_2$note, melody_2$onset,
#              segmentation1 = melody_1$phrasbeg, segmentation2 = melody_2$phrasbeg,
#              return_components = TRUE)
#
# da5 <- opti3(melody_1$note, melody_1$onset,
#              melody_2$note, melody_2$onset,
#              segmentation1 = melody_1$phrasbeg, segmentation2 = melody_2$phrasbeg,
#              return_components = TRUE, use_bootstrap = TRUE)
#
# harmcore(melody_1$note, melody_2$note,
#          segmentation1 = melody_1$phrasbeg,
#          segmentation2 = melody_2$phrasbeg)
#
# harmcore2(melody_1$note,
#           melody_2$note,
#           segmentation1 = melody_1$phrasbeg,
#           segmentation2 = melody_2$phrasbeg)



# opti3(50:60, 1:10, 50:60, 1:10, use_bootstrap = F)
