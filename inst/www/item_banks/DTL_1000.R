
library(tidyverse)

setwd("/Users/sebsilas/PhD 2021/magma-Gold/magmaGold/R/www/item_banks")

source('get_melody_features.R')

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


DTL_1000_raw <- read_csv2("DTL_lick_list_final.csv")

# value": The ngram
# "freq": total frequency of the n-gram,
# "n_solos": number of solos the n-grams occurs
# "n_performer": number of performer who used this n-gram
# "n_corpus": number of corpora the n-gram can be found (1 to 3)
# "N": length of the ngram

# For "group";"group_pos" and "group_seed" I am not sure.
# I think I used a seed (marked by "group_seed") to query the DTL1000 with a fixed similarity threshold (probably .8 or so)
# and retrieved all the ngrams and put the into a group with the (arbitrary) label "group",
# and some id in that group ("group_pos").

DTL_1000_raw <- select(DTL_1000_raw,
                   melody = value, freq)

DTL_1000 <- get_melody_features(DTL_1000_raw,
                                mel_sep = ",",
                                durationMeasures = FALSE)




ggplot(gather(DTL_1000[, names(DTL_1000)[!names(DTL_1000) %in% c("melody", "mode")]]), aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free_x')


# temporary, for now, change names to have dots to match old code. but eventually we want _ names
# also Freq wouldn't have a capital F

names(DTL_1000) <- c("melody", "Freq", "rel.freq", "log.freq", "N", "tonalness", "tonal.clarity",
                     "tonal.spike", "mode", "step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var",
                     "value", "mean_int_size", "int_range", "dir_change","mean_dir_change", "int_variety", "pitch_variety", "mean_run_length"
                     )



DTL_1000_contin <- DTL_1000[ , !names(DTL_1000) %in% c("melody", "value", "mode",
                                                       "Freq", "rel.freq", "log.freq", "step.cont.glob.dir", "N")]

p.mat <- cor.mtest(DTL_1000_contin)
M.dvs <- cor(DTL_1000_contin, use = "complete")
corrplot::corrplot(M.dvs, method = "number", type = "lower", p.mat = p.mat, sig.level = 0.05)


fa.parallel(DTL_1000_contin)

pc <- pca(DTL_1000_contin, nfactors = 3)

pc

write_rds(DTL_1000, file = "DTL_1000.RDS")

