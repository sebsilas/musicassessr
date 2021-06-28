library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(psych)

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




get.phrase <- function(row) {
  # get phrases based on segmentation information
  melodies[as.integer(row['start']):as.integer(row['end'])+1, "pitch"]
}

con <- dbConnect(RSQLite::SQLite(), "wjazzd.db")

# Show List of Tables
as.data.frame(dbListTables(con))

# get melody table
melodies <- dbReadTable(con, 'melody')

# get section information
sections <-dbReadTable(con, 'sections')
phrases <- sections[sections$type=="PHRASE", ]

# list of the melodic phrases
melodic.phrases <- apply(phrases, MARGIN = 1, get.phrase)

relative.phrases <- lapply(melodic.phrases, diff)

no_phrases <- length(relative.phrases)
# 11,082 phrases


# create string versions of the phrases and collapse into factors to see how many unique phrases there are
relative.phrases.str <- lapply(relative.phrases, paste0, collapse = ",")
relative.phrases.factor <- as.factor(unlist(relative.phrases.str))
levels(relative.phrases.factor)
no_unique <- length(levels(relative.phrases.factor))
# 8,111 unique phrases

vc <- as.data.frame(base::table(as.vector(unlist(relative.phrases.str))))
names(vc) <- c("melody", "count")
# remove the empty mel
vc <- vc[-which(vc$mel == ""), ]

vc <- arrange(vc, desc(count))

hist(vc$count)

sum_freq <- sum(vc$count)

vc$melody <- as.character(vc$melody)
names(vc) <- c("melody", "freq")

vc$N <- lapply(vc$melody, function(x) length(str.mel.to.vector(x, sep = ","))+1 )

# remove the random repeated note melodies
vc <- vc[!vc$melody=="0", ]
vc <- vc[!vc$melody=="0,0", ]
vc <- vc[!vc$melody=="0,0,0", ]
vc <- vc[!vc$melody=="-6,6", ] # hm maybe this should go back in?


WJD_corpus <- get_melody_features(vc, mel_sep = ",", durationMeasures = FALSE)



# temporary, for now, change names to have dots to match old code. but eventually we want _ names
# also Freq wouldn't have a capital F

names(WJD_corpus) <- c("melody", "Freq", "N", "rel.freq", "log.freq", "tonalness", "tonal.clarity",
                     "tonal.spike", "mode", "ks_tonality", "step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var",
                     "value", "mean_int_size", "int_range", "dir_change","mean_dir_change",
                     "int_variety", "pitch_variety", "mean_run_length", "span")



WJD_corpus <- WJD_corpus[ , !names(WJD_corpus) %in% c("value")]

WJD_corpus_contin <- WJD_corpus[ , !names(WJD_corpus) %in% c("melody", "value", "mode", "ks_tonality",
                                                             "Freq", "rel.freq", "log.freq", "step.cont.glob.dir", "N")]

p.mat <- cor.mtest(WJD_corpus_contin)
M.dvs <- cor(WJD_corpus_contin, use = "complete")
corrplot::corrplot(M.dvs, method = "number", type = "lower", p.mat = p.mat, sig.level = 0.05)


fa.parallel(WJD_corpus_contin)

pc <- pca(WJD_corpus_contin, nfactors = 3)

pc

write_rds(WJD_corpus, file = "WJD_corpus.RDS")

