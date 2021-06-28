library(rjson)

#t <- readLines("/Users/sebsilas/corpuses/berkowitz_midi_rhythmic/Berkowitz_Absolute.txt")

t2 <- lapply(t, fromJSON)

rel <- lapply(t2, diff)


#saveRDS(t2, "/Users/sebsilas/corpuses/Berkowitz_Absolute.RDS")
#saveRDS(rel, "/Users/sebsilas/corpuses/Berkowitz_Relative.RDS")

