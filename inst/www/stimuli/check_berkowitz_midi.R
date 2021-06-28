library(tuneR)
library(dplyr)

path.to.midi <- 'www/item_banks/berkowitz_midi_rhythmic_100bpm/'

midi.files <- list.files(path = path.to.midi, pattern = "\\.mid$")

midi.files <- lapply(midi.files, function(x) paste0(path.to.midi, x))

look <- lapply(midi.files, function(x) {
  df <- tuneR::readMidi(x)
  df[df$event == 'Set Tempo', ]
  })

look <- dplyr::bind_rows(look)


da <- tuneR::readMidi(midi.files[[1]])

# https://www.recordingblogs.com/wiki/midi-set-tempo-meta-message
60000000 / as.numeric(look$parameterMetaSystem[1])
# i.e, they're all set to 120 bpm
