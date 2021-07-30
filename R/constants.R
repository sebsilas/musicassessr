# constants

## audio

midi.gamut <- 21:108
midi.gamut.min <- midi.gamut[1]
midi.gamut.max <- midi.gamut[length(midi.gamut)]

sounds.list <- c('bass-electric','bassoon','cello','clarinet','contrabass','flute','french-horn','guitar-acoustic','guitar-electric','guitar-nylon', 'harmonium','harp','organ','piano','saxophone','trombone','trumpet','tuba','violin','xylophone')

gamut.freqs <- hrep::midi_to_freq(midi.gamut)
bandwidth <- 27.5 - 4186.00904
lowest.allowed.freq <- round(gamut.freqs[[1]], 3)
highest.allowed.freq <- round(gamut.freqs[[length(gamut.freqs)]], 3)

# min confidence to use for crepe
min.confidence <- 0.8

