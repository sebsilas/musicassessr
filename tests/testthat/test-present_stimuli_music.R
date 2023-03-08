
test_that("format.notes.scientific_music_notation works", {

  expect_equal(format.notes.scientific_music_notation(c("C3", "C#3", "D3", "Eb3")),
    "<note>\n        <pitch>\n        <step>C</step><octave>3</octave>\n        </pitch>\n        <duration>4</duration>\n        <type>whole</type>\n        </note><note> <pitch>\n                <step>C</step><alter>1</alter><octave>3</octave>\n                </pitch>\n                <duration>4</duration>\n                <type>whole</type></note><note> <pitch>\n                <step>D</step><octave>3</octave>\n                </pitch>\n                <duration>4</duration>\n                <type>whole</type><accidental>natural</accidental></note><note> <pitch>\n                <step>E</step><alter>-1</alter><octave>3</octave>\n                </pitch>\n                <duration>4</duration>\n                <type>whole</type></note>"
  )
})
