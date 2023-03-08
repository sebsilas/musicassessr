
test_that("to_string_df works", {

  inp <- tibble::tibble(a = LETTERS, b = 1:26)
  t <- to_string_df(inp)

  expect_true(all.equal(t,

                         tibble::tibble(a = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z",
                                        b = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26")
                         ))

})
