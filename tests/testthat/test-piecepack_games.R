test_that("piecepack_games()", {
	expect_equal(nrow(piecepack_games()), 101L)
	expect_equal(piecepack_by_name("Piecepack Halma"), piecepack_halma())
	expect_equal(piecepack_by_name("Piecepackman", seed = 42), piecepack_piecepackman(seed = 42))
})
