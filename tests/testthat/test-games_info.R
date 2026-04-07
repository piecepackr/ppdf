test_that("piecepack_games()", {
	expect_equal(nrow(domino_games()), 9L)

	expect_equal(nrow(piecepack_games()), 102L)
})
