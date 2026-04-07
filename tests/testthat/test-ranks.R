test_that("cast to rank integers", {
	expect_equal(chess_rank(c("P", "N", "B", "R", "Q", "K")), 1:6)
	expect_equal(chess_rank(c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a")), 1:6)
	expect_equal(dice_rank(c("1", "2", "6")), c(1L, 2L, 6L))
	expect_equal(dice_rank(c("\u2680", "\u2681", "\u2685")), c(1L, 2L, 6L))
	expect_equal(tarot_rank(c("ace", "ten", "jack", "knight", "queen", "king")), c(1L, 10:14))

	expect_error(tarot_rank("foobar"))
})
