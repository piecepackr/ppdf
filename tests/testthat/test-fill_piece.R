test_that("fill_piece() rank-only NA (equivalent to fill_piece_rank)", {
	withr::local_seed(42)
	df <- piecepack_tiles(suit = rep(1:4, each = 6), rank = NA_integer_)
	result <- fill_piece(df)
	expect_false(anyNA(result$rank))
	expect_false(anyNA(result$suit))
	for (s in 1:4) {
		expect_setequal(result$rank[result$suit == s], 1:6)
	}
})

test_that("fill_piece() suit-only NA (equivalent to fill_piece_suit)", {
	withr::local_seed(42)
	df <- piecepack_tiles(suit = NA_integer_, rank = rep(1:6, 4))
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	for (r in 1:6) {
		expect_setequal(result$suit[result$rank == r], 1:4)
	}
})

test_that("fill_piece() both suit and rank NA", {
	withr::local_seed(42)
	df <- piecepack_tiles(suit = NA_integer_, rank = NA_integer_)
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	expect_false(anyNA(result$rank))
	expect_length(unique(paste(result$suit, result$rank)), 24L)
})

test_that("fill_piece() mixed constraints", {
	withr::local_seed(42)
	df <- bind_rows(
		piecepack_tiles(suit = 1L, rank = NA_integer_, x = 1:6, y = 1),
		piecepack_tiles(suit = 2L, rank = NA_integer_, x = 1:6, y = 2),
		piecepack_tiles(suit = NA_integer_, rank = NA_integer_, x = 1:12, y = 3)
	)
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	expect_false(anyNA(result$rank))
	expect_length(unique(paste(result$suit, result$rank)), 24L)
})

test_that("fill_piece() partial df (subset of pieces)", {
	withr::local_seed(42)
	df <- piecepack_tiles(
		suit = c(1L, 2L, NA_integer_),
		rank = c(NA_integer_, 3L, NA_integer_),
		x = 1:3,
		y = 1
	)
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	expect_false(anyNA(result$rank))
	expect_equal(result$suit[1], 1L)
	expect_equal(result$rank[2], 3L)
	expect_length(unique(paste(result$suit, result$rank)), 3L)
})

test_that("fill_piece() pawns fill suit", {
	withr::local_seed(42)
	df <- piecepack_pawns(suit = NA_integer_)
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	expect_setequal(result$suit, 1:4)
	expect_equal(result$rank, rep(1L, 4L))
})

test_that("fill_piece() dice fill suit and set rank to 1", {
	withr::local_seed(42)
	df <- piecepack_dice(suit = NA_integer_, rank = NA_integer_)
	result <- fill_piece(df)
	expect_false(anyNA(result$suit))
	expect_setequal(result$suit, 1:4)
	expect_equal(result$rank, rep(1L, 4L))
})

test_that("fill_piece() aborts on infeasible constraints", {
	df <- bind_rows(
		piecepack_tiles(suit = 1L, rank = 1:3, x = 1:3, y = 1),
		piecepack_tiles(suit = 1L, rank = NA_integer_, x = 1:5, y = 2)
	)
	expect_error(fill_piece(df), class = "ppdf_error")
})
