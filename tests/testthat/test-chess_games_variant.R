test_that("chess variant setups work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	# Ruy Lopez opening after 1. e4 e5 2. Nf3 Nc6 3. Bb5
	fen <- "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"

	expect_snapshot(cat_piece(chess_chess(fen = fen)))
	expect_snapshot(cat_piece(chess_chess960(fen = fen)))
	expect_snapshot(cat_piece(chess_international_chess()))
	expect_snapshot(cat_piece(chess_fischer_random_chess(seed = 12)))
	expect_snapshot(cat_piece(chess_horde_chess()))
	expect_snapshot(cat_piece(chess_jeson_mor(), annotate = TRUE))
	expect_snapshot(cat_piece(chess_monochrome_chess()))
	expect_snapshot(cat_piece(chess_racing_kings()))
})
