test_that("piecepack other games work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_snapshot(cat_piece(piecepack_12345ive(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_breakthrough(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_change_change(seed = 37), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_dao(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_dodgem(), reorient = "all", annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_dodgem(nrows = 12), reorient = "all", annotate = TRUE))
	expect_snapshot(cat_piece(
		piecepack_dodgem(variant = "schoessow"),
		reorient = "all",
		annotate = TRUE
	))
	expect_snapshot(cat_piece(piecepack_evade(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_grasshopper(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_lines_of_action(), reorient = "all", annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_lukawan(), annotate = TRUE))
	expect_snapshot(cat_piece(piecepack_quatri(), annotate = TRUE))
})
