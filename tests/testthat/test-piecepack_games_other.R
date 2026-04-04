test_that("piecepack other games work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_snapshot(cat_piece(piecepack_12345ive()))
	expect_snapshot(cat_piece(piecepack_breakthrough()))
	expect_snapshot(cat_piece(piecepack_change_change(seed = 37)))
	expect_snapshot(cat_piece(piecepack_dao()))
	expect_snapshot(cat_piece(piecepack_evade()))
	expect_snapshot(cat_piece(piecepack_grasshopper()))
	expect_snapshot(cat_piece(piecepack_lines_of_action(), reorient = "all"))
	expect_snapshot(cat_piece(piecepack_lukawan()))
	expect_snapshot(cat_piece(piecepack_quatri(), color = NULL))
})
