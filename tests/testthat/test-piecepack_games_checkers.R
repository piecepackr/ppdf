test_that("piecepack checker games work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_snapshot(cat_piece(piecepack_checkers()))
	expect_snapshot(cat_piece(piecepack_turkish_checkers()))
})
