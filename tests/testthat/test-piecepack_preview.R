test_that("`piecepack_preview()` works as expected", {
	skip_on_os("windows")
	skip_if_not_installed("ppcli", minimum_version = "0.3.0-2")
	expect_snapshot(cat_piece(piecepack_preview()))
})
