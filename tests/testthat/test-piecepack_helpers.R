test_that("process_tiles()", {
	expect_error(process_tiles("&^&&"))
})

test_that("piecepack_pyramids() works", {
	skip_on_os("windows")
	skip_if_not_installed("ppcli")
	expect_snapshot(cat_piece(piecepack_pyramids()))
})

test_that("piecepack_saucers() works", {
	skip_on_os("windows")
	skip_if_not_installed("ppcli", minimum_version = "0.3.0-2")
	expect_snapshot(cat_piece(piecepack_saucers()))
})
