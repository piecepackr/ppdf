test_that("cube_bits()", {
	skip_if_not_installed("ppcli", "0.3.0-1")

	expect_snapshot(cat_piece(cube_bits()))
})
