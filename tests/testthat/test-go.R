test_that("go games work as expected", {
	skip_if_not_installed("ppcli", "0.2.0-1")
	skip_on_os("windows")

	expect_snapshot(cat_piece(go_bits(), annotate = TRUE))
	expect_snapshot(cat_piece(go_connect6(), annotate = TRUE))
	expect_snapshot(cat_piece(go_go(), annotate = TRUE))
	expect_snapshot(cat_piece(go_gomoku(), annotate = TRUE))
	expect_snapshot(cat_piece(go_renju(), annotate = TRUE))
})
