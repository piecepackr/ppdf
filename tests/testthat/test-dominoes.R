test_that("dominoes setups work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_error(
		domino_bee_donimoes(n = 8),
		"n > 7 is not currently supported"
	)
	expect_snapshot(cat_piece(domino_bee_donimoes(n = 6, seed = 12), annotate = TRUE))
	expect_snapshot(cat_piece(domino_bee_donimoes(n = 7, seed = 12), annotate = TRUE))
	expect_snapshot(cat_piece(domino_concentration(seed = 12)))
	expect_snapshot(cat_piece(domino_finder(seed = 12), annotate = TRUE))
	expect_snapshot(cat_piece(domino_freecell(n = 7, seed = 12)))
	expect_snapshot(cat_piece(domino_fujisan(seed = 12), annotate = TRUE, ybreaks = 1:2))
	expect_snapshot(cat_piece(domino_luzon(seed = 12)))
	expect_snapshot(cat_piece(domino_patience(seed = 12)))
	expect_snapshot(cat_piece(domino_runners(seed = 12), annotate = TRUE))
	expect_snapshot(cat_piece(domino_the_jubilee(seed = 12)))
	expect_snapshot(cat_piece(domino_the_sultan(seed = 12)))

	expect_equal(nrow(domino_tiles()), 28L) # Double-6
	expect_equal(nrow(domino_tiles(n = 10)), 55L) # Double-9
	expect_equal(nrow(domino_tiles(n = 13)), 91L) # Double-12
})
