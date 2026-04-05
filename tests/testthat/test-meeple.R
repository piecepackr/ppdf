test_that("meeple_bits()", {
	skip_on_cran()
	skip_if_not_installed("vdiffr")
	expect_doppelganger <- vdiffr::expect_doppelganger
	skip_if_not_installed("piecepackr", minimum_version = "1.13.3")
	withr::local_options(list(
		piecepackr.at.inform = FALSE,
		piecepackr.cfg = NULL,
		piecepackr.op_angle = NULL,
		piecepackr.op_scale = 0.5,
		piecepackr.trans = NULL
	))

	ee <- piecepackr::game_systems()
	a <- c(0, 90, 180, 270, 0, 90)
	df1 <- meeple_bits(side = "face", suit = 1:6, x = 1:6, y = 1, angle = a)
	df2 <- meeple_bits(side = "top", suit = 1:6, x = 1:6, y = 2, angle = a)
	df <- rbind(df1, df2)
	validate_df(df)
	expect_doppelganger("meeple_bits", function() {
		piecepackr::pmap_piece(df, default.units = "in", envir = ee)
	})
})
