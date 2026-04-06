test_that("piecepack traditional games work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_snapshot(cat_piece(piecepack_alquerque()))
	expect_snapshot(cat_piece(piecepack_alquerque(TRUE)))
	expect_snapshot(cat_piece(piecepack_backgammon()))
	expect_snapshot(cat_piece(piecepack_baghchal()))
	expect_snapshot(cat_piece(piecepack_baghchal(TRUE)))
	expect_snapshot(cat_piece(piecepack_brandubh(), reorient = "all"))
	expect_snapshot(cat_piece(piecepack_cribbage_board()))
	expect_snapshot(cat_piece(piecepack_julgonu()))
	expect_snapshot(cat_piece(piecepack_nine_mens_morris(has_matchsticks = TRUE)))
	expect_snapshot(cat_piece(piecepack_salta()))
	expect_snapshot(cat_piece(piecepack_tablut(), reorient = "all"))
	expect_equal(nrow(piecepack_tablut(0.75)), 42L)
	expect_equal(nrow(piecepack_tablut(0.50)), 45L)
	expect_snapshot(cat_piece(piecepack_yote()))

	# graphic checks
	skip_on_cran()
	skip_if_not_installed("vdiffr")
	expect_doppelganger <- vdiffr::expect_doppelganger
	skip_if_not_installed("piecepackr", minimum_version = "1.13.3")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dejavu Sans"))
	withr::local_options(list(
		piecepackr.cfg = NULL,
		piecepackr.op_angle = NULL,
		piecepackr.op_scale = NULL,
		piecepackr.trans = NULL
	))

	ee <- piecepackr::game_systems("dejavu")
	df <- piecepack_awithlaknannai_mosona(TRUE)
	validate_df(df)
	expect_doppelganger("awithlaknannai_mosona", function() {
		piecepackr::pmap_piece(df, default.units = "in", envir = ee)
	})
	df <- piecepack_ludo()
	validate_df(df)
	expect_doppelganger("ludo", function() {
		piecepackr::pmap_piece(df, default.units = "in", envir = ee)
	})
})
