test_that("reversi games work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-7")
    skip_on_os("windows")

    expect_equal(nrow(reversi_games()), 2L)

    expect_snapshot(cat_piece(reversi_ming_mang()))
    expect_snapshot(cat_piece(reversi_reversi()))
})
