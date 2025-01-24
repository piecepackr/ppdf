test_that("chess variant setups work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(chess_games()), 5L)

    expect_snapshot(cat_piece(chess_international_chess()))
    expect_snapshot(cat_piece(chess_fischer_random_chess(seed = 12)))
    expect_snapshot(cat_piece(chess_horde_chess()))
    expect_snapshot(cat_piece(chess_monochrome_chess()))
    expect_snapshot(cat_piece(chess_racing_kings()))
})
