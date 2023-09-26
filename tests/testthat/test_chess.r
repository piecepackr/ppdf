test_that("chess variant setups work as expected", {
    skip_if_not_installed("ppgames")
    skip_on_os("windows")

    expect_equal(nrow(games_chess()), 2L)

    cat_piece <- function(df, ..., color = FALSE) ppgames::cat_piece(df, ..., color = color)

    expect_snapshot(cat_piece(chess_international_chess()))
    expect_snapshot(cat_piece(chess_fischer_random_chess(seed = 12)))
})
