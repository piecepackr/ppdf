test_that("chess variant setups work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(games_chess()), 2L)

    cat_piece <- function(df, ..., color = FALSE) ppcli::cat_piece(df, ..., color = color)

    expect_snapshot(cat_piece(chess_international_chess()))
    expect_snapshot(cat_piece(chess_fischer_random_chess(seed = 12)))
})
