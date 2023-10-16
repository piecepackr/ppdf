test_that("dominoes setups work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(games_dominoes()), 1L)

    cat_piece <- function(df, ..., color = FALSE) ppcli::cat_piece(df, ..., color = color)
    expect_snapshot(cat_piece(dominoes_fujisan(seed = 12)))
})
