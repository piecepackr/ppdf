test_that("marbles", {
    expect_equal(nrow(marble_games()), 0L)

    skip_if_not_installed("ppcli", "0.2.0-1")
    expect_snapshot(cat_piece(marble_bits()))
})
