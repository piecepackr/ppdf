test_that("marbles", {
    skip_if_not_installed("ppcli", "0.2.0-1")
    expect_snapshot(cat_piece(marble_bits()))
})
