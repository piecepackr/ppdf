test_that("none", {
    expect_equal(nrow(checkers_none()), 0L)
    expect_equal(nrow(chess_none()), 0L)
    expect_equal(nrow(dominoes_none()), 0L)
    expect_equal(nrow(piecepack_none()), 0L)
})
