test_that("none", {
    expect_equal(nrow(alquerque_none()), 0L)
    expect_equal(nrow(checkers_none()), 0L)
    expect_equal(nrow(chess_none()), 0L)
    expect_equal(nrow(domino_none()), 0L)
    expect_equal(nrow(marbles_none()), 0L)
    expect_equal(nrow(piecepack_none()), 0L)
    expect_equal(nrow(stackpack_none()), 0L)
})
