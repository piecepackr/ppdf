test_that("none", {
    expect_equal(nrow(alquerque_none()), 0L)
    expect_equal(nrow(checker_none()), 0L)
    expect_equal(nrow(chess_none()), 0L)
    expect_equal(nrow(domino_none()), 0L)
    expect_equal(nrow(go_none()), 0L)
    expect_equal(nrow(marble_none()), 0L)
    expect_equal(nrow(morris_none()), 0L)
    expect_equal(nrow(piecepack_none()), 0L)
    expect_equal(nrow(stackpack_none()), 0L)
    expect_equal(nrow(tarot_none()), 0L)
})
