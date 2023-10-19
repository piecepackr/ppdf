test_that("piecepack stackpack diagrams work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    cat_piece <- function(df, ..., color = FALSE) ppcli::cat_piece(df, ..., color = color)

    expect_equal(nrow(games_stackpack()), 8L)

    expect_snapshot(cat_piece(stackpack_alice_chess(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_chaturaji(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_four_seasons_chess(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_international_chess()))
    expect_snapshot(cat_piece(stackpack_salta()))
    expect_snapshot(cat_piece(stackpack_shogi(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_ultima()))
    expect_snapshot(cat_piece(stackpack_xiangqi()))
})
