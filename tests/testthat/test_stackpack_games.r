test_that("piecepack stackpack diagrams work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(stackpack_games()), 10L)

    expect_snapshot(cat_piece(stackpack_alice_chess(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_chaturaji(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_fischer_random_chess(seed = 42)))
    expect_snapshot(cat_piece(stackpack_four_seasons_chess(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_horde_chess(), reorient = "symbols"))
    expect_snapshot(cat_piece(stackpack_international_chess()))
    expect_snapshot(cat_piece(stackpack_salta()))
    expect_snapshot(cat_piece(stackpack_shogi(), reorient = "all"))
    expect_snapshot(cat_piece(stackpack_ultima()))
    expect_snapshot(cat_piece(stackpack_xiangqi()))
})
