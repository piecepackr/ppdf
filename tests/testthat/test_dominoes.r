test_that("dominoes setups work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(games_dominoes()), 7L)

    expect_snapshot(cat_piece(dominoes_concentration(seed = 12)))
    expect_snapshot(cat_piece(dominoes_domino_finder(seed = 12)))
    expect_snapshot(cat_piece(dominoes_domino_runners(seed = 12)))
    expect_snapshot(cat_piece(dominoes_fujisan(seed = 12)))
    expect_snapshot(cat_piece(dominoes_luzon(seed = 12)))
    expect_snapshot(cat_piece(dominoes_patience(seed = 12)))
    expect_snapshot(cat_piece(dominoes_the_jubilee(seed = 12)))

    expect_equal(nrow(dominoes_tiles()), 28L)
    expect_equal(nrow(dominoes_tiles(n=10)), 55L)
    expect_equal(nrow(dominoes_tiles(n=13)), 91L)
})
