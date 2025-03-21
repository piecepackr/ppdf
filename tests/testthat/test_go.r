test_that("go games work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-1")
    skip_on_os("windows")

    expect_equal(nrow(go_games()), 2L)

    expect_snapshot(cat_piece(go_bits()))
    expect_snapshot(cat_piece(go_go()))
    expect_snapshot(cat_piece(go_gomoku()))
})
