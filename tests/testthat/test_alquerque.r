test_that("alquerque games work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-1")
    skip_on_os("windows")

    expect_equal(nrow(alquerque_games()), 2L)

    expect_snapshot(cat_piece(alquerque_alquerque()))
    expect_snapshot(cat_piece(alquerque_baghchal()))
})
