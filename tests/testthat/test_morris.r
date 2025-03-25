test_that("morris games work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-5")
    skip_on_os("windows")

    expect_equal(nrow(morris_games()), 7L)

    expect_snapshot(cat_piece(morris_three_mens_morris()))
    expect_snapshot(cat_piece(morris_five_mens_morris()))
    expect_snapshot(cat_piece(morris_six_mens_morris()))
    expect_snapshot(cat_piece(morris_seven_mens_morris()))
    expect_snapshot(cat_piece(morris_nine_mens_morris()))
    expect_snapshot(cat_piece(morris_ten_mens_morris()))
    expect_snapshot(cat_piece(morris_twelve_mens_morris()))
})
