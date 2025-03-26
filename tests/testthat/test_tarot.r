test_that("tarot games work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-1")
    skip_on_os("windows")

    expect_equal(nrow(tarot_games()), 0L)
    expect_equal(nrow(tarot_cards()), 4L * 13L)
})
