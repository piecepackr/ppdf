test_that("dice work as expected", {
    skip_if_not_installed("ppcli", "0.2.0-8")
    skip_on_os("windows")

    expect_equal(nrow(d4_dice()), 6L)
    expect_equal(nrow(d8_dice()), 6L)
    expect_equal(nrow(d10_dice()), 6L)
    expect_equal(nrow(percentile_dice()), 6L)
    expect_equal(nrow(d12_dice()), 6L)
    expect_equal(nrow(d20_dice()), 6L)

    expect_snapshot(cat_piece(dice_dice()))
    expect_snapshot(cat_piece(numeral_dice()))
    expect_snapshot(cat_piece(fudge_dice()))
})
