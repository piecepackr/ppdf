test_that("game diagrams work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(games_checkers()), 23L)

    cat_piece <- function(df, ..., color = FALSE) ppcli::cat_piece(df, ..., color = color)

    expect_snapshot(cat_piece(checkers_american_checkers()))
    expect_snapshot(cat_piece(checkers_american_pool_checkers()))
    expect_snapshot(cat_piece(checkers_canadian_checkers()))
    expect_snapshot(cat_piece(checkers_crossings())) # covers Breakthrough as well
    expect_snapshot(cat_piece(checkers_dameo()))
    expect_snapshot(cat_piece(checkers_focus()))
    expect_snapshot(cat_piece(checkers_four_field_kono()))
    expect_snapshot(cat_piece(checkers_grasshopper()))
    expect_snapshot(cat_piece(checkers_international_checkers()))
    expect_snapshot(suppressWarnings(cat_piece(checkers_italian_checkers()))) # rotated board
    expect_snapshot(cat_piece(checkers_julgonu()))
    expect_snapshot(cat_piece(checkers_lines_of_action()))
    expect_snapshot(cat_piece(checkers_thai_checkers()))
    expect_snapshot(cat_piece(checkers_turkish_checkers()))
})
