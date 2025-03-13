test_that("checkers games work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(checkers_games()), 31L)

    expect_snapshot(cat_piece(checkers_american_checkers()))
    expect_snapshot(cat_piece(checkers_american_pool_checkers()))
    expect_snapshot(cat_piece(checkers_corner_checkers()))
    expect_snapshot(cat_piece(checkers_crossings())) # covers Breakthrough as well
    expect_snapshot(cat_piece(checkers_dameo()))
    expect_snapshot(cat_piece(checkers_dao()))
    expect_snapshot(cat_piece(checkers_focus()))
    expect_snapshot(cat_piece(checkers_four_field_kono()))
    expect_snapshot(cat_piece(checkers_grasshopper()))
    expect_snapshot(cat_piece(checkers_international_checkers()))
    expect_snapshot(suppressWarnings(cat_piece(checkers_italian_checkers()))) # rotated board
    expect_snapshot(cat_piece(checkers_lasca()))
    expect_snapshot(cat_piece(checkers_julgonu()))
    expect_snapshot(cat_piece(checkers_lines_of_action()))
    expect_snapshot(cat_piece(checkers_one_way_checkers()))
    expect_snapshot(cat_piece(checkers_singaporean_checkers())) # covers Canadian/Malaysian
    expect_snapshot(cat_piece(checkers_thai_checkers()))
    expect_snapshot(cat_piece(checkers_turkish_checkers()))
    expect_snapshot(cat_piece(checkers_zimbabwean_pool_checkers()))
})
