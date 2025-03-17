test_that("checkers games work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(checker_games()), 31L)

    expect_snapshot(cat_piece(checker_american_checkers()))
    expect_snapshot(cat_piece(checker_american_pool_checkers()))
    expect_snapshot(cat_piece(checker_corner_checkers()))
    expect_snapshot(cat_piece(checker_crossings())) # covers Breakthrough as well
    expect_snapshot(cat_piece(checker_dameo()))
    expect_snapshot(cat_piece(checker_dao()))
    expect_snapshot(cat_piece(checker_focus()))
    expect_snapshot(cat_piece(checker_four_field_kono()))
    expect_snapshot(cat_piece(checker_grasshopper()))
    expect_snapshot(cat_piece(checker_international_checkers()))
    expect_snapshot(suppressWarnings(cat_piece(checker_italian_checkers()))) # rotated board
    expect_snapshot(cat_piece(checker_lasca()))
    expect_snapshot(cat_piece(checker_julgonu()))
    expect_snapshot(cat_piece(checker_lines_of_action()))
    expect_snapshot(cat_piece(checker_one_way_checkers()))
    expect_snapshot(cat_piece(checker_singaporean_checkers())) # covers Canadian/Malaysian
    expect_snapshot(cat_piece(checker_thai_checkers()))
    expect_snapshot(cat_piece(checker_turkish_checkers()))
    expect_snapshot(cat_piece(checker_zimbabwean_pool_checkers()))
})
