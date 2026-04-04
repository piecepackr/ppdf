test_that("checker other games work as expected", {
	skip_if_not_installed("ppcli")
	skip_on_os("windows")

	expect_snapshot(cat_piece(checker_crossings()))
	expect_snapshot(cat_piece(checker_dao()))
	expect_snapshot(cat_piece(checker_focus()))
	expect_snapshot(cat_piece(checker_four_field_kono()))
	expect_snapshot(cat_piece(checker_grasshopper()))
	expect_snapshot(cat_piece(checker_julgonu()))
	expect_snapshot(cat_piece(checker_lines_of_action()))
})
