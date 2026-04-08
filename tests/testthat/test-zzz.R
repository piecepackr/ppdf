test_that("assert_suggested()", {
	expect_no_error(assert_suggested("base"))
	expect_error(
		assert_suggested("nonexistentpackage"),
		regexp = "nonexistentpackage",
		class = "piecepackr_suggested_package"
	)
})
