test_that("cast to angle doubles", {
	expect_equal(piece_angle(c("^", "<", "v", ">")), c(0, 90, 180, 270))
	expect_error(piece_angle("foobar"))
})
