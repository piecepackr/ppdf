test_that("normalize_name()", {
    skip_if_not_installed("snakecase")
    expect_equal(normalize_name("Fuji-san"), "fujisan")
    expect_equal(normalize_name("Nine Men's Morris", sep = "-"), "nine-mens-morris")
})
