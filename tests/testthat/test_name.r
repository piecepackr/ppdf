test_that("normalize_name()", {
    skip_if_not_installed("snakecase")
    expect_equal(normalize_name("Fuji-san"), "fujisan")
    expect_equal(normalize_name("Nine Men's Morris", sep = "-"), "nine-mens-morris")
})

test_that("setup_by_name()", {
    skip_if_not_installed("ppcli")
    expect_snapshot(ppcli::cat_piece(checkers_by_name("Turkish Draughts")))
    expect_snapshot(ppcli::cat_piece(chess_by_name("Chess960", seed = 23)))
    expect_snapshot(ppcli::cat_piece(dominoes_by_name("Fuji-san", seed = 42)))
    expect_snapshot(ppcli::cat_piece(piecepack_by_name("Fuji-san", seed = 42)))
    expect_snapshot(ppcli::cat_piece(stackpack_by_name("Salta")))
    expect_snapshot(ppcli::cat_piece(setup_by_name("Four Field Kono")))
})

test_that("normalize_system()", {
    expect_equal(normalize_system("draughts"), "checkers")
    expect_equal(normalize_system("icehouse"), "icehouse")
    expect_equal(normalize_system("icehouse_pieces"), "icehouse")
    expect_equal(normalize_system("looney_pyramids"), "icehouse")
    expect_equal(normalize_system("piecepack_stackpack"), "stackpack")
    expect_snapshot(normalize_system("decktet"))
})
