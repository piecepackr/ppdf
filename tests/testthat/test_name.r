test_that("normalize_name()", {
    skip_if_not_installed("snakecase")
    expect_equal(normalize_name("Fuji-san"), "fujisan")
    expect_equal(normalize_name("Nine Men's Morris", sep = "-"), "nine-mens-morris")
})

test_that("setup_by_name()", {
    skip_if_not_installed("ppcli", "0.2.0-1")
    expect_snapshot(cat_piece(alquerque_setup_by_name("Alquerque")))
    expect_snapshot(cat_piece(checker_setup_by_name("Turkish Draughts")))
    expect_snapshot(cat_piece(chess_setup_by_name("Chess960", seed = 23)))
    expect_snapshot(cat_piece(domino_setup_by_name("Domino Fuji-san", seed = 42)))
    expect_snapshot(cat_piece(marble_setup_by_name("Board")))
    expect_snapshot(cat_piece(piecepack_setup_by_name("Fuji-san", seed = 42)))
    expect_snapshot(cat_piece(stackpack_setup_by_name("Salta")))
    expect_snapshot(cat_piece(setup_by_name("Four Field Kono")))
})

test_that("normalize_system()", {
    expect_equal(normalize_system("dominoes"), "domino")
    expect_equal(normalize_system("draughts"), "checker")
    expect_equal(normalize_system("icehouse"), "icehouse")
    expect_equal(normalize_system("icehouse_pieces"), "icehouse")
    expect_equal(normalize_system("looney_pyramids"), "icehouse")
    expect_equal(normalize_system("piecepack_stackpack"), "stackpack")
    skip_on_ci() # Gives different snapshot on Github?
    expect_snapshot(normalize_system("decktet"))
})
