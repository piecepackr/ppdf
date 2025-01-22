test_that("piecepack diagrams work as expected", {
    skip_if_not_installed("ppcli")
    skip_on_os("windows")

    expect_equal(nrow(piecepack_games()), 90L)

    expect_equal(piecepack_by_name("Piecepack Halma"), piecepack_halma())
    expect_equal(piecepack_by_name("Piecepackman", seed = 42),
                 piecepack_piecepackman(seed = 42))

    expect_snapshot(cat_piece(piecepack_alice_chess()))
    expect_snapshot(cat_piece(piecepack_alien_city(seed=42), reorient = "symbols"))
    expect_snapshot({
        tiles <- "G^R^K^R^/R<B<GvB^/B<R^K<B</GvR^K>B>/G>K>G>K<"
        cat_piece(piecepack_alien_city(seed=42, tiles=tiles),
                  reorient = "symbols")
    })

    expect_snapshot({
        tiles <- "G3^Rn^K3^R4^/R3<Ba<GnvB4^/B2<R2^Ka<Bn</G4vRa^K4>B3>/Ga>Kn>G2>K2<"
        cat_piece(piecepack_alien_city(seed=42, tiles=tiles),
                  reorient = "symbols")
    })
    expect_snapshot(cat_piece(piecepack_alquerque()))
    expect_snapshot(cat_piece(piecepack_alquerque(TRUE)))
    expect_snapshot(cat_piece(piecepack_backgammon()))
    expect_snapshot(cat_piece(piecepack_black_pawn_trucking(seed = 25)))
    expect_snapshot(cat_piece(piecepack_brandubh(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_brain_burn(seed = 25)))
    expect_snapshot(cat_piece(piecepack_burbuja(seed = 25)))
    expect_snapshot(cat_piece(piecepack_breakthrough()))
    expect_snapshot(cat_piece(piecepack_change_change(seed = 37)))
    expect_snapshot(cat_piece(piecepack_chariots()))
    expect_snapshot(cat_piece(piecepack_chaturaji(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_checkers()))
    expect_snapshot(cat_piece(piecepack_cardinals_guards(seed = 33)))
    expect_snapshot(cat_piece(piecepack_chinese_checkers(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_climbing_man(seed = 42)))
    expect_snapshot(cat_piece(piecepack_coin_collectors(seed = 15)))
    expect_snapshot(cat_piece(piecepack_cribbage_board()))
    expect_snapshot(cat_piece(piecepack_crocodile_hop(seed = 42)))
    expect_snapshot({
        df <- piecepack_desfases(seed=42)
        tiles <- generate_sra(df)
        dice <- generate_sra(df, "^die", "r")
        df <- piecepack_desfases(tiles=tiles, dice=dice)
        cat_piece(df, reorient = "symbols")
    })
    expect_snapshot(cat_piece(piecepack_dominoids(seed=42)))
    expect_snapshot(cat_piece(piecepack_easy_slider(seed=71)))
    expect_snapshot(cat_piece(piecepack_evade()))
    expect_snapshot(cat_piece(piecepack_everest()))
    expect_snapshot(cat_piece(piecepack_froggy_bottom()))
    expect_snapshot(cat_piece(piecepack_four_blind_mice()))
    expect_snapshot(cat_piece(piecepack_four_seasons_chess(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_galaxy_express(seed = 42)))
    expect_snapshot(cat_piece(piecepack_grasshopper()))
    expect_snapshot({
        df <- piecepack_iceberg(seed=42)
        tiles <- generate_sra(df)
        df <- piecepack_iceberg(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(piecepack_ice_floe()))
    expect_snapshot(cat_piece(piecepack_international_chess()))
    expect_snapshot(cat_piece(piecepack_japan(seed=42)))
    expect_snapshot(cat_piece(piecepack_julgonu()))
    expect_snapshot(cat_piece(piecepack_lab_rats(seed=42), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_landlocked(seed=42)))
    expect_snapshot(cat_piece(piecepack_ley_lines()))
    expect_snapshot(cat_piece(piecepack_lines_of_action(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_mathrix(seed=72)))
    expect_snapshot(cat_piece(piecepack_minishogi(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_nine_mens_morris(has_matchsticks = TRUE)))
    expect_snapshot(cat_piece(piecepack_one_man_thrag(seed = 42)))
    expect_snapshot(cat_piece(piecepack_pass_the_food()))
    expect_snapshot({
        cat_piece(piecepack_pawns_crossing(seed=42, n_players = 2L), reorient = "symbols")
        cat_piece(piecepack_pawns_crossing(seed=42, n_players = 3L), reorient = "symbols")
        cat_piece(piecepack_pawns_crossing(seed=42, n_players = 4L), reorient = "symbols")
    })
    expect_snapshot(cat_piece(piecepack_piece_gaps(seed = 23)))
    expect_snapshot(cat_piece(piecepack_piece_packing_pirates(seed = 42)))
    expect_snapshot(cat_piece(piecepack_klondike(seed = 42)))
    expect_snapshot(cat_piece(piecepack_piecepackman(seed = 42)))
    expect_error(piecepack_piecepackman(seed = 42, variant = 2))
    expect_snapshot(cat_piece(piecepack_plans_of_action(seed=42)))
    expect_snapshot({
        coins <- "ASSCCM/CAMSMS/AAMCSS/ACAMMC"
        cat_piece(piecepack_plans_of_action(coins=coins))}
    )
    expect_snapshot(cat_piece(piecepack_quatri(), color=NULL))
    expect_snapshot(cat_piece(piecepack_relativity(seed=42)))
    expect_snapshot({
        coins <- "3ann4a/524253/345n34/a2na52"
        cat_piece(piecepack_relativity(coins=coins))
    })
    expect_snapshot(cat_piece(piecepack_salta()))
    expect_snapshot(cat_piece(piecepack_san_andreas()))
    expect_snapshot(cat_piece(piecepack_sarcophagus(seed = 42)))
    expect_snapshot(cat_piece(piecepack_ship_it(seed = 42), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_shogi(), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_shopping_mall(seed = 42), reorient = "symbols"))
    expect_snapshot(cat_piece(piecepack_skyscrapers(seed=23)))
    expect_snapshot(cat_piece(piecepack_slides_of_action()))
    expect_snapshot(cat_piece(piecepack_speedy_towers(seed=42), reorient = "all"))
    expect_snapshot(cat_piece(piecepack_steppin_stones(seed = 42), reorient = "symbols"))
    expect_snapshot(cat_piece(piecepack_the_in_crowd()))
    expect_snapshot(cat_piece(piecepack_the_magic_bag(seed=27)))
    expect_snapshot(cat_piece(piecepack_tablut(), reorient = "all"))
    expect_equal(nrow(piecepack_tablut(0.75)), 42L)
    expect_equal(nrow(piecepack_tablut(0.50)), 45L)
    expect_snapshot({
        df <- piecepack_tower_of_babel(seed=42)
        tiles <- generate_sra(df, "^tile", "sr")
        df <- piecepack_tower_of_babel(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(piecepack_the_penguin_game(seed = 42)))
    expect_snapshot(cat_piece(piecepack_tracers()))
    expect_snapshot(cat_piece(piecepack_triactor(), reorient = "all"))
    expect_snapshot({
        df <- piecepack_tula(seed=42)
        tiles <- generate_sra(df)
        df <- piecepack_tula(tiles = tiles)
        cat_piece(df)
    })
    expect_snapshot(cat_piece(piecepack_turkish_checkers()))
    expect_snapshot(cat_piece(piecepack_ultima()))
    expect_snapshot(cat_piece(piecepack_wormholes()))
    expect_snapshot(cat_piece(piecepack_xiangqi(), annotate = "cartesian"))


    expect_error(process_tiles("&^&&"))

    # graphic checks
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    expect_doppelganger <- vdiffr::expect_doppelganger
    skip_if_not_installed("piecepackr", minimum_version = "1.13.3")
    skip_if_not_installed("systemfonts")
    skip_if_not(piecepackr::has_font("Dejavu Sans"))
    withr::local_options(list(piecepackr.cfg = NULL,
                              piecepackr.op_angle = NULL,
                              piecepackr.op_scale = NULL,
                              piecepackr.trans = NULL))

    ee <- piecepackr::game_systems("dejavu")
    df <- piecepack_awithlaknannai_mosona(TRUE)
    validate_df(df)
    expect_doppelganger("awithlaknannai_mosona", function() {
        piecepackr::pmap_piece(df, default.units = "in", envir = ee)
    })
    df <- piecepack_cell_management(seed = 42)
    validate_df(df)
    expect_doppelganger("cell_management", function() {
        piecepackr::pmap_piece(df, default.units = "in", envir = ee)
    })
    df <- piecepack_ludo()
    validate_df(df)
    expect_doppelganger("ludo", function() {
        piecepackr::pmap_piece(df, default.units = "in", envir = ee)
    })
})
