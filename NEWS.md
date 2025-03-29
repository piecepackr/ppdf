ppdf 0.2.0 (development)
========================

Breaking changes
----------------

* The default for `chess_board()`'s `cell_width` argument is now `1` instead of `NULL`.
  This makes it consistent with the default for `checker_board()`'s `cell_width` argument.

Deprecated features
-------------------

* The following functions have been deprecated:

  Deprecated function | Replacement function
  --- | ---
  `checkers_american_checkers()` | `checker_american_checkers()`
  `checkers_american_pool_checkers()` | `checker_american_pool_checkers()`
  `checkers_board()` | `checker_board()`
  `checkers_brazilian_checkers()` | `checker_brazilian_checkers()`
  `checkers_breakthrough()` | `checker_breakthrough()`
  `checkers_by_name()` | `checker_setup_by_name()`
  `checkers_canadian_checkers()` | `checker_canadian_checkers()`
  `checkers_crossings()` | `checker_crossings()`
  `checkers_czech_checkers()` | `checker_czech_checkers()`
  `checkers_checkers()` | `checker_checkers()`
  `checkers_dameo()` | `checker_dameo()`
  `checkers_english_checkers()` | `checker_english_checkers()`
  `checkers_focus()` | `checker_focus()`
  `checkers_four_field_kono()` | `checker_four_field_kono()`
  `checkers_frisian_checkers()` | `checker_frisian_checkers()`
  `checkers_gothic_checkers()` | `checker_gothic_checkers()`
  `checkers_grasshopper()` | `checker_grasshopper()`
  `checkers_international_checkers()` | `checker_international_checkers()`
  `checkers_italian_checkers()` | `checker_italian_checkers()`
  `checkers_jamaican_checkers()` | `checker_jamaican_checkers()`
  `checkers_julgonu()` | `checker_julgonu()`
  `checkers_lines_of_action()` | `checker_lines_of_action()`
  `checkers_none()` | `checker_none()`
  `checkers_portuguese_checkers()` | `checker_portuguese_checkers()`
  `checkers_russian_checkers()` | `checker_russian_checkers()`
  `checkers_spanish_checkers()` | `checker_spanish_checkers()`
  `checkers_thai_checkers()` | `checker_thai_checkers()`
  `checkers_turkish_checkers()` | `checker_turkish_checkers()`
  `chess_by_name()` | `chess_setup_by_name()`
  `dominoes_by_name()` | `domino_setup_by_name()`
  `dominoes_concentration()` | `domino_concentration()`
  `dominoes_domino_finder()` | `domino_finder()`
  `dominoes_domino_runners()` | `domino_runners()`
  `dominoes_fujisan()` | `domino_fujisan()`
  `dominoes_luzon()` | `domino_luzon()`
  `dominoes_none()` | `domino_none()`
  `dominoes_patience()` | `domino_patience()`
  `dominoes_the_jubilee()` | `domino_the_jubilee()`
  `dominoes_tiles()` | `domino_tiles()`
  `games_chess()` | `chess_games()`
  `games_checkers()` | `checker_games()`
  `games_dominoes()` | `domino_games()`
  `games_piecepack()` | `piecepack_games()`
  `games_stackpack()` | `stackpack_games()`
  `piecepack_piecepack_accordion()` | `piecepack_accordion()`
  `piecepack_by_name()` | `piecepack_setup_by_name()`
  `piecepack_piecepack_halma()` | `piecepack_halma()`
  `piecepack_piecepack_klondike()` | `piecepack_klondike()`
  `piecepack_rect_board_tiles()` | `piecepack_rectangular_board()`
  `stackpack_by_name()` | `stackpack_setup_by_name()`

New features
------------

* The following functions are helper functions for setup functions (#5, #7, #32, #63):

  + `alquerque_bits()`
  + `alquerque_board()`
  + `checker_bits()`
  + `chess_bits()`
  + `chess_rank()`
  + `d4_dice()`
  + `d8_dice()`
  + `d10_dice()`
  + `d12_dice()`
  + `d20_dice()`
  + `dice_dice()`
  + `dice_rank()`
  + `domino_suit()`
  + `fudge_dice()`
  + `fudge_dice_rank()`
  + `go_bits()`
  + `go_board()`
  + `marble_bits()`
  + `marble_board()`
  + `meeple_bits()`
  + `morris_bits()`
  + `morris_board()`
  + `numeral_dice()`
  + `percentile_dice()`
  + `percentile_dice_rank()`
  + `piece_angle()`
  + `piece_rank()`
  + `piece_suit()`
  + `piecepack_coins()`
  + `piecepack_dice()`
  + `piecepack_donut_board()`
  + `piecepack_matchsticks()`
  + `piecepack_pawns()`
  + `piecepack_rectangular_board()`
  + `piecepack_tiles()`
  + `reversi_bits()`
  + `reversi_board()`
  + `tarot_cards()`
  + `tarot_rank()`
  + `tarot_suit()`

* The following functions generate data frames to setup checkers/draughts variants:

  + `checker_bashni()` aka `checker_column_checkers()`
  + `checker_corner_checkers()`
  + `checker_lasca()`
  + `checker_one_way_checkers()`
  + `checker_malaysian_checkers()` aka `checker_singaporean_checkers()`
  + `checker_unified_pool_checkers()`
  + `checker_zimbabwean_pool_checkers()`

* The following functions generate data frames to setup other games playable with checkers/draughts sets:

  + `checker_dao()`

* The following functions generate data frames to setup chess variants (#11):

  + `chess_horde_chess()`
  + `chess_monochrome_chess()`
  + `chess_racing_kings()`

* The following functions generate data frames to setup checkers variants playable with piecepack sets:

  + `piecepack_bashni()` aka `piecepack_column_checkers()`
  + `piecepack_corner_checkers()`
  + `piecepack_one_way_checkers()`
  + `piecepack_unified_pool_checkers()`
  + `piecepack_zimbabwean_pool_checkers()`

* The following functions generate data frames to setup chess variants playable with piecepack sets:

  + `piecepack_fischer_random_chess()` aka `piecepack_chess960()`
  + `piecepack_racing_kings()`

* The following functions generate data frames to setup original games playable with piecepack sets (#36, #40):

  + `piecepack_dominoids()`
  + `piecepack_lukawan()`
  + `piecepack_pawns_crossing()`
  + `piecepack_ship_it()`

* The following functions generate data frames to setup other modern games playable with piecepack sets:

  + `piecepack_12345ive()`
  + `piecepack_dao()`

* The following functions generate data frames to setup other traditional games playable with piecepack sets:

  + `piecepack_baghchal()`

* The following functions generate data frames to setup games playable with piecepack stackpacks:

  + `stackpack_fischer_random_chess()` aka `stackpack_chess960()`
  + `stackpack_horde_chess()`

* The following functions generate data frames to setup games playable with alquerque sets:

  + `alquerque_alquerque()`
  + `alquerque_baghchal()`

* The following functions generate data frames to setup games playable with go sets:

  + `go_go()`
  + `go_gomoku()`

* The following functions generate data frames to setup games playable with morris sets:

  + `morris_three_mens_morris()`
  + `morris_five_mens_morris()`
  + `morris_six_mens_morris()`
  + `morris_seven_mens_morris()`
  + `morris_nine_mens_morris()`
  + `morris_ten_mens_morris()` aka `morris_lasker_morris()`
  + `morris_twelve_mens_morris()`

* The following functions generate data frames to setup games playable with reversi sets:

  + `reversi_reversi()`
  + `reversi_ming_mang()`

* The following functions generate data frames with zero rows:

  + `alquerque_none()`
  + `go_none()`
  + `marble_none()`
  + `morris_none()`
  + `reversi_none()`
  + `tarot_none()`

* The following functions generate data frames by game name:

  + `alquerque_setup_by_name()`
  + `go_setup_by_name()`
  + `marble_setup_by_name()`
  + `morris_setup_by_name()`
  + `reversi_setup_by_name()`
  + `tarot_setup_by_name()`

* The following functions generate data frames with info about the games whose setups are
  provided by this package:

  + `alquerque_games()`
  + `go_games()`
  + `marble_games()`
  + `morris_games()`
  + `reversi_games()`
  + `tarot_games()`

Bug fixes and minor improvements
--------------------------------

* `checker_board()` and `chess_board()` gain a `side` argument.
* `normalize_name()` now removes the first `"Checkers "`, `"Chess "`, `"Domino "`, `"Icehouse "`, `"Piecepack "`, or `"Stackpack "` (if any) from name e.g.
  it will now normalize "Piecepack Klondike" / "Klondike" and "Domino Patience" / "Patience" to the same name.
* Data frame functions now consistently return all the columns
  "piece\_side", "suit", "rank", "cfg", "x", "y", and "angle" (in that order)
  with the types "character", "integer", "integer", "character", "double", "double", "double" (respectively)
  without any missing values.
* "black" checker men are now the "black" suit and "white" checker men are the "white" suit
  so PPN logic is simpler.  If you want to visualize with white and red pieces mutate
  your checker data frames to swap black with red pieces (#56).
* `piecepack_julgonu()` suit colors swapped places to give better checkers results.
* `checker_dao()` suit colors swapped to better match the original setups.
* Default checker cell widths can now be set by the `ppdf.checker_cell_width` option.
* Default chess cell widths can now be set by the `ppdf.chess_cell_width` option.
* If the setup function for a given game doesn't exist for the target `system`
  then `setup_by_name()` now falls back on a piecepack setup if it exists (#43).

ppdf 0.1.1
==========

* The following functions generate data frames with info about the games whose setups are
  provided by this package:

  + `games_checkers()`
  + `games_chess()`
  + `games_dominoes()`
  + `games_piecepack()`
  + `games_stackpack()`

* The following functions are helper functions for checkers setup functions:

  + `checkers_board()`

* The following functions generate data frames to setup checkers/draughts variants:

  + `checkers_american_checkers()` aka `checkers_english_checkers()` aka `checkers_checkers()`
  + `checkers_american_pool_checkers()`
  + `checkers_brazilian_checkers()`
  + `checkers_canadian_checkers()`
  + `checkers_czech_checkers()`
  + `checkers_dameo()`
  + `checkers_frisian_checkers()`
  + `checkers_gothic_checkers()`
  + `checkers_international_checkers()`
  + `checkers_italian_checkers()`
  + `checkers_jamaican_checkers()`
  + `checkers_portuguese_checkers()`
  + `checkers_russian_checkers()`
  + `checkers_spanish_checkers()`
  + `checkers_thai_checkers()`
  + `checkers_turkish_checkers()`

* The following functions generate data frames to setup other games playable with checkers/draughts sets:

  + `checkers_breakthrough()`
  + `checkers_crossings()`
  + `checkers_focus()`
  + `checkers_four_field_kono()`
  + `checkers_grasshopper()`
  + `checkers_julgonu()`
  + `checkers_lines_of_action()`

* The following functions are helper functions for chess setup functions:

  + `chess_board()`

* The following functions generate data frames to setup chess variants:

  + `chess_fischer_random_chess()` aka `chess_chess960()`
  + `chess_international_chess()` aka `chess_chess()`

* The following functions are helper functions for dominoes setup functions:

  + `dominoes_tiles()`

* The following functions generate data frames to setup dominoes:

  + `dominoes_concentration()`
  + `dominoes_domino_finder()`
  + `dominoes_domino_runners()`
  + `dominoes_fujisan()`
  + `dominoes_luzon()`
  + `dominoes_patience()`
  + `dominoes_the_jubilee()`

* The following functions are helper functions for piecepack setup functions:

  + `piecepack_rect_board_tiles()`

* The following functions generate data frames to setup checkers variants playable with piecepack sets:

  + `piecepack_american_checkers()` aka `piecepack_english_checkers()` aka `piecepack_checkers()`
  + `piecepack_american_pool_checkers()`
  + `piecepack_brazilian_checkers()`
  + `piecepack_czech_checkers()`
  + `piecepack_gothic_checkers()`
  + `piecepack_italian_checkers()`
  + `piecepack_jamaican_checkers()`
  + `piecepack_portuguese_checkers()`
  + `piecepack_russian_checkers()`
  + `piecepack_spanish_checkers()`
  + `piecepack_thai_checkers()`
  + `piecepack_turkish_checkers()`

* The following functions generate data frames to setup chess variants playable with piecepack sets:

  + `piecepack_alice_chess()`
  + `piecepack_chaturaji()`
  + `piecepack_four_seasons_chess()`
  + `piecepack_international_chess()` aka `piecepack_chess()`
  + `piecepack_minishogi()`
  + `piecepack_shogi()` aka `piecepack_japanese_chess()`
  + `piecepack_ultima()` aka `piecepack_baroque_chess()`
  + `piecepack_xiangqi()` aka `piecepack_chinese_chess()`

* The following functions generate data frames to setup original games playable with piecepack sets:

  + `piecepack_alien_city()`
  + `piecepack_black_pawn_trucking()`
  + `piecepack_brain_burn()`
  + `piecepack_burbuja()`
  + `piecepack_cardinals_guards()`
  + `piecepack_cell_management()`
  + `piecepack_chariots()`
  + `piecepack_chinese_checkers()` aka `piecepack_piecepack_halma()`
  + `piecepack_climbing_man()`
  + `piecepack_coin_collectors()`
  + `piecepack_crocodile_hop()`
  + `piecepack_desfases()`
  + `piecepack_easy_slider()`
  + `piecepack_everest()`
  + `piecepack_four_blind_mice()`
  + `piecepack_froggy_bottom()`
  + `piecepack_fujisan()`
  + `piecepack_galaxy_express()`
  + `piecepack_ice_floe()`
  + `piecepack_iceberg()`
  + `piecepack_japan()`
  + `piecepack_lab_rats()`
  + `piecepack_landlocked()`
  + `piecepack_ley_lines()`
  + `piecepack_mathrix()`
  + `piecepack_one_man_thrag()`
  + `piecepack_pass_the_food()`
  + `piecepack_piece_gaps()`
  + `piecepack_piece_packing_pirates()`
  + `piecepack_piecepack_klondike()`
  + `piecepack_piecepackman()`
  + `piecepack_plans_of_action()`
  + `piecepack_relativity()`
  + `piecepack_san_andreas()`
  + `piecepack_sarcophagus()`
  + `piecepack_shopping_mall()`
  + `piecepack_skyscrapers()`
  + `piecepack_slides_of_action()`
  + `piecepack_speedy_towers()`
  + `piecepack_steppin_stones()`
  + `piecepack_the_in_crowd()`
  + `piecepack_the_magic_bag()`
  + `piecepack_the_penguin_game()`
  + `piecepack_tower_of_babel()` aka `piecepack_piecepack_accordion()`
  + `piecepack_tracers()`
  + `piecepack_triactor()`
  + `piecepack_tula()`
  + `piecepack_wormholes()`

* The following functions generate data frames to setup other modern games playable with piecepack sets:

  + `piecepack_breakthrough()`
  + `piecepack_change_change()`
  + `piecepack_crossings()`
  + `piecepack_evade()`
  + `piecepack_grasshopper()`
  + `piecepack_lines_of_action()`
  + `piecepack_quatri()`

* The following functions generate data frames to setup other traditional games playable with piecepack sets:

  + `piecepack_alquerque()`
  + `piecepack_awithlaknannai_mosona()`
  + `piecepack_backgammon()`
  + `piecepack_brandubh()`
  + `piecepack_cribbage()` aka `piecepack_cribbage_board()`
  + `piecepack_four_field_kono()`
  + `piecepack_julgonu()`
  + `piecepack_ludo()`
  + `piecepack_nine_mens_morris()`
  + `piecepack_salta()`
  + `piecepack_tablut()`
  + `piecepack_twelve_mens_morris()`

* The following functions generate data frames to setup games playable with piecepack stackpacks:

  + `stackpack_alice_chess()`
  + `stackpack_chaturaji()`
  + `stackpack_four_seasons_chess()`
  + `stackpack_international_chess()` aka `stackpack_chess()`
  + `stackpack_salta()`
  + `stackpack_shogi()` aka `stackpack_japanese_chess()`
  + `stackpack_ultima()` aka `stackpack_baroque_chess()`
  + `stackpack_xiangqi()` aka `stackpack_chinese_chess()`

* The following functions generate data frames with zero rows:

  + `checkers_none()`
  + `chess_none()`
  + `dominoes_none()`
  + `piecepack_none()`
  + `stackpack_none()`

* The following functions generate data frames by game name:

  + `setup_by_name()`
  + `checkers_by_name()`
  + `chess_by_name()`
  + `dominoes_by_name()`
  + `piecepack_by_name()`
  + `stackpack_by_name()`

* Other utilities intended for developers:

  + `normalize_name()`
