#' Setups for various original piecepack games
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_original())` games playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(piecepack_games_original())`
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param cfg2 A string of a piecepack expansion (or perhaps \code{"piecepack"} for a second piecepack)
#' @param tiles String of tile layout
#' @param coins String of coin layout
#' @param dice String of dice layout
#' @param pawns String of pawns layout
#' @param n_players Number of players
#' @param variant Name of variant
#' @param ... Should be left empty.
#' @rdname piecepack_games_original
#' @return `r return_df()`
#' @name piecepack_games_original
NULL

piecepack_games_original <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Alien City"
            , "``piecepack_alien_city()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/AlienCity"
            , "Black Pawn Trucking"
            , "``piecepack_black_pawn_trucking()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/BlackPawnTrucking"
            , "Brain Burn"
            , "``piecepack_brain_burn()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/BrainBurn"
            , "Burbuja"
            , "``piecepack_burbuja()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Burbuja"
            , "Cardinal's Guards"
            , "``piecepack_cardinals_guards()``"
            , NA_character_
            , "https://ludism.org/ppwiki/CardinalsGuards"
            , "Cell Management"
            , "``piecepack_cell_management()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/CellManagement"
            , "Chariots"
            , "``piecepack_chariots()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Chariots"
            , "Chinese Checkers AKA (Piecepack) Halma"
            , "``piecepack_chinese_checkers()`` aka ``piecepack_halma()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/ChineseCheckers"
            , "Climbing Man"
            , "``piecepack_climbing_man()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/ClimbingMan"
            , "Coin Collectors"
            , "``piecepack_coin_collectors()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/CoinCollectors"
            , "Crocodile Hop"
            , "``piecepack_crocodile_hop()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/CrocodileHop"
            , "Desfases"
            , "``piecepack_desfases()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Desfases"
            , "Easy Slider"
            , "``piecepack_easy_slider()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/EasySlider"
            , "Everest"
            , "``piecepack_everest()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Everest"
            , "Four Blind Mice"
            , "``piecepack_four_blind_mice()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/FourBlindMice"
            , "Froggy Bottom"
            , "``piecepack_froggy_bottom()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/FroggyBottom"
            , "Fuji-san"
            , "``piecepack_fujisan()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Fuji-san"
            , "Galaxy Express"
            , "``piecepack_galaxy_express()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/GalaxyExpress"
            , "Ice Floe"
            , "``piecepack_ice_floe()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/IceFloe"
            , "Iceberg"
            , "``piecepack_iceberg()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Iceberg"
            , "Japan"
            , "``piecepack_japan()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Japan"
            , "Lab Rats"
            , "``piecepack_lab_rats()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/LabRats"
            , "Landlocked"
            , "``piecepack_landlocked()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Landlocked"
            , "Ley Lines"
            , "``piecepack_ley_lines()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/LeyLines"
            , "Mathrix"
            , "``piecepack_mathrix()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Mathrix"
            , "One Man Thrag!"
            , "``piecepack_one_man_thrag()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/OneManThrag"
            , "Pass the Food"
            , "``piecepack_pass_the_food()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/PassTheFood"
            , "Pawns Crossing"
            , "``piecepack_pawns_crossing()``"
            , NA_character_
            , "https://ludism.org/ppwiki/PawnsCrossing"
            , "Piece Gaps"
            , "``piecepack_piece_gaps()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/PieceGaps"
            , "Piece Packing Pirates"
            , "``piecepack_piece_packing_pirates()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/PiecePackingPirates"
            , "(Piecepack) Klondike"
            , "``piecepack_klondike()``"
            , NA_character_
            , "https://ludism.org/ppwiki/PiecepackKlondike"
            , "Piecepackman"
            , "``piecepack_piecepackman()``"
            , "Currently only supports the \"Roundabout\" variant"
            , "https://www.ludism.org/ppwiki/Piecepackman"
            , "Plans of Action"
            , "``piecepack_plans_of_action()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/PlansOfAction"
            , "Relativity"
            , "``piecepack_relativity()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Relativity"
            , "San Andreas"
            , "``piecepack_san_andreas()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/SanAndreas"
            , "Sarcophagus"
            , "``piecepack_sarcophagus()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Sarcophagus"
            , "Shopping Mall"
            , "``piecepack_shopping_mall()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/ShoppingMall"
            , "Skyscrapers"
            , "``piecepack_skyscrapers()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Skyscrapers"
            , "Slides of Action"
            , "``piecepack_slides_of_action()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/SlidesOfAction"
            , "Speedy Towers"
            , "``piecepack_speedy_towers()``"
            , "Currently only supports the two players variant"
            , "https://ludism.org/ppwiki/SpeedyTowers"
            , "Steppin' Stones"
            , "``piecepack_steppin_stones()``"
            , NA_character_
            , "https://ludism.org/ppwiki/Steppin'_Stones"
            , "The \"In\" Crowd"
            , "``piecepack_the_in_crowd()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/TheInCrowd"
            , "The Magic Bag"
            , "``piecepack_the_magic_bag()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/The_Magic_Bag"
            , "The Penguin Game"
            , "``piecepack_the_penguin_game()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/PenguinGame"
            , "Tower of Babel AKA (Piecepack) Accordion"
            , "``piecepack_tower_of_babel()`` aka ``piecepack_accordion()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/TowerOfBabel"
            , "Tracers"
            , "``piecepack_tracers()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Tracers"
            , "Triactor"
            , "``piecepack_triactor()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Triactor"
            , "Tula"
            , "``piecepack_tula()``"
            , "Also supports Tim Schutz's variants 1--4"
            , "https://www.ludism.org/ppwiki/Tula"
            , "Twelve Men's Morris"
            , "``piecepack_twelve_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Morabaraba"
            , "Wormholes"
            , "``piecepack_wormholes()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Wormholes"
    )
}

#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr near
#' @importFrom dplyr select
#' @importFrom dplyr setdiff
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
#' @importFrom stringr str_glue
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tibble tribble

#' @rdname piecepack_games_original
#' @export
piecepack_alien_city <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_t1 <- tibble(piece_side = "tile_face",
                   x = 0.5+rep(seq(1,7,2),5),
                   y = 0.5+rep(seq(9,1,-2), each = 4),
                   cfg = "piecepack")
    if (is.null(tiles)) {
        df_t2 <- tibble(suit = rep(1:4, each = 5),
                        rank = rep.int(1:5, 4L)+ 1L,
                        angle = 90 * (sample(4, 20, replace = TRUE)-1))
        df_t2 <- df_t2[sample.int(20L), ]
    } else {
        df_t2 <- process_tiles(tiles, 20)
    }
    bind_cols(df_t1, df_t2) %>% select_piece()
}

#' @rdname piecepack_games_original
#' @export
piecepack_black_pawn_trucking <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = "face",
                    x = c(7,9,     8,10,   8,10,   3,5, 9,11, 2,4, 10,12, 2,4,14, 8,10,12, 14, 10,12, 8) - 0.5,
                    y = c(15,15, 13,13, 11,11, 10,10,  9,9, 8,8, 7,7,  6,6,6,  5,5,5,  4,  3,3,  2) - 0.5) %>%
        slice_sample_piece()
    df_dice <- piecepack_dice(x = c(1, 2, 1, 2), y = c(2, 2, 1, 1), rank = random_dice())
    df_ <- filter(df_tiles,
                  paste(.data$rank, .data$suit) %in% paste(df_dice$rank, df_dice$suit))
    df_pawn <- filter(df_, .data$suit == 2L) %>%
        mutate(piece_side = "pawn_face", rank = 1L,
               x = .data$x + 0.5, y = .data$y + 0.5)
    df_coins <- mutate(df_, piece_side = "coin_back",
                       x = .data$x + 0.5, y = .data$y - 0.5)
    i_sr_c <- sample.int(24L, 4L)
    df_coins$rank <- rep.int(1:6, 4L)[i_sr_c]
    df_coins$suit <- rep(1:4, each = 6L)[i_sr_c]
    bind_rows(df_tiles, df_dice, df_coins, df_pawn)
}

#' @rdname piecepack_games_original
#' @export
piecepack_brain_burn <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- tibble(piece_side = "tile_face",
                       cfg = "piecepack", angle = 0,
                       x = rep(seq(1.5, 10, 2), 5),
                       y = rep(seq(1.5, 10, 2), each = 5)) %>%
        slice(-sample.int(25, 1)) %>%
        mutate(suit = rep(1:4, each = 6L), rank = rep(1:6, 4L)) %>%
        slice_sample_piece()
    df_coins <- mutate(df_tiles,
                       piece_side = "coin_face",
                       x = .data$x + 0.5, y = .data$y - 0.5,
                       suit = rep(1:4, each = 6L),
                       rank = rep.int(1:6, 4L)) %>%
        slice_sample_piece()
    df <- bind_rows(df_tiles, df_coins) %>% select_piece()
    # attr(df, "scale_factor") <- 2
    df
}

#' @rdname piecepack_games_original
#' @export
piecepack_burbuja <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    sra <- process_tiles(tiles)
    df_tiles <- piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                                      x0 = 5.5, y0 = 9.5, side = "face")
    df_coins <- piecepack_coins(side = rep(c(rep_len("back", 5), "face"), 4),
                       rank = c((1:6)[sample.int(6)], (1:6)[sample.int(6)],
                                (1:6)[sample.int(6)], (1:6)[sample.int(6)]),
                       x = rep(seq.int(from = 2, by = 2, length.out = 4), each = 6),
                       y = rep(c(rep_len(4, 5), 2), 4))
    df_pawns <- piecepack_pawns(x = seq.int(3, length.out = 4, by = 2), y = 3)
    bind_rows(df_tiles, df_coins, df_pawns)
}

#' @rdname piecepack_games_original
#' @export
piecepack_cardinals_guards <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    sra <- process_tiles(tiles)
    df_tiles <- piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                                      x0 = 3.5, y0 = 3.5, side = "face")
    df_coins <- piecepack_coins(side = "back") %>% slice_sample_piece()
    df_coins$x <- c(rep(c(2, 14), each = 5),
                    rep(seq.int(from = 4, by = 2, length.out = 5), 2),
                    rep_len(18, 4))
    df_coins$y <- c(rep(seq.int(from = 4, by = 2, length.out = 5), 2),
                    rep(c(2, 14), each = 5),
                    seq.int(from = 2, by = 2, length.out = 4))
    df_dice <- piecepack_dice(rank = 1, x = 16,
                              y = seq.int(from = 2, by = 2, length.out = 4))
    df_pawns <- filter(df_tiles, .data$rank == 1) %>%
        mutate(piece_side = "pawn_face")
    df <- bind_rows(df_tiles, df_coins, df_dice, df_pawns)
    attr(df, "scale_factor") <- 2
    df
}

#' @rdname piecepack_games_original
#' @export
piecepack_cell_management <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)

    # hexagon distances
    #    between closest vertices: 2
    #    between vertex to opposite vertix: 4
    #    between vertex to vertex 2 away: 2*sqrt(3)
    x0 <- 5.5
    y0 <- 4.5+sqrt(3)
    # Sun and Crown tiles
    theta <- seq(30, 330, by = 60)
    r <- sqrt(3) + 1
    xt <- x0 + to_x(theta, r)
    yt <- y0 + to_y(theta, r)
    df_t <- piecepack_tiles(side = "face", suit = 1L, rank = sample.int(6L),
                            x = xt, y = yt, angle = theta-90)
    df_t[sample.int(6L, 3L), "suit"] <- 3L

    # Moon tiles and Coins
    last_played <- 1
    moon_ranks <- c(sample.int(5L)+1L, 1L)
    df_tm <- piecepack_tiles(side = "face", suit = 2L, rank = moon_ranks,
                             x = NA_real_, y = NA_real_, angle = NA_real_)
    df_c <- piecepack_coins(side = "face", length.out = 12L)
    for (ii in seq(along = moon_ranks)) {
        angle <- as.numeric(df_t[which(df_t$rank == last_played), "angle"])
        theta <- angle + 90
        xt <- x0 + to_x(theta, sqrt(3) + 3)
        yt <- y0 + to_y(theta, sqrt(3) + 3)
        last_played <- moon_ranks[ii]

        df_tm[ii, "angle"] <- angle
        df_tm[ii, "x"] <- xt
        df_tm[ii, "y"] <- yt

        is <- c(2*ii-1,2*ii)
        xc <- xt + to_x(theta+c(-135,135), 0.5*sqrt(2))
        yc <- yt + to_y(theta+c(-135,135), 0.5*sqrt(2))
        df_c[is, "angle"] <- angle
        df_c[is, "x"] <- xc
        df_c[is, "y"] <- yc
        df_c[is, "suit"] <- 1:2
        df_c[is, "rank"] <- moon_ranks[ii]
    }
    # Guards
    df_p <- piecepack_pawns(suit = 1:2, x = x0+c(4,5), y = y0)

    bind_rows(df_t, df_tm, df_c, df_p)
}

to_x <- function(t, r) r * cos(pi * t / 180)
to_y <- function(t, r) r * sin(pi * t / 180)

#' @rdname piecepack_games_original
#' @export
piecepack_chariots <- function() {
    piecepack_tiles(side = "back",
                    x = c(rep(seq(5.5, 9.5, 2), 4),
                          rep(c(3.5, 11.5), 4),
                          rep(c(1.5, 13.5), 2)),
                    y = c(rep(c(9.5,7.5,3.5,1.5), each = 3),
                          rep(c(2.00, 4.00, 7.00, 9.00), each = 2),
                          rep(c(4.5, 6.5), each = 2)))
}

#' @rdname piecepack_games_original
#' @export
piecepack_chinese_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_c <- piecepack_coins(side = "back",
                            x = c(1:3, 1:2, 1, 6:8, 7:8, 8, 8, 7:8, 6:8, 1, 1:2, 1:3),
                            y = c(8,8,8, 7,7, 6, 8,8,8, 7,7, 6, 3, 2,2, 1,1,1, 3, 2,2, 1,1,1),
                            angle = 45 + rep(c(180, 90, 0, -90), each=6))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_original
#' @export
piecepack_halma <- piecepack_chinese_checkers

#' @rdname piecepack_games_original
#' @export
piecepack_coin_collectors <- function(seed = NULL, tiles = NULL, coins = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    sra <- process_tiles(tiles)
    df_tiles <- piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                                      x0 = 1.0, y0 = 1.0, side = "face")
    df_coins <- piecepack_coins(side = "face",
                                x = df_tiles$x + 0.5, y = df_tiles$y - 0.5,
                                rank = process_ranks(coins), suit = NA_integer_) %>%
        fill_piece_suit()
    ranks <- if (is.null(dice)) random_dice() else process_ranks(dice)
    df_dice <- piecepack_dice(x = 12, y = c(9, 7, 5, 3), rank = ranks)
    df_pawn <- piecepack_pawns(x = c(5, 6, 6, 5), y = c(6, 6, 5, 5))
    bind_rows(df_tiles, df_coins, df_dice, df_pawn)
}

#' @rdname piecepack_games_original
#' @export
piecepack_climbing_man <- function(seed = NULL, variant = c("Basic", "Free")) {
    if (!is.null(seed)) withr::local_seed(seed)
    variant <- match.arg(variant)
    if (variant == "Free") {
        df_none()
    } else {
        df_tiles <- piecepack_rectangular_board(nrows = 12, ncols = 8, y0 = 2)
        repeat {
            df_coins <- df_tiles
            df_coins$x <- df_coins$x + sample(c(-0.5, 0.5), 24, replace = TRUE)
            df_coins$y <- df_coins$y + sample(c(-0.5, 0.5), 24, replace = TRUE)
            if (!any_adjacent_coins(df_coins)) {
                break
            }
        }
        df_coins <- mutate(df_coins, piece_side = "coin_back",
                           suit = rep(1:4, each = 6L),
                           rank = rep.int(1:6, 4L)) %>%
            slice_sample_piece()

        bind_rows(df_tiles, df_coins)
    }
}

any_adjacent_coins <- function(df) {
    for(i in seq_len(nrow(df))) {
        if (any(df$x[-i] == df$x[i] & abs(df$y[-i] - df$y[i]) == 1))
            return(TRUE)
        if (any(abs(df$x[-i] - df$x[i]) == 1 & df$y[-i] == df$y[i]))
            return(TRUE)
    }
    FALSE
}

#' @rdname piecepack_games_original
#' @export
piecepack_crocodile_hop <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = rep.int(c("back", "face", "back"), c(19, 2, 3)),
                       x = rep.int(rep(c(1, 3, 6.5, 8.5, 10.5), 2), c(8, 8, rep(1, 8))),
                       y = rep.int(c(3, 3.5, 1, 1.5), c(16, 3, 2, 3))) %>%
        slice_sample_piece()
    df_coins <- piecepack_coins(side = "back",
                                x = rep(seq(6, by = 1, length.out = 6), each = 4),
                                y = rep(4:1, 6),
                                suit = as.integer(replicate(6, sample.int(4))),
                                rank = rep(c(2:6, 1L), each = 4))
    last_tile_sr <- paste(df_tiles$suit[24], df_tiles$rank[24])
    df_coins <- df_coins[which(paste(df_coins$suit, df_coins$rank) != last_tile_sr), ]
    repeat { # re-roll die if equals bottom-right tile (removed lily pond)
        df_dice <- piecepack_dice(x = seq(6, length.out = 4), y = 5,
                                  rank = random_dice())
        if (!any(paste(df_dice$suit, df_dice$rank) %in% last_tile_sr))
            break
    }
    df_pawns <- filter(df_coins, paste(.data$suit, .data$rank) %in% paste(df_dice$suit, df_dice$rank)) %>%
        mutate(piece_side = "pawn_face", rank = 1L)
    bind_rows(df_tiles, df_coins, df_dice, df_pawns)
}

#' @rdname piecepack_games_original
#' @export
piecepack_desfases <- function(seed = NULL, tiles = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = "tile_face",
                     x = 2+rep(seq(1, by=3, length.out=5), 5),
                     y = 2+rep(seq(1, by=3, length.out=5), each=5),
                     cfg = "piecepack")
    df_txy <- df_txy[-13L, ]
    if (is.null(tiles)) {
        df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
        df_tsr$angle <- ((df_tsr$suit + 1) * -90) %% 360
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_t <- bind_cols(df_txy, df_tsr)

    df_c <- piecepack_coins(side = "face",
                            x = c(14:9, rep(17, 6), 4:9, rep(1, 6)),
                            y = c(rep(17, 6), 4:9, rep(1, 6), 14:9),
                            angle = rep(c(180, 90, 0, -90), each=6))

    if (is.null(dice)) {
        dice <- random_dice()
    } else {
        dice <- process_ranks(dice)
    }
    df_d <- piecepack_dice(x = c(7, 17, 11, 1), y = c(17, 11, 1, 7),
                           angle = c(180, 90, 0, -90), rank = dice)

    df_p <- df_d %>% mutate(piece_side = "pawn_face", rank = 1L)

    for (i in seq.int(4L)) {
        # Move relevant coins under their respective dice
        index <- which(df_c$rank == dice[i] & df_c$suit == i)
        df_c[index, "piece_side"] <- "coin_back"
        df_c[index, "x"] <- df_d$x[i]
        df_c[index, "y"] <- df_d$y[i]

        # Move pawns on top of their respective tiles
        index <- which(df_t$rank == dice[i] & df_t$suit == i)
        df_p[i, "x"] <- df_t$x[index]
        df_p[i, "y"] <- df_t$y[index]
    }

    df <- bind_rows(df_t, df_c, df_d, df_p) %>% select_piece()
    attr(df, "scale_factor") <- 3
    df
}

#' @rdname piecepack_games_original
#' @export
piecepack_easy_slider <- function(seed = NULL, tiles = NULL, coins = NULL, pawns = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = "tile_face",
                     x = 2 + c(rep(seq(0, 8, 2), 4), 0, 2, 4, 6),
                     y = 2 + c(rep(c(8, 6, 4, 2), each = 5), rep.int(0, 4)),
                     cfg = "piecepack")
    df_tsr <- process_tiles(tiles)
    df_tiles <- bind_cols(df_txy, df_tsr)
    ranks <- if (is.null(coins)) sample.int(5L) + 1L else process_ranks(coins)
    df_coins <- piecepack_coins(x = seq(2, 10, 2), y = 11.5, rank = ranks, suit = 1L)
    suits <- if (is.null(pawns)) sample.int(4L) else process_suits(pawns)
    df_pawns <- piecepack_pawns(x = 0.5, y = seq(10, 4, -2), suit = suits)
    df <- bind_rows(df_tiles, df_coins, df_pawns) %>% select_piece()
    attr(df, "scale_factor") <- 2
    df
}

#' @rdname piecepack_games_original
#' @export
piecepack_everest <- function() {
    df_t1 <- piecepack_tiles(side = "back", suit = NA_integer_, rank = NA_integer_,
                    x = 0.5 + c(seq(1, 7, 2),seq(2, 6, 2), seq(1, 7, 2)),
                    y = 0.5 + c(rep(1, 4), rep(3, 3), rep(5, 4)))
    df_t2 <- piecepack_rectangular_board(4, 6, x0 = 2, y0 = 2, suit = NA_integer_, rank = NA_integer_)
    df_t3 <- piecepack_rectangular_board(4, 4, x0 = 3, y0 = 2, suit = NA_integer_, rank = NA_integer_)
    df_t4 <- piecepack_rectangular_board(2, 4, x0 = 3, y0 = 3, suit = NA_integer_, rank = NA_integer_)
    df_t5 <- piecepack_tiles(side = "back", x = 4.5, y = 3.5, suit = NA_integer_, rank = NA_integer_)
    df_t <- bind_rows(df_t1, df_t2, df_t3, df_t4, df_t5) %>%
        mutate(suit = rep(1:4, each = 6L), rank = rep.int(1:6, 4L))
    df_p <- piecepack_pawns(x = c(1,8,8,1), y = c(5,5,2,2))
    bind_rows(df_t, df_p)
}

#' @rdname piecepack_games_original
#' @export
piecepack_four_blind_mice <- function() piecepack_rectangular_board(ncols = 8, nrows = 8)

#' @rdname piecepack_games_original
#' @export
piecepack_froggy_bottom <- function() piecepack_rectangular_board(ncols = 6, nrows = 8)

#' @rdname piecepack_games_original
#' @export
piecepack_fujisan <- function(seed = NULL, coins = NULL, dice = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(coins)) {
        coins <- random_fujisan_coins()
    } else if (is.character(coins)) {
        coins <- process_ranks(coins) - 1
    }
    if (is.vector(coins)) {
        coins <- matrix(coins, nrow = 2, byrow = TRUE)
    }
    df_t <- piecepack_tiles(side = "back", y = 1.5,
                   x = 1.5+c(seq(1,11,2),seq(2,10,2),seq(3,9,2),4,6,8,5,7,5,7,6,6))
    suit <- rev((0:23%%4)+1)
    df_c <- piecepack_coins(x = rep(2:13, 2),
                            y = rep(1:2, each = 12),
                            suit = suit,
                            rank = c(coins[2, ], coins[1, ]) + 1)
    df_p <- piecepack_pawns(x = c(1,14,14,1), y = c(2,2,1,1))
    if (first_move_needs_dice(coins)) {
        if (is.null(dice)) {
            dice <- random_dice()
        } else {
            dice <- process_ranks(dice)
        }
        df_d <- piecepack_dice(x = c(16,17,16,17), y = c(2,2,1,1),
                               suit = c(1,2,4,3), rank = dice)
        bind_rows(df_t, df_c, df_p, df_d)
    } else {
        bind_rows(df_t, df_c, df_p)
    }
}

first_move_needs_dice <- function(coins) {
    !((1 %in% coins[, c(1, 12)]) ||
      (2 %in% coins[, c(2, 11)]) ||
      (3 %in% coins[, c(3, 10)]) ||
      (4 %in% coins[, c(4, 9)]) ||
      (5 %in% coins[, c(5, 8)]))
}

random_fujisan_coins <- function() {
    coins <- integer(24)
    coins[which(1:24 %% 4 == 0)] <- sample(0:5)
    coins[which(1:24 %% 4 == 1)] <- sample(0:5)
    coins[which(1:24 %% 4 == 2)] <- sample(0:5)
    coins[which(1:24 %% 4 == 3)] <- sample(0:5)
    coins <- matrix(coins, nrow = 2, byrow = TRUE)
    coins
}


#' @rdname piecepack_games_original
#' @export
piecepack_galaxy_express <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_board <- piecepack_rectangular_board(nrows = 6, ncols = 8, y0 = 2,
                                            rank = rep(4:6, each = 4L), suit = rep(1:4, 3L))
    df_arms <- df_board[sample.int(12L, 6L), ]
    df_arms$x <- df_arms$x + sample(c(-0.5, 0.5), 6, replace = TRUE)
    df_arms$y <- df_arms$y + sample(c(-0.5, 0.5), 6, replace = TRUE)
    df_arms$piece_side <- c(rep_len("coin_back", 5L), "coin_face")
    df_arms$suit <- 4L
    df_arms$rank <- sample.int(6L)
    df_pawn <- df_arms[6, ] %>% mutate(piece_side = "pawn_face", rank = 1L)
    df_crowns <- piecepack_coins(x = 1, y = 1, suit = 3L, rank = sample.int(6L))
    if (df_arms$rank[6] == df_crowns$rank[6])
        df_crowns$rank <- df_crowns$rank[c(6, 1:5)]
    df_dice <- piecepack_dice(x = c(3, 4, 11, 12), y = c(1, 1, 4, 4),
                              rank = 1L, suit = c(3, 4, 1, 2))
    df_speed_tiles <- piecepack_tiles(x = 10.5, y = c(1.5, 5.5),
                                      rank = 2L, suit = c(2L, 1L))
    df_speed_coins <- piecepack_coins(
                         side = rep.int(rep(c("back", "face"), 2), c(4, 2, 4, 2)),
                         suit = rep.int(1:2, c(6L, 6L)),
                         rank = c(sample.int(6L), sample.int(6L)),
                         x = 12, y = rep.int(c(7, 6, 5, 3, 2, 1), c(4, 1, 1, 4, 1, 1)))
    bind_rows(df_board, df_arms, df_crowns, df_dice,
              df_speed_tiles, df_speed_coins, df_pawn)
}

#' @rdname piecepack_games_original
#' @export
piecepack_iceberg <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    sra <- process_tiles(tiles)
    piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                          x0 = 0.5, y0 = 0.5, side = "back")
}

#' @rdname piecepack_games_original
#' @export
piecepack_ice_floe <- function() {
    tiles <- "S2S3CaM2M3/S4S5AaM4M5/MnCnSnAn/A2A3MaC2C3/A4A5SaC4C5"
    sra <- process_tiles(tiles)
    piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                          x0 = 1.0, y0 = 1.0, side = "face")
}

#' @rdname piecepack_games_original
#' @export
piecepack_japan <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = "back",
                       x = 0.5 + c(rep(seq(1, by=4, length.out=4), each = 3),
                                   rep(seq(3, by=4, length.out=3), each = 4)),
                       y = 0.5 + c(rep(seq(2, by=2, length.out=3), 4),
                                   rep(seq(1, by=2, length.out=4), 3)))

    # data frame of possible coin coordinates
    xy <- xy_ll <- xy_ul <- xy_lr <- xy_ur <- df_tiles[, c("x", "y")]
    xy_ll$x <- xy$x - 0.5
    xy_ul$x <- xy$x - 0.5
    xy_lr$x <- xy$x + 0.5
    xy_ur$x <- xy$x + 0.5
    xy_ll$y <- xy$y - 0.5
    xy_ul$y <- xy$y + 0.5
    xy_lr$y <- xy$y - 0.5
    xy_ur$y <- xy$y + 0.5
    xy <- bind_rows(xy_ll, xy_ul, xy_lr, xy_ur)
    # find 24 random "non-orthogonal" coordinates
    xy_coins <- tibble()
    for (i in 1:24) {
        i_new <- sample.int(nrow(xy), 1L)
        xy_coins <- bind_rows(xy_coins, xy[i_new,])
        i_remove <- c(i_new,
                      which(xy$x == xy$x[i_new] & xy$y == xy$y[i_new] + 1),
                      which(xy$x == xy$x[i_new] & xy$y == xy$y[i_new] - 1),
                      which(xy$x == xy$x[i_new] + 1 & xy$y == xy$y[i_new]),
                      which(xy$x == xy$x[i_new] - 1 & xy$y == xy$y[i_new]))
        xy <- xy[-i_remove,]
    }
    df_coins <- piecepack_coins(side = "face", x = xy_coins$x, y = xy_coins$y)

    bind_rows(df_tiles, df_coins)
}

#' @rdname piecepack_games_original
#' @export
piecepack_lab_rats <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = "back", x = 1.5, y = 1.5)
    repeat {
        x0 <- 1.5
        y0 <- 1.5
        a0 <- 0
        for (i in seq.int(2, 24)) {
            die <- sample.int(6, 1)
            xo <- switch(die, 0, -2, -1, 0, 1, 2)
            yo <- switch(die, 2, 1, 2, 2, 2, 1)
            ao <- switch(die, 0, 90, 0, 0, 0, -90)
            a1 <- (a0 + ao) %% 360
            if (a0 == 0) {
                x1 <- x0 + xo
                y1 <- y0 + yo
            } else if (a0 == 90) {
                x1 <- x0 - yo
                y1 <- y0 + xo
            } else if (a0 == 180) {
                x1 <- x0 - xo
                y1 <- y0 - yo
            } else { # a0 == 270
                x1 <- x0 + yo
                y1 <- y0 - xo
            }
            # cat(i, die, xo, yo, ao, x1, y0, a1, "\n")
            df_tiles$x[i] <- x0 <- x1
            df_tiles$y[i] <- y0 <- y1
            df_tiles$angle[i] <- a0 <- a1
        }
        if (is_legal_labrats(df_tiles))
            break
    }
    if (a0 == 0) {
        px <- df_tiles$x[24] + 0.5 * c(-1, 1)
        py <- df_tiles$y[24] + 1.5
    } else if (a0 == 90) {
        px <- df_tiles$x[24] - 1.5
        py <- df_tiles$y[24] + 0.5 * c(-1, 1)
    } else if (a0 == 180) {
        px <- df_tiles$x[24] - 0.5 * c(-1, 1)
        py <- df_tiles$y[24] - 1.5
    } else { # a0 == 270
        px <- df_tiles$x[24] + 1.5
        py <- df_tiles$y[24] - 0.5 * c(-1, 1)
    }
    df_pawns <- piecepack_pawns(suit = 1:2, x = px, y = py, angle = a1)
    # "optimal" rat placement
    x <- df_tiles$x[which(df_tiles$x != 1.5)[1]]
    if (x < 1.5)
        df_rat <- piecepack_pawns(suit = 3L, angle = 0, x = 1, y = 1)
    else
        df_rat <- piecepack_pawns(suit = 3L, angle = 0, x = 2, y = 1)
    df_coins <- piecepack_coins(side = "face",
                                x = df_tiles$x[1] + rep(c(-0.5, 0.5, 1.5), each = 8),
                                y = df_tiles$y[1] + rep(c(-1.5, -1.5, -0.5), each = 8)) %>%
        slice_sample_piece()
    bind_rows(df_tiles, df_pawns, df_rat, df_coins)
}

is_legal_labrats <- function(df) {
    for (i in seq.int(2, 23)) {
        if (sum(sqrt((df$x[i] - df$x[-i])^2 + (df$y[i] - df$y[-i])^2) <= sqrt(5)) != 2)
            return(FALSE)
    }
    if (sum(sqrt((df$x[1] - df$x[-1])^2 + (df$y[1] - df$y[-1])^2) <= sqrt(5)) != 1)
        return(FALSE)
    if (sum(sqrt((df$x[24] - df$x[-24])^2 + (df$y[24] - df$y[-24])^2) <= sqrt(5)) != 1)
        return(FALSE)
    TRUE
}

#' @rdname piecepack_games_original
#' @export
piecepack_landlocked <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = rep(c("back", "face"), each = 12),
                       x = 0.5 + c(seq(3, 9, 2), rep(seq(1, 9, 2), 4)),
                       y = 0.5 + c(rep(9, 4), rep(seq(7, 1, -2), each = 5))) %>%
        slice_sample_piece(names = c("piece_side", "suit", "rank"))
    df_pawn <- piecepack_pawns(suit = 1L, x = 1, y = 9)
    bind_rows(df_tiles, df_pawn)
}

#' @rdname piecepack_games_original
#' @export
piecepack_ley_lines <- function() {
    piecepack_tiles(side = "back",
                    x = c(6,8,     7,9,   7,9,   3,5, 8,10, 2,4, 9,11, 2,4,13, 7,9,11, 13, 9,11, 7) - 0.5,
                    y = c(15,15, 13,13, 11,11, 10,10,  9,9, 8,8, 7,7,  6,6,6,  5,5,5,  4,  3,3,  2) - 0.5)
}

#' @rdname piecepack_games_original
#' @export
piecepack_mathrix <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_rectangular_board(nrows=4, ncols=6)
    df_coins <- piecepack_coins(side = "face",
                       rank = process_ranks(coins), suit = NA_integer_,
                       x = rep(1:6, 4), y = rep(4:1, each=6)) %>%
        fill_piece_suit()
    bind_rows(df_tiles, df_coins)
}

#' @rdname piecepack_games_original
#' @export
piecepack_pawns_crossing <- function(..., seed = NULL, n_players = 2L) {
    check_dots_empty()
    if (!is.null(seed)) withr::local_seed(seed)
    stopifnot(n_players >= 2L && n_players <= 4L)
    df_t <- piecepack_tiles(suit = rep(1:4, each = 4L), rank = rep(2:5, 4L),
                            x = rep(3 * 1:4, each = 4L) - 1.5,
                            y = rep(3 * 1:4, 4L) - 1.5,
                            angle = sample(c(0, 90, 180, 270), 16L, replace = TRUE)) %>%
        slice_sample_piece()
    df_d <- domino_tiles() %>%
        mutate(sr = paste0(.data$suit, .data$rank)) %>%
        filter(!(.data$sr %in% c("77", "76", "75"))) %>%
        select(-"sr") %>%
        slice_sample_piece()
    df_dv <- df_d %>% slice(seq.int(13L)) %>%
        mutate(angle = sample(c(0, 180), 13L, replace = TRUE),
               x = c(rep(3 * 1:3, each = 4L), 13),
               y = c(rep(3 * 1:4, 3L) - 1.5, 6))
    df_dh <- df_d %>% slice(-seq.int(13L)) %>%
        mutate(angle = sample(c(90, 270), 12L, replace = TRUE),
               x = c(rep(3 * 1:4, 3L) - 1.5),
               y = c(rep(3 * 1:3, 4L)))
    if (n_players == 2L) {
        df_cxy <- filter(df_t, .data$rank == 2L) %>% arrange(.data$suit)
        df_c <- piecepack_coins(side = "back",
                                suit = rep(1:4, each = 3L),
                                rank = as.integer(replicate(4L, sample(3:5))),
                                x = rep(df_cxy$x, each = 3L) + rep(c(0.5, -0.5, 0.5), 4L),
                                y = rep(df_cxy$y, each = 3L) + rep(c(0.5, -0.5, -0.5), 4L),
                                angle = as.double(replicate(4L, sample(c(0, 90, 180, 270), 3L))))
    } else if (n_players == 3L) {
        df_cxy <- filter(df_t, .data$rank == 2L & .data$suit != 2L) %>% arrange(.data$suit)
        df_c <- piecepack_coins(side = "back",
                                suit = rep(c(1L, 3L, 4L), each = 4L),
                                rank = as.integer(replicate(3L, sample(c(1L, 3:5)))),
                                x = rep(df_cxy$x, each = 4L) + rep(c(0.5, -0.5, 0.5, -0.5), 3L),
                                y = rep(df_cxy$y, each = 4L) + rep(c(-0.5, 0.5, 0.5, -0.5), 3L),
                                angle = as.double(replicate(3L, sample(c(0, 90, 180, 270)))))
    } else {
        df_cxy <- filter(df_t, .data$rank == 2L) %>% arrange(.data$suit)
        df_c <- piecepack_coins(side = "back",
                                suit = rep(1:4, each = 4L),
                                rank = as.integer(replicate(4L, sample(c(1L, 3:5)))),
                                x = rep(df_cxy$x, each = 4L) + rep(c(0.5, -0.5), 8L),
                                y = rep(df_cxy$y, each = 4L) + rep(c(-0.5, 0.5, 0.5, -0.5), 4L),
                                angle = as.double(replicate(4L, sample(c(0, 90, 180, 270)))))
    }
    if (n_players == 2L) {
        df_p <- piecepack_pawns(x = c(4, 5, 8, 7), y = c(13, 13, -1, -1),
                                angle = c(180, 180, 0, 0))
        df_d <- piecepack_dice(x = c(1, 2, 11, 10), y = c(13, 13, -1, -1), rank = random_dice(),
                                angle = c(180, 180, 0, 0))
    } else {
        df_p <- piecepack_pawns(x = c(3, 13, 9, -1), y = c(13, 9, -1, 3),
                                angle = c(180, 90, 0, 270))
        df_d <- piecepack_dice(x = c(1, 13, 11, -1), y = c(13, 11, -1, 1), rank = random_dice(),
                                angle = c(180, 90, 0, 270))
        if (n_players == 3L) {
            df_p <- filter(df_p, .data$suit != 2L)
            df_d <- filter(df_d, .data$suit != 2L)
        }
    }
    bind_rows(df_t, df_dv, df_dh, df_c, df_p, df_d)
}

#' @rdname piecepack_games_original
#' @export
piecepack_piecepackman <- function(seed = NULL, variant = "Roundabout") {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = "back",
                   x = 0.5 + c(rep(seq(1, 9, 2), 4), seq(2, 8, 2)),
                   y = 0.5 + c(rep(c(1, 3, 7, 9), each = 5), rep(5, 4)))
    df_p <- switch(variant,
                   Roundabout = piecepack_roundabout(seed),
                   abort(paste("Can't handle Piecepackman variant ", variant), class = "board_setup"))
    p_xy <- filter(df_p, !grepl("matchstick", .data$piece_side)) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select("x_y")
    x_y_omit <- c(p_xy$x_y, "1_5", "1_6", "10_5", "10_6")
    df_x_y <- expand.grid(x = 1:10, y = 1:10, stringsAsFactors = FALSE) %>%
            mutate(x_y = paste(.data$x, .data$y, sep = "_")) %>%
            select("x_y")
    x_y <- setdiff(df_x_y$x_y, x_y_omit)

    # Set seed again so backwards compatible with new sampling in `piecepack_roundabout()`
    if (!is.null(seed)) withr::local_seed(seed)
    x_y_nulls <- str_split(sample(x_y, 24), "_", simplify = TRUE)
    df_n <- piecepack_matchsticks(rank = 1L,
                                  x = as.numeric(x_y_nulls[, 1]),
                                  y = as.numeric(x_y_nulls[, 2]))

    bind_rows(df_tiles, df_p, df_n)
}

piecepack_roundabout <- function(seed = NULL) {
    df_c <- piecepack_coins(side = "back",
                   x = c(1, 1, 10, 10), y = c(1, 10, 10, 1),
                   suit = c(4, 3, 2, 1),
                   rank = c(sample(setdiff(2:6, 2L), 1L), # not ace of arms
                            sample(setdiff(2:6, 4L), 1L), # not 3 of crowns
                            sample(setdiff(2:6, 5L), 1L), # not 4 of moons
                            sample(setdiff(2:6, 3L), 1L))) # not 2 of suns
    df_p <- piecepack_pawns(x = c(5, 5, 6, 6),
                            y = c(5, 6, 6, 5),
                            suit = c(4, 2, 3, 1))
    df_n <- piecepack_coins(x = 6, y = 4, rank = 1L, suit = 1L)
    df_mav <- piecepack_matchsticks(rank = 2L,
                     x = 0.5 + c(4, 4, 6, 6, 5, 5),
                     y = c(6, 8, 8, 5, 2:3),
                     suit = c(2L, 2L, 2L, 1L, 4L, 4L))
    df_mah <- piecepack_matchsticks(
                     rank = 2L, angle = 90,
                     x = c(3, 8, 5, 6, 3, 8),
                     y = 0.5 + c(9, 9, 4, 4, 2, 2),
                     suit = c(2, 2, 1, 1, 4, 4))
    df_m3v <- piecepack_matchsticks(rank = 4L,
                     x = 0.5 + c(1, 9, 5, 1, 9),
                     y = 0.5 + c(2, 2, 8, 8, 8),
                     suit = c(4, 4, 2, 3, 3))
    df_m3h <- piecepack_matchsticks(
                     rank = 4L, angle = 90,
                     x = 0.5 + c(3, 4, 6, 7, 5, 2, 2, 8, 8, 2, 2, 8, 8, 3, 7, 4, 6, 3, 7),
                     y = 0.5 + c(8, 9, 9, 8, 6, 6, 7, 6, 7, 4, 5, 4, 5, 3, 3, 2, 2, 1, 1),
                     suit = c(rep(2, 5), rep(3, 4), rep(1, 6), rep(4, 4)))

    bind_rows(df_c, df_p, df_n, df_mav, df_mah, df_m3v, df_m3h)
}


#' @rdname piecepack_games_original
#' @export
piecepack_one_man_thrag <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_board <- piecepack_rectangular_board(4, 4, x0 = 1, y0 = 1, suit = 1:4, rank = 1L)
    df_tiles <- piecepack_tiles(side = "back",
                                x = rep(6 + 2 * c(0, 1, 2, 3.5), each = 5),
                                y = rep(3, each = 20),
                                suit = rep(c(1L, 3L, 4L, 2L), each = 5),
                                rank = 1L + c(sample.int(5), sample.int(5), sample.int(5), sample.int(5)))
    df_dice <- piecepack_dice(x = 1:4, y = 6, suit = c(1L, 3L, 4L, 2L), rank = random_dice())
    df_coins <- piecepack_coins(side = "back",
                       x = rep.int(6 + 2 * c(0, 1, 2, 3.5), c(6, 6, 6, 3)),
                       y = 5, suit = rep.int(c(1L, 3L, 4L, 2L), c(6L, 6L, 6L, 3L)),
                       rank = c(sample.int(6), sample.int(6), sample.int(6), c(2, 4, 6)[sample.int(3)]))
    df_pawns <- piecepack_pawns(x = c(1, 6, 8, 10), y = c(4, 6, 6, 6),
                                suit = c(2L, 1L, 3L, 4L))
    df_health <- piecepack_coins(side = "face", x = 13, y = 6:8,
                                 suit = 2L, rank = c(1L, 3L, 5L))
    bind_rows(df_board, df_tiles, df_dice, df_coins, df_pawns, df_health)

}

#' @rdname piecepack_games_original
#' @export
piecepack_pass_the_food <- function() {
    piecepack_tiles(side = "face",
                    rank = rep(c(1, 3:6, 2), 4),
                    x = rep(2 * 1:4 - 0.5, each = 6),
                    y = rep(2 * 1:6 - 0.5, 4))
}

#' @rdname piecepack_games_original
#' @export
piecepack_klondike <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_tiles(side = c(rep_len("back", 15L),
                                         rep_len("face", 6L),
                                         rep_len("back", 3L)),
                                x = c(seq(4, 12, 2), seq(6, 12, 2), seq(8, 12, 2),
                                      10, 12, 12, seq(2, 12, 2), rep(2, 3)),
                                y = c(rep(2, 21), rep(6, 3)),
                                suit = rep.int(1:4, 6L), # reverse-compatible random
                                rank = rep(1:6, each = 4L)) %>%
        slice_sample_piece()
    df_tiles
}

#' @rdname piecepack_games_original
#' @export
piecepack_piece_gaps <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df <- piecepack_tiles(side = "face",
                          x = rep.int(seq.int(2L, 12L, 2L), 4L),
                          y = rep(seq.int(2L, 8L, 2L), each = 6L),
                          suit = rep.int(1:4, 6L), # reverse-compatible random
                          rank = rep(1:6, each = 4L)) %>%
        slice_sample_piece()
    df[-which(df$rank == 1L), ]
}

#' @rdname piecepack_games_original
#' @export
piecepack_piece_packing_pirates <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    new <- "0_0"
    tiles_xy <- new
    impossible <- c()
    possible <- c()
    while (length(tiles_xy) < 24L) {
        impossible <- union(impossible, new_impossible(new))
        possible <- union(possible, new_possible(new))
        possible <- setdiff(possible, impossible)
        new <- sample(possible, 1L)
        tiles_xy <- c(tiles_xy, new)
    }
    xy <- str_split(tiles_xy, "_")
    x <- as.numeric(sapply(xy, function(x) x[1]))
    y <- as.numeric(sapply(xy, function(x) x[2]))
    df_tiles <- piecepack_tiles(side = "back",
                                x = x - min(x) + 1.5,
                                y = y - min(y) + 1.5,
                                suit = rep.int(1:4, 6L), # reverse-compatible random
                                rank = rep(1:6, each = 4L),
                                angle = sample(c(0, 90, 180, 270), 24L, replace=TRUE)) %>%
        slice_sample_piece()
    df_tiles
}

p_ <- function(...) paste(..., sep = "_")
new_impossible <- function(x_y) {
    xy <- as.integer(str_split(x_y, "_")[[1]])
    c(p_(xy[1] - 1L, xy[2] + -1:1),
      p_(xy[1], xy[2] + -1:1),
      p_(xy[1] + 1L, xy[2] + -1:1))
}
new_possible <- function(x_y) {
    xy <- as.integer(str_split(x_y, "_")[[1]])
    c(p_(xy[1] - 2L, xy[2] + -1:1),
      p_(xy[1] - 1L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 0L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 1L, xy[2] + c(-2L, 2L)),
      p_(xy[1] + 2L, xy[2] + -1:1))
}

#' @rdname piecepack_games_original
#' @export
piecepack_plans_of_action <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_rectangular_board(nrows=8L, ncols=8L)
    if (is.null(coins)) {
        suits <- sample(rep.int(1:4, 6L), 24L)
    } else {
        suits <- process_suits(coins)
    }
    df_coins <- piecepack_coins(side = "back", suit = suits,
                                x = rep.int(2:7, 4L), y = rep(6:3, each=6L),
                                rank = NA_integer_) %>%
        fill_piece_rank()
    bind_rows(df_tiles, df_coins)
}

#' @rdname piecepack_games_original
#' @export
piecepack_relativity <- function(seed = NULL, coins = NULL) {
    df_tiles <- piecepack_rectangular_board(nrows=4, ncols=6)
    if (is.null(coins)) {
        if (!is.null(seed)) withr::local_seed(seed)
        ranks <- c(sample.int(6L), sample.int(6L), sample.int(6L), sample.int(6L))
        while (should_resample_relativity(ranks)) {
            ranks <- c(sample.int(6L), sample.int(6L), sample.int(6L), sample.int(6L))
        }
        ranks <- ranks[c(1:3, 7:9, 4:6, 10:12, 13:15, 19:21, 16:18, 22:24)]
    } else {
        ranks <- process_ranks(coins)
    }
    df_coins <- piecepack_coins(side = "face", rank = ranks,
                                x = rep(1:6, 4), y = rep(4:1, each=6),
                                suit = rep(c(1,2,1,2,4,3,4,3), each=3))
    bind_rows(df_tiles, df_coins)
}
should_resample_relativity <- function(coins) {
    all(diff(c(coins[6], coins[4], coins[3], coins[1])) == 0)
}

#' @rdname piecepack_games_original
#' @export
piecepack_san_andreas <- function() {
    x <- 0.5+c(rep(c(1,3,5), 3), 2,4,6, 3,5,7, 4,6,8, 5,7,9, 7,9)
    y <- 0.5+c(rep(c(15,13,11,9,7,5,3), each=3), 1, 1)
    piecepack_tiles(side = "back", x = x, y = y, length.out = 23L)
}

#' @rdname piecepack_games_original
#' @export
piecepack_sarcophagus <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_sarcophagus <- piecepack_tiles(side = "back",
                             suit = c(4, 3, 4:1),
                             rank = c(1, 1, rep(6, 4)),
                             x = rep(c(6.5, 5.5, 7.5), 2),
                             y = rep(c(3.5, 1.5, 1.5), 2)) %>%
        slice_sample_piece()
    df_other <- piecepack_tiles(side = "back",
                       suit = c(2:1, rep(4:1, each = 4)),
                       rank = c(1, 1, rep(2:5, 4)),
                       x = c(6.5,  5.5,7.5,  4.5,6.5,8.5,  seq(3.5,9.5,2),
                             2.5,4.5,8.5,10.5,  1.5,3.5,9.5,11.5),
                       y = rep.int(seq(11.5,1.5,-2), c(1, 2, 3, 4, 4, 4))) %>%
        slice_sample_piece()
    bind_rows(df_sarcophagus, df_other)
}

#' @rdname piecepack_games_original
#' @export
piecepack_shopping_mall <- function(seed = NULL, cfg2 = "go") {
    if (!is.null(seed)) withr::local_seed(seed)
    i_tf <- sample.int(20)
    df_tf <- tibble(piece_side = "tile_face",
                    suit = rep(1:4, 5L)[i_tf],
                    rank = rep(2:6, 4L)[i_tf],
                    cfg = "piecepack",
                    x = 3 + c(1, 5, 9,11,13,
                              5,
                              1, 5, 9, 13,
                              1,3, 7,9, 13,
                              1,3, 7, 11,13),
                    y = 3 + rep.int(c(13, 11, 9, 5, 1), c(5, 1, 4, 5, 5)),
                    angle = sample(c(0, 90, 180, 270), 20, replace = TRUE))
    df_pennies <- tibble(piece_side = "bit_face",
                         suit = 1L, rank = 1L, cfg = cfg2,
                         x = 3 + c(7, 3, 9, 3, 11, 1, 7, 1, 7, 11, 5, 15, 1),
                         y = 1 + c(17, 15, 13, 11, 11, 9, 9, 5, 5, 5, 3, 3, 1),
                         angle = 0)
    df_nickels <- tibble(piece_side = "bit_face",
                         suit = 2L, rank = 1L, cfg = cfg2,
                         x = 1 + c(1, 17, 11),
                         y = 1 + c(13, 13, 1),
                         angle = 0)
    bind_rows(df_tf, df_pennies, df_nickels)
}

#' @rdname piecepack_games_original
#' @export
piecepack_skyscrapers <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    sra <- process_tiles(tiles)
    df_tiles <- piecepack_donut_board(suit = sra$suit, rank = sra$rank, angle = sra$angle,
                                      x0 = 1.0, y0 = 1.0, side = "face")
    df_pawn <- filter(df_tiles, .data$rank == 1)
    df_pawn <- mutate(df_pawn, piece_side = "pawn_face",
                      x = .data$x + 0.5, y = .data$y - 0.5)
    bind_rows(df_tiles, df_pawn)
}

#' @rdname piecepack_games_original
#' @export
piecepack_slides_of_action <- function() {
    df_tiles <- piecepack_rectangular_board(4, 4)
    df_coins <- piecepack_coins(side = "back",
                       suit = rep(c(1,3,4), each = 5),
                       rank = rep(1:5, 3),
                       x = c(3,1,4,2,3, 2,3,1,4,2, 1,4,2,3,1),
                       y = c(4,3,3,2,1, 4,3,2,2,1, 4,4,3,2,1))
    bind_rows(df_tiles, df_coins)
}

# 2 players = 24 tiles, 24 coins => 2 towers, 11 tiles each, 12 coins each
# 3 players = 48 tiles, 48 coins => 2 towers, 15 tiles each + 1 extra tile, 16 coins each
# 4 players = 48 tiles, 48 coins => 3 towers, 11 tiles each + 1 extra tile, 12 coins each
# 5 players = 72 tiles, 72 coins => 4 towers, 13 tiles each + 3 extra tiles, 16 coins each + 2 coins
# 6 players = 72 tiles, 72 coins => 5 towers, 11 tiles each + 1 extra tile, 12 coins each

#' @rdname piecepack_games_original
#' @export
piecepack_speedy_towers <- function(n_players = 2, seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    stopifnot(n_players >= 2, n_players <= 6)
    if (n_players == 2) {
        df_tiles <- piecepack_tiles(side = "back",
                           suit = rep.int(1:4, 6L),
                           rank = rep(1:6, each = 4)) %>%
            slice_sample_piece() %>%
            mutate(x = c(seq.int(2, by = 2, length.out = 11),
                         16, 10,
                         seq.int(24, by = -2, length.out = 11)),
                   y = c(rep_len(4, 11), 8, 8, rep_len(12, 11)),
                   angle = c(rep_len(0, 12), rep_len(180, 12)))
        df_coins <- piecepack_coins(side = "back",
                           suit = rep.int(1:4, 6L),
                           rank = rep(1:6, each = 4)) %>%
            slice_sample_piece() %>%
            slice(c(order(.data$suit[1:12]),
                    12 + order(.data$suit[13:24]))) %>%
            mutate(x = c(seq(2, by = 2, length.out = 12),
                         seq(24, by = -2, length.out = 12)),
                   y = c(rep(2, 12), rep(14, 12)),
                   angle = c(rep_len(0, 12), rep_len(180, 12)))
        df_pawns <- piecepack_pawns(suit = c(3, 1),
                                    x = c(24, 2), y = c(4, 12),
                                    angle = c(0, 180))
        bind_rows(df_tiles, df_coins, df_pawns)
    } else {
        stop("We haven't been programmed for this case yet")
    }
}

#' @rdname piecepack_games_original
#' @export
piecepack_steppin_stones <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles_faces <- piecepack_tiles(side = "face",
                             suit = 1:4, rank = 2L,
                             x = c(1.5, 7.5, 7.5, 1.5),
                             y = c(7.5, 7.5, 1.5, 1.5),
                             angle = c(180, 90, 0, -90))
    df_tiles_backs <- piecepack_tiles(side = "back",
                             suit = rep(1:4, each = 3),
                             rank = rep(3:5, 4),
                             x = c(3.5, 1.5, 3.5,  5.5, 5.5, 7.5,
                                   5.5, 5.5, 7.5, 3.5, 3.5, 1.5),
                             y = c(7.5, 5.5, 5.5,  7.5, 5.5, 5.5,
                                   3.5, 1.5, 3.5, 3.5, 1.5, 3.5))
    df_coins <- piecepack_coins(suit = rep(1:4, each = 5),
                       rank = rep(c(1, 3:6), 4),
                       x = c(3, 2, 1, 3, 3,  6, 7, 8, 6, 6,
                             6, 7, 8, 6, 6,  3, 2, 1, 3, 3),
                       y = c(6, 6, 6, 7, 8,  6, 6, 6, 7, 8,
                             3, 3, 3, 2, 1,  3, 3, 3, 2, 1),
                       angle = rep(c(180, 90, 0, -90), each = 5))
    df_pawns <- piecepack_pawns(
                       x = c(5, 4, 4, 5), y = c(4, 4, 5, 5),
                       angle = c(180, 90, 0, -90))
    bind_rows(df_tiles_faces, df_tiles_backs, df_coins, df_pawns)
}

#' @rdname piecepack_games_original
#' @export
piecepack_the_in_crowd <- function() {
    df_t1 <- piecepack_rectangular_board(6L, 6L)
    df_t2 <- piecepack_rectangular_board(4L, 4L, x0 = 2, y0 = 2, rank = 4L)
    df_t3 <- piecepack_tiles(side="back", x=3.5, y=3.5, suit = 2L, rank = 3L)
    bind_rows(df_t1, df_t2, df_t3)
}

#' @rdname piecepack_games_original
#' @export
piecepack_the_magic_bag <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    piecepack_tiles(x = 0.5 + c(rep(1, 9), rep(3, 7), rep(5, 5), rep(7, 3)),
                    y = 0.5 + c(seq(17, 1, -2), seq(13, 1, -2), seq(9, 1, -2), seq(5, 1, -2))) %>%
        slice_sample_piece()
}

#' @rdname piecepack_games_original
#' @export
piecepack_the_penguin_game <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- piecepack_rectangular_board(4, 4)
    df_coins <- piecepack_coins(side = "back",
                                x = rep.int(1:4, 6L),
                                y = c(rep(4:1, each = 4), 4:1, 1:4)) %>%
        slice_sample_piece()
    bind_rows(df_tiles, df_coins)
}

#' @rdname piecepack_games_original
#' @export
piecepack_tower_of_babel <- function(seed = NULL, tiles = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(tiles)) {
        df <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
    } else {
        df <- process_tiles(tiles)
    }
    df <- mutate(df, piece_side = "tile_back", x = 2, y = 4,
                 cfg = "piecepack", angle = 0) %>%
        select_piece()
    df[24L, "y"] <- 2
    df[24L, "piece_side"] <- "tile_face"
    attr(df, "scale_factor") <- 2
    df
}

#' @rdname piecepack_games_original
#' @export
piecepack_accordion <- piecepack_tower_of_babel

#' @rdname piecepack_games_original
#' @export
piecepack_tracers <- function() {
    df_tiles <- piecepack_rectangular_board(8, 8)
    df_pawns <- piecepack_pawns(suit = 1:2, x = c(1, 8),
                                y = c(1, 8), angle = c(0, 180))
    bind_rows(df_tiles, df_pawns)
}

#' @rdname piecepack_games_original
#' @export
piecepack_triactor <- function(seed = NULL, cfg2 = "playing_cards_expansion") {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tb <- piecepack_tiles(side = "back",
                    x = 0.5+rep(c(seq(5,15,2),1,2,18,19),2),
                    y = 0.5+c(rep(1,6),5,3,3,5,rep(11,6),7,9,9,7),
                    suit = rep(1:4, each = 5L),
                    rank = rep(2:6, 4L),
                    angle = rep(c(rep(0,7),90,90,0),2))
    df_tf <- piecepack_tiles(
                    x = 0.5+c(3,17,17,3), y = 0.5+c(11,11,1,1),
                    suit = 1:4, rank = 1L)
    df_c1 <- piecepack_coins(side = "back",
                    suit = 1:4, rank = sample.int(6L, 4L, replace = TRUE),
                    x = 0.5+c(5,15,15,5), y = 0.5+c(11,11,1,1))
    df_c2 <- piecepack_coins(side = "back", cfg = cfg2,
                    suit = 1:4, rank = sample.int(6L, 4L, replace = TRUE),
                    x = 0.5+c(2,18,18,2), y = 0.5+c(9,9,3,3))
    df_p <- piecepack_pawns(cfg = rep(c("piecepack", cfg2), each = 4),
                   x = 10.5, y = 0.5+0:7, angle = 90, suit = rep.int(1:4, 2L))
    bind_rows(df_tb, df_tf, df_c1, df_c2, df_p)
}

#' @rdname piecepack_games_original
#' @export
piecepack_tula <- function(seed = NULL, tiles = NULL,
                    variant = c("Original", "Variant 1", "Variant 2", "Variant 3", "Variant 4")) {
    variant <- match.arg(variant)
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(tiles)) {
        if (variant %in% c("Original", "Variant 1"))
            df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[sample.int(24L), ]
        else
            df_tsr <- expand.grid(suit = 1:4, rank = 1:6)[c(sample.int(24L), sample.int(24L)), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    df_txy <- switch(variant,
                     Original = tibble(piece_side = c(rep("tile_back", 23), "tile_face"),
                                       x = c(rep(seq(1,7,2), 3), 4,
                                             rep(seq(2,6,2), 2), 4,
                                             3, 5, 4, 4),
                                       y = c(rep(seq(7,3,-2), each=4), 1,
                                             rep(c(6,4), each=3), 2,
                                             5, 5, 3, 4)),
                     `Variant 1` = tibble(piece_side = c(rep("tile_back", 21),
                                                         "tile_face", "tile_back", "tile_face"),
                                          x = c(seq(2,8,2), seq(1,9,2), seq(2,8,2),
                                                rep(seq(2,8,2), 2),
                                                seq(3,7,2)),
                                          y = c(rep.int(seq(5,1,-2), c(4, 5, 4)),
                                                rep(c(4,2), each=4),
                                                rep(3,3))),
                     `Variant 2` = tibble(piece_side = c(rep("tile_back", 28),
                                                         "tile_face", rep("tile_back",4), "tile_face", rep("tile_back",4), "tile_face", rep("tile_back",4), "tile_face",
                                                         rep("tile_face", 4)),
                                          x = c(6,8, seq(3,11,2), rep(seq(1,13,2),2), seq(3,11,2), 6,8,
                                                7, seq(4,10,2), seq(2,12,2), seq(4,10,2), 7,
                                                5,9,5,9),
                                          y = c(rep.int(seq(11,1,-2), c(2,5,7,7,5,2)),
                                                rep.int(seq(10,2,-2), c(1,4,6,4,1)),
                                                7,7,5,5)),
                     `Variant 3` = tibble(piece_side = c(rep("tile_back", 20),
                                                         rep("tile_back", 15),
                                                         "tile_face", rep("tile_back", 8), "tile_face",
                                                         "tile_face", "tile_back", "tile_face"),
                                          x = c(5,7, seq(2,10,2), seq(1,11,2), seq(2,10,2), 5,7,
                                                6, seq(3,9,2), seq(2,10,2), seq(3,9,2), 6,
                                                6, seq(3,9,2), seq(3,9,2), 6,
                                                4,6,8),
                                          y = c(rep.int(seq(9,1,-2), c(2,5,6,5,2)),
                                                rep.int(seq(9,1,-2), c(1,4,5,4,1)),
                                                rep.int(seq(8,2,-2), c(1,4,4,1)),
                                                rep(5,3))),
                     `Variant 4` = tibble(piece_side = c(rep("tile_back", 24),
                                                         rep("tile_back",4), "tile_face", rep("tile_back",5),"tile_face",rep("tile_back",4),
                                                         rep(c("tile_face","tile_back","tile_back","tile_face"),2),
                                                         "tile_face"),
                                          x = c(seq(4,12,2), seq(3,13,2), 1, 15, seq(3,13,2), seq(4,12,2),
                                                seq(5,11,2), seq(2,14,2), seq(5,11,2),
                                                seq(5,11,2), seq(5,11,2),
                                                8),
                                          y = c(rep.int(c(7,5,4,3,1), c(5, 6, 2, 6, 5)),
                                                rep.int(c(6,4,2), c(4, 7, 4)),
                                                rep.int(c(5,3), c(4, 4)),
                                                4)),
                     abort(paste("Can't handle Tula variant ", variant), class = "board_setup"))
    bind_cols(df_txy, df_tsr) %>%
        mutate(cfg = "piecepack", angle = 0) %>%
        select_piece()
}


#' @rdname piecepack_games_original
#' @export
piecepack_wormholes <- function() {
    df_tiles <- piecepack_tiles(side = "back", length.out = 23L,
                       x = -0.5 + 2*c(1,2, 2,3, 2,3,4, 3,4,5, 2,3,4, 1,2,3, 2,3,4, 3,4, 4,5),
                       y = -0.5 + 2*c(9,9, 8,8, 7,7,7, 6,6,6, 5,5,5, 4,4,4, 3,3,3, 2,2, 1,1))
    df_pawns <- piecepack_pawns(x = c(10,10,1,1), y = c(1,2,17,18))
    bind_rows(df_tiles, df_pawns)
}
