#' Setups for other traditional games playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_traditional())` other traditional games playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various other traditional games:
#'
#' `r man_markdown_table(piecepack_games_traditional())`
#'
#' @param has_matchsticks Has matchsticks
#' @param has_subpack Has a piecepack subpack
#' @param die_width Width of dice
#' @rdname piecepack_games_traditional
#' @name piecepack_games_traditional
#' @return `r return_df()`
NULL

piecepack_games_traditional <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Alquerque AKA Qirkat"
            , "``piecepack_alquerque()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Alquerque"
            , "Awithlaknannai Mosona"
            , "``piecepack_awithlaknannai_mosona()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Awithlaknannai_Mosona"
            , "Backgammon"
            , "``piecepack_backgammon()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Backgammon"
            , "Bagh-chal"
            , "``piecepack_baghchal()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Bagh_Chal"
            , "Brandubh AKA Brandub"
            , "``piecepack_brandubh()``"
            , NA_character_
            , "http://www.cyningstan.com/game/125/brandub"
            , "Cribbage"
            , "``piecepack_cribbage()`` aka ``piecepack_cribbage_board()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Cribbage"
            , "Four Field Kono"
            , "``piecepack_four_field_kono()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/FourFieldKono"
            , "Jul-Gonu"
            , "``piecepack_julgonu()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/JulGonu"
            , "Ludo"
            , "``piecepack_ludo()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Ludo_(board_game)"
            , "Nine Men's Morris"
            , "``piecepack_nine_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Nine_men%27s_morris"
            , "Salta"
            , "``piecepack_salta()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Salta_(game)"
            , "Tablut"
            , "``piecepack_tablut()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Tablut"
            )
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_alquerque <- function(has_matchsticks = FALSE) {
    df_t <- piecepack_rectangular_board(5L, 5L)
    df_c <- piecepack_coins(side = "back",
                   suit = c(rep(1:2, 6), rep(3:4, 6)),
                   rank = rep(c(1:6, 1:6), each=2),
                   x = c(1:5, 1:5, 1:2, 4:5, 1:5, 1:5),
                   y = c(rep(5:4, each=5), rep(3, 4), rep(2:1, each=5)),
                   angle = rep(c(180, 0), each = 12))
    if (has_matchsticks) {
        df_m <- piecepack_matchsticks(side = "back",
                       suit = rep(1:4, each=4),
                       rank = 2,
                       x = 0.5 + c(1,2,2,1, 3,4,4,3, 3,4,4,3, 1,2,2,1),
                       y = 0.5 + c(4,4,3,3, 4,4,3,3, 2,2,1,1, 2,2,1,1),
                       angle = rep(c(45, -45, -135, 135), 4))
        df <- bind_rows(df_t, df_m, df_c)
    } else {
        df <- bind_rows(df_t, df_c)
    }
    df
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_awithlaknannai_mosona <- function(has_matchsticks = FALSE) {
    df_t <- piecepack_tiles(side = "face",
                   suit = rep.int(1:4, 2L),
                   rank = rep(1:2, each=4L),
                   x = sqrt(2) + sqrt(2) * seq(1, by=2, length.out=8),
                   y = 2 * sqrt(2),
                   angle = rep(c(45, -45), each=4))
    df_c <- piecepack_coins(side = "back",
                   suit = c(rep.int(1:2, 6L), rep.int(3:4, 6L)),
                   rank = rep(c(1:6, 1:6), each=2),
                   x = sqrt(2) * c(seq(2, by=2, length.out=8),
                                   seq(17, by=-2, length.out=4),
                                   seq(1, by=2, length.out=4),
                                   seq(2, by=2, length.out=8)),
                   y = sqrt(2) * c(rep(3:1, each=8)),
                   angle = rep(c(180,0), each = 12))
    if (has_matchsticks) {
        df_m1 <- piecepack_matchsticks(side = "back",
                        suit = c(rep(1:2, length.out=7), rep(3:4, length.out=7)),
                        rank = 5L, angle=90,
                        x = sqrt(2) * rep(seq(3, by=2, length.out=7), 2),
                        y = sqrt(2) * rep(c(3, 1), each=7))
        df_m2 <- piecepack_matchsticks(side = "back",
                        suit = rep(1:4, 2), rank = 4L, angle=90,
                        x = sqrt(2) * seq(2, by=2, length.out=8),
                        y = sqrt(2) * rep(2, each=8))
        bind_rows(df_t, df_m1, df_m2, df_c)
    } else {
        bind_rows(df_t, df_c)
    }
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_backgammon <- function() {
    y_top <- 5
    y_bot <- 1
    x_1 <- 26-2+1
    x_6 <- 26-12+1
    x_12 <- 1
    x_08 <- 5*2-1

    # tiles
    df_t <- piecepack_tiles(side = "face",
                 suit = c(3+1:6%%2, 3 + (1:6+1)%%2, 1 + (1:6+1)%%2, 1 + (1:6+0)%%2),
                 rank = rep(c(2:6, 1), 4),
                 x = c(27-2*1:6, 13-2*1:6, 27-2*1:6, 13-2*1:6),
                 y = rep(c(y_bot, y_bot, y_top, y_top), each = 6),
                 angle = rep(c(0, 0, 180, 180), each = 6))

    # coins
    df_c1 <- tribble(~piece_side, ~x, ~y, ~suit,
                     "coin_back", x_6 +0.5, y_bot+0.5, 4L,
                     "coin_back", x_6 +0.5, y_bot-0.5, 4L,
                     "coin_back", x_6 -0.5, y_bot+0.5, 4L,
                     "coin_back", x_6 -0.5, y_bot-0.5, 4L,
                     "coin_back", x_12+0.5, y_top+0.5, 3L,
                     "coin_back", x_12+0.5, y_top-0.5, 3L,
                     "coin_back", x_12-0.5, y_top+0.5, 3L,
                     "coin_back", x_12-0.5, y_top-0.5, 3L,
                     "coin_back", x_12-0.0, y_top-0.0, 3L,
                     "coin_back", x_08+0.5, y_bot-0.5, 4L,
                     "coin_back", x_08-0.5, y_bot+0.5, 4L,
                     "coin_back", x_08+0.5, y_bot+0.5, 3L)
    df_c1$angle <- 0
    df_c2 <- tribble(~piece_side, ~x, ~y, ~suit,
                     "coin_back", x_6 +0.5, y_top+0.5, 1L,
                     "coin_back", x_6 +0.5, y_top-0.5, 1L,
                     "coin_back", x_6 -0.5, y_top+0.5, 1L,
                     "coin_back", x_6 -0.5, y_top-0.5, 1L,
                     "coin_back", x_12+0.5, y_bot+0.5, 2L,
                     "coin_back", x_12+0.5, y_bot-0.5, 2L,
                     "coin_back", x_12-0.5, y_bot+0.5, 2L,
                     "coin_back", x_12-0.5, y_bot-0.5, 2L,
                     "coin_back", x_12-0.0, y_bot-0.0, 2L,
                     "coin_back", x_08+0.5, y_top-0.5, 1L,
                     "coin_back", x_08-0.5, y_top+0.5, 1L,
                     "coin_back", x_08-0.5, y_top-0.5, 2L)
    df_c2$angle <- 180
    df_c <- bind_rows(df_c1, df_c2) %>%
        mutate(cfg = "piecepack", rank = NA_integer_) %>%
        fill_piece_rank()

    # pawns
    df_p <- piecepack_pawns(suit = 4:1,
                            x = x_1+c(-0.5,0.5,-0.5,0.5),
                            y = c(y_top+0.5,y_top-0.5, y_bot+0.5, y_bot-0.5),
                            angle = c(0, 0, 180, 180))

    # dice
    df_d <- piecepack_dice(suit = c(4,1,3,2), rank = c(1,1,2,2),
                           x = c(x_6, x_6, 6.5-0.5, 6.5+0.5),
                           y = c(y_bot, y_top, y_bot+2.0, y_bot+2.0),
                           angle = c(0, 180, 0, 180))
    bind_rows(df_t, df_c, df_p, df_d)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_baghchal <- function(has_matchsticks = FALSE) {
    df_t <- piecepack_rectangular_board(5L, 5L)
    df_p <- piecepack_pawns(x = c(1, 5, 5, 1), y = c(5, 5, 1, 1), angle = 180)
    if (has_matchsticks) {
        df_m <- piecepack_matchsticks(side = "back",
                       suit = rep(1:4, each=4),
                       rank = 2,
                       x = 0.5 + c(1,2,2,1, 3,4,4,3, 3,4,4,3, 1,2,2,1),
                       y = 0.5 + c(4,4,3,3, 4,4,3,3, 2,2,1,1, 2,2,1,1),
                       angle = rep(c(45, -45, -135, 135), 4))
        df <- bind_rows(df_t, df_m, df_p)
    } else {
        df <- bind_rows(df_t, df_p)
    }
    df
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_brandubh <- function() {
    df_t <- piecepack_rectangular_board(7L, 7L)
    df_c <- tibble(piece_side = c(rep("coin_back", 4L), "die_face"),
                    suit = 3L,
                    rank = c(1L, 3L, 4L, 6L, 2L),
                    cfg = "piecepack",
                    x = c(1, 1, 7, 7, 4),
                    y = c(1, 7, 1, 7, 4),
                    angle = c(0, 180, 0, 180, 0))
    df_a <- tibble(piece_side = c(rep("coin_back", 4L), "pawn_face"),
                    suit = 4L,
                    rank = c(1L, 3L, 4L, 6L, 1L),
                    x = c(4, 5, 4, 3, 4),
                    y = c(5, 4, 3, 4, 4),
                    cfg = "piecepack",
                    angle = c(180, 90, 0, 270, 0))
    df_f <- piecepack_coins(side = "face",
                            x = c(4, 4, 7, 6, 4, 4, 1, 2),
                            y = c(7, 6, 4, 4, 1, 2, 4, 4),
                            suit = rep(1:4, each = 2L),
                            rank = rep.int(c(2L, 5L), 4L),
                            angle = rep(c(180, 90, 0, 270), each = 2L))
    bind_rows(df_t, df_c, df_a, df_f)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_cribbage <- function() {
    df_l <- piecepack_rectangular_board(30, 3, x0 = 1, y0 = 3, max_tiles = 12L)
    df_r <- piecepack_rectangular_board(30, 3, x0 = 6, y0 = 3, max_tiles = 12L)
    df_t <- bind_rows(df_l, df_r) %>%
        mutate(suit = rep(1:4, each = 6L), rank = rep.int(1:6, 4L))
    df_c <- piecepack_coins(side = "face",
                            rank = rep(rep(1:6, each = 2), 2),
                            suit = c(rep.int(1:2, 6L), rep.int(3:4, 6L)),
                            x = rep(c(2, 7), each = 12),
                            y = rep(c(3,7,8,12,13,17,18,22,23,27,28,32), 2))
    df_p <- piecepack_pawns(x = c(1,3,6,8), y = 1)
    df_d <- piecepack_dice(x = c(2,7), y = 1, suit = c(1,3))
    bind_rows(df_t, df_c, df_p, df_d)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_cribbage_board <- piecepack_cribbage

#' @rdname piecepack_games_traditional
#' @export
piecepack_four_field_kono <- function() {
    df_t <- piecepack_rectangular_board(4L, 4L)
    df_c <- piecepack_coins(side = "back",
                            suit = rep(1:4, each = 4L),
                            rank = rep.int(3:6, 4L),
                            x = c(1:2,1:2,3:4,3:4,3:4,3:4,1:2,1:2),
                            y = rep(c(4,3,4,3,2,1,2,1), each = 2),
                            angle = rep(c(180,0), each = 8))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_julgonu <- function() {
    df_t <- piecepack_rectangular_board(4L, 4L)
    df_c <- piecepack_coins(side = "back",
                            suit = rep(1:2, each = 4),
                            rank = rep(3:6, 2),
                            x = 1 * c(1:4, 1:4),
                            y = rep(c(4,1), each = 4),
                            angle = rep(c(180,0), each = 4))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_ludo <- function() {
    df_tb <- piecepack_tiles(side = "back",
                    x = c(2, 5, 11, 14, rep.int(8, 4L)),
                    y = c(rep(8, 4), 2, 5, 11, 14),
                    suit = rep(1:4, each = 2L),
                    rank = rep.int(5:6, 4L))
    offset <- 0.50 + sqrt(2)
    df_tf <- piecepack_tiles(suit = 1:4, rank = 2L,
                             x = c(2, 9+offset, 14, 7-offset),
                             y = c(9+offset, 14, 7-offset, 2),
                             angle = c(135, 45, -45, -135))
    df_c <- piecepack_coins(side =  "back",
                            x = rep(df_tf$x, each = 4) + 0.5 * c(-sqrt(2), 0, sqrt(2), 0),
                            y = rep(df_tf$y, each = 4) + 0.5 * c(0, sqrt(2), 0, -sqrt(2)),
                            suit = rep(1:4, each=4L), rank = rep.int(1:4, 4L),
                            angle = rep(df_tf$angle, each = 4) + 45)
    df_p <- piecepack_pawns(x = c(-0.25, 7.5, 15.25, 7.5) + 0.5,
                            y = c(7.5, 15.25, 7.5, -0.25) + 0.5,
                            angle = c(-90, 180, 90, 0))
    df_d <- piecepack_dice(x = c(5, 11, 11, 5),
                           y = c(11, 11, 5, 5),
                           rank = 2L, angle = c(-90, 180, 90, 0))
    bind_rows(df_tb, df_tf, df_c, df_p, df_d)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_nine_mens_morris <- function(has_matchsticks = FALSE) {
    df <- piecepack_tiles(
           x = c(7,1,7,3,7,5,     13,13,11,11,9,9,
               7,13,7,11,7,9,   1,1,3,3,5,5),
           y = c(13,13,11,11,9,9,   7,13,7,11,7,9,
               1,1,3,3,5,5,       7,1,7,3,7,5)
    )
    if (has_matchsticks) {
        df_m <- piecepack_matchsticks(
                   suit = rep(1:4, each = 6L),
                   rank = 4L,
                   x = c(1,1,3,3,5,5, 9,9,11,11,13,13,
                       9,9,11,11,13,13, 1,1,3,3,5,5),
                   y = c(9,11,9,13,13,11, 11,13,13,9,11,9,
                       3,1,1,5,5,3, 3,5,5,1,1,3),
                   angle = rep(rep(c(0,90,90,0,90,0,0,90),each = 3L))
        )
        df <- bind_rows(df, df_m)
    }
    df
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_salta <- function(has_subpack = FALSE) {
    if (has_subpack) {
        df_t <- piecepack_donut_board()
    } else {
        df_t <- piecepack_rectangular_board(10L, 10L)
    }
    df_cf <- piecepack_coins(
                    rank=c(2:6, 6:2),
                    suit = c(2, 1, 1, 2, 1, 3, 3, 4, 4, 3),
                    x=c(seq(1,9,2), seq(2,10,2)), y=rep(c(1,10), each=5),
                    angle=rep(c(0,180), each=5)
    )
    if (has_subpack) {
        df_st <- piecepack_tiles(
                    cfg = "subpack",
                    suit=rep(c(2L,1L,3L,4L), each=5L),
                    rank=rep.int(2:6, 4L),
                    x=c(seq(2,10,2), seq(1,9,2), seq(10,2,-2), seq(9,1,-2)),
                    y=rep(c(2,3,8,9), each=5), angle=rep(c(0,180), each=10)
        )
        df_tb <- piecepack_tiles(side = "back", cfg = "subpack",
                                 suit = 1:4, rank = 1L,
                                 x = c(5,5,6,6), y = c(5,6,5,6))
        df <- bind_rows(df_t, df_tb, df_cf, df_st)
    } else {
        df_cb <- piecepack_coins(
                    side = "back",
                    suit=c(1:4, 4:1), rank = rep(2:5, 2),
                    x=c(seq(2,8,2), seq(3,9,2)), y=rep(c(2,9), each=4),
                    angle=rep(c(0,180), each=4)
        )
        df_cf2 <- piecepack_coins(
                     side = "face",
                     rank=c(1,1,6,1,1,6), suit = c(1,2,2,3,4,4),
                     x=c(10,9,9,1,2,2), y=c(2,3,3,9,8,8),
                     angle=rep(c(0,180), each=3)
        )
        df_d <- piecepack_dice(rank=2:5, x=seq(1,7,2), y=3)
        df_p <- piecepack_pawns(x=seq(10,4,-2), y=8, angle=180)
        df <- bind_rows(df_t, df_cf, df_cb, df_cf2, df_d, df_p)
    }
    df$cfg <- ifelse(is.na(df$cfg), "piecepack", df$cfg)
    df
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_tablut <- function(die_width = 0.63) {
    df_t <- piecepack_rectangular_board(9L, 9L)
    df_cf <- piecepack_coins(side = "face",
                 rank = rep.int(3:6, 4L),
                 suit = rep(1:4, each = 4L),
                 x = c(5,4,5,6,5,6,5,4,2,1,1,1,8,9,9,9),
                 y = c(2,1,1,1,8,9,9,9,5,6,5,4,5,4,5,6),
                 angle = c(rep(0,4),rep(180,4), rep(-90, 4), rep(90, 4))
    )
    df_cb <- piecepack_coins(side = "back",
                 suit = rep(1:4, each = 2),
                 rank = rep.int(1:2, 4L),
                 x = c(5,5,6,7,5,5,4,3),
                 y = c(6,7,5,5,4,3,5,5),
                 angle = rep(c(0, -90, 180,  90), each = 2)
    )
    if (less_than(0.5, die_width)) {
        df_d <- piecepack_dice(suit = 3L, rank = 1L, x = 5, y = 5)
    } else {
        df_d <- piecepack_dice(rank = 1L,
                     x = c(4.75,5.25,5.25,4.75),
                     y = c(5.25,5.25,4.75,4.75),
                     angle = c(0,-90,180,90))
    }
    df_p <- piecepack_pawns(suit = 3L, x = 5, y = 5)
    bind_rows(df_t, df_cf, df_cb, df_d, df_p)
}

#' @rdname piecepack_games_traditional
#' @export
piecepack_twelve_mens_morris <- piecepack_nine_mens_morris
