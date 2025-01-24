#' Setups for chess variants playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_chess())` chess variants playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various chess variants:
#'
#' `r man_markdown_table(piecepack_games_chess())`
#'
#' @param cfg2 A string of a piecepack expansion (or perhaps \code{"piecepack"} for a second piecepack)
#' @param has_subpack Has a piecepack subpack
#' @param max_tiles Maximum number of (piecepack) tiles available to build boards
#' @inheritParams piecepack_dominoids
#' @rdname piecepack_games_chess
#' @name piecepack_games_chess
#' @return `r return_df()`
NULL

piecepack_games_chess <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Alice Chess"
            , "``piecepack_alice_chess()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Alice_chess"
            , "Chaturaji"
            , "``piecepack_chaturaji()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Chaturaji"
            , "Fischer Random Chess AKA Chess960"
            , "``piecepack_fischer_random_chess()`` aka ``piecepack_chess960()``"
            , NA_character_
            , "https://www.chessvariants.com/diffsetup.dir/fischer.html"
            , "Four Seasons Chess"
            , "``piecepack_four_seasons_chess()``"
            , NA_character_
            , "https://www.chessvariants.com/historic.dir/4seiz.html"
            , "(International) Chess"
            , "``piecepack_international_chess()`` aka ``piecepack_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Chess"
            , "Minishogi"
            , "``piecepack_minishogi()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Minishogi"
            , "Racing Kings"
            , "``piecepack_racing_kings()``"
            , NA_character_
            , "https://www.chessvariants.com/diffobjective.dir/racing.html"
            , "Shogi AKA Japanese Chess"
            , "``piecepack_shogi()`` aka ``piecepack_japanese_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Shogi"
            , "Ultima AKA Baroque Chess"
            , "``piecepack_ultima()`` aka ``piecepack_baroque_chess()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Baroque_chess"
            , "Xiangqi AKA Chinese Chess"
            , "``piecepack_xiangqi()`` aka ``piecepack_chinese_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Xiangqi"
    )
}


#' @rdname piecepack_games_chess
#' @export
piecepack_alice_chess <- function(has_subpack = FALSE, max_tiles = 24L) {
    max_tiles_per_board <- floor(max_tiles / 2)
    df_t1 <- piecepack_rectangular_board(8L, 8L, max_tiles = max_tiles_per_board)
    df_t2 <- piecepack_rectangular_board(8L, 8L, max_tiles = max_tiles_per_board, x0 = 11)
    df_p <- piecepack_chess_pieces(has_subpack)
    bind_rows(df_t1, df_t2, df_p)
}

#' @rdname piecepack_games_chess
#' @export
piecepack_baroque_chess <- function(has_subpack = FALSE) {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_p1 <- piecepack_coins(side = "back",
                suit = (1:8+1) %% 2 + 1, rank = NA_integer_,
                x = 1:8, y = 7, angle = 180)
    df_p2 <- piecepack_coins(side = "back",
                suit = 1:8 %% 2 + 3, rank = NA_integer_,
                x = 1:8, y = 2)
    df_r <- piecepack_coins(
                suit = 1:4, x = c(1,8,8,1), y = c(8,8,1,1), rank = NA_integer_)
    if (has_subpack) {
        df_r <- mutate(df_r,
                       cfg = rep(c("subpack", "piecepack"), 2L),
                       piece_side = rep(c("tile_face", "die_face"), 2L),
                       rank = 4L, angle = c(180,0,0,180))
    } else {
        df_r <- mutate(df_r, rank = c(4L, 1L, 4L, 1L), angle = c(180, 180, 0, 0))
    }
    df_n <- piecepack_dice(rank = 2L, x = c(2,7,7,2), y = c(8,8,1,1),
                           angle = c(180,180,0,0))
    df_b <- piecepack_pawns(
               x = c(3,6,6,3), y = c(8,8,1,1), angle = c(180,180,0,0))
    df_q <- piecepack_coins(
                   suit = c(1,3), rank = 5,
                   x = c(4,5), y = c(8,1), angle = c(180,0))
    df_k <- piecepack_coins(
                   suit = c(2,4), rank = 6,
                   x = c(5,4), y = c(8,1), angle = c(180,0))
    if (has_subpack) {
        df_sb <- bind_rows(df_n, mutate(df_b, rank = 3L), df_q, df_k) %>%
            mutate(piece_side = "tile_face", cfg = "subpack")
    } else {
        df_sb <- bind_rows(df_n, df_b, df_q, df_k)
    }
    df <- bind_rows(df_t, df_p1, df_p2, df_r, df_sb) %>% fill_piece_rank()
    df
}

#' @rdname piecepack_games_chess
#' @export
piecepack_chaturaji <- function(has_subpack = FALSE) {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_p <- piecepack_coins(side = "back",
                            x = c(rep(2,4), 5:8, rep(7,4), 1:4),
                            y = c(5:8, rep(7,4), 1:4, rep(2,4)),
                            suit = rep(1:4, each = 4L),
                            rank = NA_integer_,
                            angle = rep(c(-90,180,90,0), each = 4L))
    df_b <- piecepack_coins(side = "face", rank = 3L, suit = 1:4,
                            x = c(1,8,8,1), y = c(8,8,1,1), angle = c(-90,180,90,0))
    df_h <- piecepack_coins(side = "face", rank = 2L, suit = 1:4,
                            x = c(1,7,8,2), y = c(7,8,2,1), angle = c(-90,180,90,0))
    df_r <- piecepack_dice(rank = 4L, x = c(1,6,8,3), y = c(6,8,3,1), angle = c(-90,180,90,0))
    df_k <- piecepack_pawns(x = c(1,5,8,4), y = c(5,8,4,1), angle = c(-90,180,90,0))
    if (has_subpack) {
        df_sb <- bind_rows(df_b, df_h, df_r, mutate(df_k, rank = 6L)) %>%
            mutate(piece_side = "tile_face", cfg = "subpack")
    } else {
        df_sb <- bind_rows(df_b, df_h, df_r, df_k)
    }
    df <- bind_rows(df_t, df_p, df_sb) %>% fill_piece_rank()
    df
}

#' @rdname piecepack_games_chess
#' @export
piecepack_chess <- function(has_subpack = FALSE) {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_p <- piecepack_chess_pieces(has_subpack)
    bind_rows(df_t, df_p)
}

#' @rdname piecepack_games_chess
#' @export
piecepack_chess960 <- function(..., seed = NULL, has_subpack = FALSE) {
    check_dots_empty()
    if (!is.null(seed)) withr::local_seed(seed)
    ranks <- fischer_random_ranks()
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_p <- piecepack_chess_pieces(has_subpack, ranks = ranks)
    bind_rows(df_t, df_p)
}

#' @rdname piecepack_games_chess
#' @export
piecepack_fischer_random_chess <- piecepack_chess960

#' @rdname piecepack_games_chess
#' @export
piecepack_chinese_chess <- function(has_subpack = FALSE) {
    ang2 <- rep(c(180, 0), each = 2)
    suits <- c(1L, 2L, 4L, 3L)
    x2 <- function(x) rep(c(x, 10-x), 2)
    y2 <- function(y) rep(c(11-y, y), each = 2)
    df_t1 <- piecepack_rectangular_board(10L, 9L, rank = rep(3:6, each = 4))
    df_t2 <- piecepack_tiles(suit = c(1,3), rank = 2L,
                             x = 5, y = c(9, 2), angle = c(180, 0))
    df_zu1 <- piecepack_coins(side = "back",
                              suit = (1:5+1) %% 2 + 3, rank = c(1, 4, 6, 1, 4),
                              x = seq(1, 9, 2), y = 4)
    df_zu2 <- piecepack_coins(side = "back",
                              suit = (1:5+1) %% 2 + 1, rank = c(1, 4, 6, 1, 4),
                              x = seq(1, 9, 2), y = 7, angle = 180)
    df_pao <- piecepack_pawns(suit = suits, x = x2(2), y = y2(3), angle = ang2)
    df_che <- piecepack_dice(suit = suits, rank = 4L, x = x2(1), y = y2(1), angle = ang2)
    df_ma <- piecepack_coins(side = "face", suit = suits, rank = 2L,
                             x = x2(2), y = y2(1), angle = ang2)
    df_xiang <- piecepack_coins(side = "face", suit = suits, rank = 3L,
                                x = x2(3), y = y2(1), angle = ang2)
    df_shi <- piecepack_coins(side = "face", suit = suits, rank = 5L,
                              x = x2(4), y = y2(1), angle = ang2)
    df_jiang <- piecepack_coins(side = "face", suit = c(4,2), rank = 6L,
                                x = 5, y = c(1,10), angle = c(0, 180))
    df_sb <- bind_rows(df_ma, df_xiang, df_shi, df_jiang, df_che)
    if (has_subpack) {
        df_sb <- mutate(df_sb, piece_side = "tile_face", cfg = "subpack")
    }
    df <- bind_rows(df_t1, df_t2, df_zu1, df_zu2, df_pao, df_sb)
    df
}


#' @rdname piecepack_games_chess
#' @export
piecepack_four_seasons_chess <- function(has_subpack = FALSE) {
    df_t <- piecepack_rectangular_board(8L, 8L)
    angles <- c(180,90,0,-90)
    suits <- c(1,4,2,3)
    df_p <- piecepack_coins(side = "back",
                   suit = rep(suits, each = 4),
                   rank = rep.int(3:6, 4L),
                   x = c(1,2,3,3,6,6,7,8,8,7,6,6,3,3,2,1),
                   y = c(6,6,7,8,8,7,6,6,3,3,2,1,1,2,3,3),
                   angle = rep(angles, each = 4))
    df_k <- piecepack_pawns(suit = suits, angle = angles,
                            x = c(1,8,8,1), y = c(8,8,1,1))
    df_r <- piecepack_dice(suit = suits, rank = 4L,
                           x = c(2,7,7,2), y = c(8,8,1,1), angle = angles)
    df_b <- piecepack_coins(
                   suit = suits, rank = ifelse(has_subpack, 3, 1),
                   x = c(2,7,7,2), y = c(7,7,2,2), angle = angles)
    df_n <- piecepack_coins(
                   suit = suits, rank = 2L,
                   x = c(1,8,8,1), y = c(7,7,2,2), angle = angles)
    if (has_subpack) {
        df_sb <- bind_rows(df_k %>% mutate(rank = 6L), df_r, df_b, df_n) %>%
            mutate(piece_side = "tile_face", cfg = "subpack")
    } else {
        df_sb <- bind_rows(df_k, df_r, df_b, df_n)
    }
    df <- bind_rows(df_t, df_p, df_sb)
    df
}

piecepack_chess_pieces <- function(has_subpack = FALSE, ...,
                                   ranks = c(4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L)) {
    if (has_subpack) {
        df_pb <- piecepack_coins(side = "back", x = 1:8, y = 7, angle = 180,
                                 suit = rep(1:2, each = 4), rank = rep(3:6, 2))
        df_ob <- piecepack_tiles(side = "face", cfg = "subpack",
                                 suit = rep(1:2, each = 4), x = 1:8, y = 8, angle = 180,
                                 rank = ranks)
        df_pw <- piecepack_coins(side = "back", x = 1:8, y = 2,
                        suit = rep(4:3, each = 4), rank = rep(3:6, 2))
        df_ow <- piecepack_tiles(side = "face", cfg = "subpack",
                                 suit = rep(4:3, each = 4), x = 1:8, y = 1,
                                 rank = ranks)
        bind_rows(df_pb, df_pw, df_ow, df_ob)
    } else {
        xr <- function(r) {
            i <- which(ranks == r)
            c(i, rev(i))
        }
        df_p1 <- piecepack_coins(side = "back", x = 1:8, y = 7, angle = 180,
                        suit = (1:8+1) %% 2 + 1, rank = c(1,1, 3,3, 4,4, 5,6))
        df_p2 <- piecepack_coins(side = "back", x = 1:8, y = 2, angle = 0,
                        suit = 1:8 %% 2 + 3, rank = c(1,1, 3,3, 4,4, 5,6))
        df_r <- piecepack_dice(rank = 4L, x = xr(4L), y = c(8,8,1,1),
                               angle = c(180,180,0,0))
        df_n <- piecepack_coins(side = "face", rank = 2, suit = 1:4,
                                x = xr(2L), y = c(8,8,1,1), angle = c(180,180,0,0))
        df_b <- piecepack_pawns(x = xr(3L), y = c(8,8,1,1), angle = c(180,180,0,0))
        df_q <- piecepack_coins(side = "face", rank = 5L, suit = c(3L, 2L),
                                x = xr(5L), y = c(8,1), angle = c(180,0))
        df_k <- piecepack_coins(side = "face", rank = 6L, suit = c(4L, 1L),
                                x = xr(6L), y = c(8,1), angle = c(180,0))
        bind_rows(df_p1, df_p2, df_r, df_n, df_b, df_q, df_k)
    }
}


#' @rdname piecepack_games_chess
#' @export
piecepack_international_chess <- piecepack_chess

#' @rdname piecepack_games_chess
#' @export
piecepack_japanese_chess <- function(has_subpack = FALSE, cfg2 = "piecepack") {
    # board
    df_t <- piecepack_rectangular_board(9L, 9L)

    # pawns
    df_pb <- piecepack_coins(side = "back", x = 1:9, y = 3,
                             suit = 1:9 %% 4 +1, rank = NA_integer_)
    df_pt <- df_pb %>% mutate(y = 7, angle = 180)
    df_p <- bind_rows(df_pb, df_pt) %>% fill_piece_rank()

    # bishops
    df_b <- piecepack_coins(rank = 3L, cfg = cfg2, suit = c(2,4),
                   x = c(2, 10-2), y = c(2, 10-2), angle = c(0, 180))

    # rooks
    df_r <- piecepack_coins(rank = 4L, cfg = cfg2, suit = c(3,1),
                   x = c(8, 10-8), y = c(2, 10-2), angle = c(0, 180))

    # lances
    df_l <- piecepack_coins(rank = 5L, cfg = cfg2, suit = 1:4,
                   x = c(1,9,10-1,10-9), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))
    # knights
    df_n <- piecepack_coins(rank = 2L, cfg = cfg2, suit = c(4,1:3),
                   x = c(2,8,10-2,10-8), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))

    # silvers
    df_s <- piecepack_coins(rank = 6L, cfg = cfg2, suit = c(3:4, 1:2),
                   x = c(3,7,10-3,10-7), y = c(1,1,10-1,10-1), angle = c(0,0,180,180))
    # golds
    df_g <- piecepack_dice(rank = 6L, angle = c(0,0,180,180),
                           x = c(4,6,10-4,10-6), y = c(1,1,10-1,10-1))
    # kings
    df_k <- piecepack_pawns(suit = 4-0:1, x = c(5,10-5), y = c(1,10-1), angle = c(0,180))

    df <- bind_rows(df_t, df_p, df_b, df_r, df_l, df_n, df_s, df_g, df_k)

    if (has_subpack) {
        df <- mutate(df,
                     cfg = ifelse(.data$piece_side == "coin_face", "subpack", .data$cfg),
                     piece_side = ifelse(.data$piece_side == "coin_face", "tile_face", .data$piece_side))
    }
    df
}

#' @rdname piecepack_games_chess
#' @export
piecepack_minishogi <- function() {
    df_tiles <- piecepack_rectangular_board(5L, 5L)
    df_faces <- piecepack_coins(side = "face",
                                x = c(1, 1, 4, 5, 5, 5, 2, 1),
                                y = c(2, 1, 1, 1, 4, 5, 5, 5),
                                rank = c(1, 6, 3, 4, 1, 6, 3, 4),
                                suit = rep(1:2, each = 4),
                                angle = rep(c(0, 180), each = 4))
    df_backs <- piecepack_coins(side = "back",
                                x = c(2, 3, 4, 3), y = c(1, 1, 5, 5),
                                rank = c(4, 4, 5, 5), suit = c(1, 2, 1, 2),
                                angle = c(0, 0, 180, 180))
    bind_rows(df_tiles, df_faces, df_backs)
}

#' @rdname piecepack_games_chess
#' @export
piecepack_racing_kings <- function() {
    df_tiles <- piecepack_rectangular_board(8L, 8L)
    df_b <- piecepack_coins(side = "face", angle = 180,
                            rank = c(6L, 4L, 3L, 2L, 5L, 4L, 3L, 2L),
                            suit = rep(1:2, each = 4L),
                            x = rep(1:4, 2L), y = rep(2:1, each = 4L))
    df_w <- piecepack_coins(side = "face",
                            rank = c(6L, 4L, 3L, 2L, 5L, 4L, 3L, 2L),
                            suit = rep(3:4, each = 4L),
                            x = rep(8:5, 2L), y = rep(2:1, each = 4L))
    bind_rows(df_tiles, df_b, df_w)
}

#' @rdname piecepack_games_chess
#' @export
piecepack_shogi <- piecepack_japanese_chess

#' @rdname piecepack_games_chess
#' @export
piecepack_ultima <- piecepack_baroque_chess

#' @rdname piecepack_games_chess
#' @export
piecepack_xiangqi <- piecepack_chinese_chess
