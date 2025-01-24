#' Setups for games playable with a piecepack stackpack
#'
#' \code{tibble} data frames of setups for `r nrow(stackpack_games_other())` other games playable with a piecepack stackpack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the games:
#'
#' `r man_markdown_table(stackpack_games_other())`
#'
#' @param max_tiles Maximum number of (piecepack) tiles available to build boards
#' @inheritParams piecepack_chess960
#' @rdname stackpack_games_other
#' @name stackpack_games_other
#' @return `r return_df()`
NULL

stackpack_games_other <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Alice Chess"
            , "``stackpack_alice_chess()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Alice_chess"
            , "Chaturaji"
            , "``stackpack_chaturaji()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Chaturaji"
            , "Fischer Random Chess AKA Chess960"
            , "``stackpack_fischer_random_chess()`` aka ``stackpack_chess960()``"
            , NA_character_
            , "https://www.chessvariants.com/diffsetup.dir/fischer.html"
            , "Four Seasons Chess"
            , "``stackpack_four_seasons_chess()``"
            , NA_character_
            , "https://www.chessvariants.com/historic.dir/4seiz.html"
            , "Horde Chess"
            , "``chess_horde_chess()``"
            , NA_character_
            , "https://lichess.org/variant/horde"
            , "(International) Chess"
            , "``stackpack_international_chess()`` aka ``stackpack_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Chess"
            , "Salta"
            , "``stackpack_salta()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Salta_(game)"
            , "Shogi aka Japanese Chess"
            , "``stackpack_shogi()`` aka ``stackpack_japanese_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Shogi"
            , "Ultima aka Baroque Chess"
            , "``stackpack_ultima()`` aka ``stackpack_baroque_chess()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Baroque_chess"
            , "Xiangqi AKA Chinese Chess"
            , "``stackpack_xiangqi()`` aka ``stackpack_chinese_chess()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Xiangqi"
    )
}

#' @rdname stackpack_games_other
#' @export
stackpack_alice_chess <- function(max_tiles = 24L) piecepack_alice_chess(TRUE, max_tiles)

#' @rdname stackpack_games_other
#' @export
stackpack_baroque_chess <- function() piecepack_baroque_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_chaturaji <- function() piecepack_chaturaji(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_chess <- function() piecepack_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_chess960 <- function(..., seed = NULL) {
    check_dots_empty()
    piecepack_chess960(seed = seed, has_stackpack = TRUE)
}

#' @rdname stackpack_games_other
#' @export
stackpack_fischer_random_chess <- piecepack_chess960

#' @rdname stackpack_games_other
#' @export
stackpack_chinese_chess <- function() piecepack_chinese_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_four_seasons_chess <- function() piecepack_four_seasons_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_horde_chess <- function() {
    df_board <- piecepack_rectangular_board(8L, 8L)
    df_bb <- piecepack_tiles(x = 1:8, y = 8, angle = 180,
                             suit = rep(1:4, 2L),
                             rank = c(4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                             cfg = "subpack")
    df_bf <- piecepack_pawns(x = 1:8, y = 7, angle = 180, suit = rep(4:1, 2L),
                             cfg = rep(c("piecepack", "subpack"), 4L))
    df_p1 <- piecepack_coins(suit = rep(1:4, 6L), rank = rep(1:6, each = 4L),
                             x = c(1:8, 8:1, 1:8), y = rep(1:3, each = 8L), side = "back")
    df_p2 <- piecepack_coins(x = c(1:8, 2:3, 6:7), y = c(rep.int(4, 8), rep.int(5, 4)),
                             suit = rep(1:4, 3), rank = rep(1:6, each = 2), side = "back",
                             cfg = "subpack")
    bind_rows(df_board, df_bb, df_bf, df_p1, df_p2)
}

#' @rdname stackpack_games_other
#' @export
stackpack_international_chess <- stackpack_chess

#' @rdname stackpack_games_other
#' @export
stackpack_japanese_chess <- function() piecepack_japanese_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_salta <- function() piecepack_salta(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_shogi <- stackpack_japanese_chess

#' @rdname stackpack_games_other
#' @export
stackpack_ultima <- stackpack_baroque_chess

#' @rdname stackpack_games_other
#' @export
stackpack_xiangqi <- stackpack_chinese_chess
