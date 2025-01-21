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
            , "Four Seasons Chess"
            , "``stackpack_four_seasons_chess()``"
            , NA_character_
            , "https://www.chessvariants.com/historic.dir/4seiz.html"
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
stackpack_chinese_chess <- function() piecepack_chinese_chess(TRUE)

#' @rdname stackpack_games_other
#' @export
stackpack_four_seasons_chess <- function() piecepack_four_seasons_chess(TRUE)

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
