#' Generate morris boards and pieces
#'
#' `morris_board()` returns various morris boards.
#' `morris_bits()` returns morris pieces aka merels.
#'
#' @param n The number of "men" the morris board is for.
#' @param x0 X coordinate for the center of the first point
#' @param y0 Y coordinate for the center of the first point
#' @param ... Should be left empty.
#' @param suit Suit value (color) of board/bit.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".  Will be coerced by [piece_suit()].
#' @return `r return_df()`
#' @name morris_pieces
NULL

#' @rdname morris_pieces
#' @export
morris_board <- function(n = 9L, x0 = 1, y0 = 1, ..., suit = "green") {
    check_dots_empty()
    if (n < 5L) {
        x <- x0 + 1
        y <- y0 + 1
    } else if (n < 8L) {
        x <- x0 + 2
        y <- y0 + 2
    } else {
        x <- x0 + 3
        y <- y0 + 3
    }
    df_board <- tibble(piece_side = "board_face",
                       suit = piece_suit(suit),
                       rank = as.integer(n),
                       cfg = "morris",
                       x = as.double(x),
                       y = as.double(y),
                       angle = 0.0)
    df_board
}

#' @inheritParams piecepack_tiles
#' @rdname morris_pieces
#' @export
morris_bits <- function(...,
                        suit = 1:6, x = as.double(1:6), y = 1,
                        angle = 0,
                        length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep("bit_back", length.out = length.out),
                      suit = rep(piece_suit(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      cfg = rep("morris", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(as.double(angle), length.out = length.out))
    df_bits
}

#' Setups for morris variants
#'
#' \code{tibble} data frames of setups for `r nrow(morris_games_variant())` morris variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(morris_games_variant())`
#'
#' @name morris_games_variant
#' @rdname morris_games_variant
#' @return `r return_df()`
#' @examples
#' df <- morris_three_mens_morris()
#' if (requireNamespace("piecepackr", quietly = TRUE)) {
#'   piecepackr::render_piece(df, open_device = FALSE)
#' }
NULL

morris_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Three Men's Morris"
            , "``morris_three_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Three_men%27s_morris"
            , "Five Men's Morris"
            , "``morris_five_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Nine_men%27s_morris#Six_men's_morris"
            , "Six Men's Morris"
            , "``morris_six_mens_morris()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/25702/six-mens-morris"
            , "Seven Men's Morris"
            , "``morris_seven_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Nine_men%27s_morris#Six_men's_morris"
            , "Nine Men's Morris"
            , "``morris_nine_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Nine_men%27s_morris"
            , "Ten Men's Morris aka Lasker Morris"
            , "``morris_six_mens_morris()`` aka ``morris_lasker_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Nine_men%27s_morris#Lasker_morris"
            , "Twelve Men's Morris"
            , "``morris_twelve_mens_morris()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Morabaraba"
    )
}

#' @rdname morris_games_variant
#' @export
morris_three_mens_morris <- function() morris_board(3L)

#' @rdname morris_games_variant
#' @export
morris_five_mens_morris <- function() morris_board(5L)

#' @rdname morris_games_variant
#' @export
morris_six_mens_morris <- function() morris_board(6L)

#' @rdname morris_games_variant
#' @export
morris_seven_mens_morris <- function() morris_board(7L)

#' @rdname morris_games_variant
#' @export
morris_nine_mens_morris <- function() morris_board(9L)

#' @rdname morris_games_variant
#' @export
morris_ten_mens_morris <- function() morris_board(10L)

#' @rdname morris_games_variant
#' @export
morris_lasker_morris <- function() morris_board(10L)

#' @rdname morris_games_variant
#' @export
morris_twelve_mens_morris <- function() morris_board(12L)
