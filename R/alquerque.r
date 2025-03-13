#' Generate alquerque boards and pieces
#'
#' `alquerque_board()` returns an alquerque board.
#' `alquerque_bits()` returns alquerque pieces.
#'
#' @param x0 X coordinate for the center of the first point
#' @param y0 Y coordinate for the center of the first point
#' @param ... Should be left empty.
#' @param suit Suit value (color) of board.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#' @return `r return_df()`
#' @name alquerque_pieces
NULL

#' @rdname alquerque_pieces
#' @export
alquerque_board <- function(x0 = 1, y0 = 1, ..., suit = 3L) {
    check_dots_empty()
    x <- x0 + 2
    y <- y0 + 2
    df_board <- tibble(piece_side = "board_face", 
                       suit = as.integer(suit), 
                       rank = as.integer(1L),
                       cfg = "alquerque",
                       x = as.double(x), 
                       y = as.double(y), 
                       angle = 0.0)
    df_board
}

#' @inheritParams piecepack_tiles
#' @rdname alquerque_pieces
#' @export
alquerque_bits <- function(...,
                          suit = 1:6, x = as.double(1:6), y = 1,
                          angle = 0,
                          length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep("bit_back", length.out = length.out),
                      suit = rep(as.integer(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      cfg = rep("alquerque", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(as.double(angle), length.out = length.out))
    df_bits
}

#' Setups for alquerque variants
#'
#' \code{tibble} data frames of setups for `r nrow(alquerque_games_variant())` alquerque variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(alquerque_games_variant())`
#'
#' @name alquerque_games_variant
#' @rdname alquerque_games_variant
#' @return `r return_df()`
#' @examples
#' df <- alquerque_alquerque()
#' if (requireNamespace("ppcli", quietly = TRUE) &&
#'     packageVersion("ppcli") >= "0.2.0-1") {
#'   ppcli::cat_piece(df)
#' }
NULL

alquerque_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Alquerque"
            , "``alquerque_alquerque()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Alquerque"
            , "Bagh-chal"
            , "``alquerque_bagchal()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Bagh-chal"
    )
}

#' @rdname alquerque_games_variant
#' @export
alquerque_alquerque <- function() {
    bind_rows(alquerque_board(suit = 3L),
              alquerque_bits(x = c(1:5, 1:5, 4:5),
                             y = rep.int(1:3, c(5L, 5L, 2L)),
                             suit = 6L),
              alquerque_bits(x = c(1:5, 1:5, 1:2),
                             y = rep.int(5:3, c(5L, 5L, 2L)),
                             angle = 180,
                             suit = 2L))
}

#' @rdname alquerque_games_variant
#' @export
alquerque_baghchal <- function() {
    bind_rows(alquerque_board(suit = 3L),
              alquerque_bits(x = c(1, 5, 5, 1),
                             y = c(5, 5, 1, 1),
                             angle = 180,
                             suit = 2L))
}
