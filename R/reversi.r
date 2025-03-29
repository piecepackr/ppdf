#' Generate reversi boards and pieces
#'
#' `reversi_board()` returns various reversi boards.
#' `reversi_bits()` returns reversi pieces.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first point
#' @param y0 Y coordinate for the center of the first point
#' @param ... Should be left empty.
#' @param side Either "face" or "back"
#' @param piece_side If `"board_face"` a lined board.
#'                   If `"board_back"` a checkered board.
#' @param suit Suit value (color) of board/bit.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".  Will be coerced by [piece_suit()].
#' @param angle Angle of board in degrees.
#' @return `r return_df()`
#' @name reversi_pieces
NULL

#' @rdname reversi_pieces
#' @export
reversi_board <- function(nrows = 8L, ncols = nrows, x0 = 1, y0 = 1, ...,
                          side = "face", piece_side = paste0("board_", side),
                          suit = "black",
                          angle = 0) {
    stopifnot("Don't support non-square reversi boards yet" = nrows == ncols)
    check_dots_empty()
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = piece_side,
                       suit = piece_suit(suit),
                       rank = as.integer(nrows),
                       cfg = "reversi",
                       x = as.double(x),
                       y = as.double(y),
                       angle = piece_angle(angle))
}

#' @inheritParams piecepack_tiles
#' @rdname reversi_pieces
#' @export
reversi_bits <- function(...,
                        side = "face", piece_side = paste0("bit_", side),
                        suit = 1:6, x = as.double(1:6), y = 1,
                        angle = 0,
                        length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep(piece_side, length.out = length.out),
                      suit = rep(piece_suit(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      cfg = rep("reversi", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(piece_angle(angle), length.out = length.out))
    df_bits
}

#' Setups for reversi variants
#'
#' \code{tibble} data frames of setups for `r nrow(reversi_games_variant())` reversi variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(reversi_games_variant())`
#'
#' @name reversi_games_variant
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @rdname reversi_games_variant
#' @return `r return_df()`
NULL

reversi_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Ming Mang"
            , "``reversi_ming_mang()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/40573/ming-mang"
            , "Reversi"
            , "``reversi_reversi()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Reversi"
    )
}

# I've seen sources with board setup permutated in various ways
# Matching setup by Damian Walker (shrunk to 8x8 board)

#' @rdname reversi_games_variant
#' @export
reversi_ming_mang <- function(nrows = 8L, ncols = nrows) {
    df_board <- reversi_board(nrows, ncols)
    df_w1 <- reversi_bits(side = "back", suit = "black",
                          x = 1, y = seq.int(nrows - 1L))
    df_w2 <- reversi_bits(side = "back", suit = "black",
                          x = seq.int(ncols - 1L), y = nrows)
    df_k1 <- reversi_bits(side = "face", suit = "black",
                          x = ncols, y = seq.int(2L, nrows))
    df_k2 <- reversi_bits(side = "face", suit = "black",
                          x = seq.int(2L, ncols), y = 1L)
    bind_rows(df_board, df_w1, df_w2, df_k1, df_k2)
}

#' @rdname reversi_games_variant
#' @export
reversi_reversi <- function(nrows = 8L, ncols = nrows) reversi_board(nrows, ncols)
