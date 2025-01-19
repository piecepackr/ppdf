#' Generate chess boards and pieces
#'
#' `chess_board()` returns a square checkered board of desired size and type.
#' `chess_bits()` returns chess pieces.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell
#' @param y0 Y coordinate for the center of the first cell
#' @param ... Should be left empty.
#' @param side Either "face" or "back"
#' @param piece_side If `"board_face"` a checkered board (as in American Checkers).
#'                   If `"board_back"` a lined board (as in Turkish Checkers).
#' @param suit Suit value (color) of board.  `2L` is "black" and `3L` is "green".
#' @param angle Angle of board in degrees.
#'              Italian Checkers rotates its board 90 degrees compared to other checkers variants.
#' @param cell_width Width of board cell.
#'                   [piecepackr::game_systems()] supports `1` or `2`.
#' @return `r return_df()`
#' @name chess_pieces
NULL

#' @rdname chess_pieces
#' @export
chess_board <- function(nrows = 8L, ncols = nrows, x0 = 1, y0 = 1, ...,
                        side = "face", piece_side = paste0("board_", side),
                        suit = 3L, angle = 0, cell_width = 1) {
    stopifnot("Don't support non-square checkers boards yet" = nrows == ncols)
    check_dots_empty()
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = piece_side,
                       suit = as.integer(suit),
                       rank = as.integer(nrows),
                       x = as.double(x),
                       y = as.double(y),
                       angle = as.double(angle))
    set_cell_width(df_board, cell_width, "chess")
}

#' @inheritParams piecepack_tiles
#' @rdname chess_pieces
#' @export
chess_bits <- function(...,
                       suit = rep(1:6, each = 6L),
                       rank = rep(1:6, 6L),
                       x = rep(1:6, 6L),
                       y = rep(1:6, each = 6L),
                       angle = 0,
                       cell_width = 1,
                       length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep("bit_face", length.out = length.out),
        suit = rep(as.integer(suit), length.out = length.out),
        rank = rep(as.integer(rank), length.out = length.out),
        x = rep(as.double(x), length.out = length.out),
        y = rep(as.double(y), length.out = length.out),
        angle = rep(as.double(angle), length.out = length.out))
    set_cell_width(df_bits, cell_width, "chess")
}
