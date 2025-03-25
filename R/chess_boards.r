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
#' @param suit Suit value (color) of board and pieces.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#'             Will be coerced by [chess_suit()].
#' @param rank Rank value of chess pieces from `1L` (pawn) to `6L` (king).
#'             Will be coerced by [chess_rank()].
#' @param angle Rotation of bit/board (numeric vector of degrees, counter-clockwise).
#'              Will be coerced by [chess_angle()].
#' @param cell_width Width of board cell.
#'                   [piecepackr::game_systems()] supports `1` or `2`.
#' @inheritParams piecepack_tiles
#' @return `r return_df()`
#' @name chess_pieces
NULL

#' @rdname chess_pieces
#' @export
chess_board <- function(nrows = 8L, ncols = nrows, x0 = 1, y0 = 1, ...,
                        side = "face", piece_side = paste0("board_", side),
                        suit = "green", angle = 0, cell_width = 1) {
    stopifnot("Don't support non-square checkers boards yet" = nrows == ncols)
    check_dots_empty()
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = piece_side,
                       suit = chess_suit(suit),
                       rank = as.integer(nrows),
                       x = as.double(x),
                       y = as.double(y),
                       angle = as.double(angle))
    set_cell_width(df_board, cell_width, "chess")
}

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
        suit = rep(chess_suit(suit), length.out = length.out),
        rank = rep(chess_rank(rank), length.out = length.out),
        x = rep(as.double(x), length.out = length.out),
        y = rep(as.double(y), length.out = length.out),
        angle = rep(chess_angle(angle), length.out = length.out))
    set_cell_width(df_bits, cell_width, "chess")
}
