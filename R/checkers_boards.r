#' Checkers boards
#'
#' `checkers_board()` returns a square checkers board of desired size and type.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell
#' @param y0 Y coordinate for the center of the first cell
#' @param ... Ignored
#' @param piece_side If `"board_face"` a checkered board (as in American Checkers).
#'                   If `"board_back"` a lined board (as in Turkish Checkers).
#' @param suit Suit value (color) of board.  `2L` is "black" and `3L` is "green".
#' @param angle Angle of board in degrees.
#'              Italian Checkers rotates its board 90 degrees compared to other checkers variants.
#' @param cell_width Width of board cell.
#'                   [piecepackr::game_systems()] supports `1` or `2`.
#'                   If `NULL` then don't set it yet.
#' @return `r return_df()`
#' @export
checkers_board <- function(nrows = 8L, ncols = nrows, x0 = 1, y0 = 1, ...,
                           piece_side = "board_face", suit = 3L, angle = 0,
                           cell_width = 1L) {
    stopifnot("Don't support non-square checkers boards yet" = nrows == ncols)
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = piece_side, suit = suit, rank = nrows,
                       x = x, y = y, angle = angle)
    if (is.null(cell_width))
        df_board
    else
        set_cell_width(df_board, cell_width, "checkers")
}
