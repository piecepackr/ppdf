#' Generate checkers boards and pieces
#'
#' `checker_board()` returns a square checkers board of desired size and type.
#' `checker_bits()` returns checkers discs.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell
#' @param y0 Y coordinate for the center of the first cell
#' @param ... Should be left empty.
#' @param side Either "face" or "back"
#' @param piece_side If `"board_face"` a checkered board (as in American Checkers).
#'                   If `"board_back"` a lined board (as in Turkish Checkers).
#' @param suit Suit value (color) of board/checkers.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#'             Will be coerced by [piece_suit()].
#' @param angle Angle of board in degrees.
#'              Italian Checkers rotates its board 90 degrees compared to other checkers variants.
#' @param cell_width Width of board cell.
#'                   [piecepackr::game_systems()] supports `1` or `2`.
#'                   If `NULL` then don't set it yet.
#' @return `r return_df()`
#' @examples
#' dfb <- checker_board(nrows = 4L, ncol = 4L)
#' df1 <- checker_bits(suit = "white", x = c(1, 3), y = 1)
#' df2 <- checker_bits(suit = "white", x = c(2, 4), y = 2)
#' df3 <- checker_bits(suit = "black", x = c(1, 3), y = 3)
#' df4 <- checker_bits(suit = "black", x = c(2, 4), y = 4)
#' df <- rbind(dfb, df1, df2, df3, df4)
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1") {
#'   grid::grid.newpage()
#'   envir = game_systems()
#'   pmap_piece(df, envir = envir, default.units = "in")
#' }
#' @name checker_pieces
NULL

#' @rdname checker_pieces
#' @export
checker_board <- function(nrows = 8L, ncols = nrows, x0 = 1, y0 = 1, ...,
                          side = "face", piece_side = paste0("board_", side),
                          suit = "green", angle = 0, cell_width = getOption("ppdf.checker_cell_width", 1)) {
    stopifnot("Don't support non-square checkers boards yet" = nrows == ncols)
    check_dots_empty()
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = piece_side,
                       suit = piece_suit(suit),
                       rank = as.integer(nrows),
                       x = as.double(x),
                       y = as.double(y),
                       angle = piece_angle(angle))
    set_cell_width(df_board, cell_width, "checkers")
}

#' @inheritParams piecepack_tiles
#' @rdname checker_pieces
#' @export
checker_bits <- function(...,
                         side = "back",
                         piece_side = paste0("bit_", side),
                         suit = 1:6,
                         x = as.double(1:6),
                         y = 1,
                         angle = 0,
                         cell_width = getOption("ppdf.checker_cell_width", 1),
                         length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep(piece_side, length.out = length.out),
                      suit = rep(piece_suit(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(piece_angle(angle), length.out = length.out))
    set_cell_width(df_bits, cell_width, "checkers")
}
