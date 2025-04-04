#' Generate marbles boards and pieces
#'
#' `marble_board()` returns a square holed board of desired size and color.
#' `marble_bits()` returns marble pieces.
#'
#' @param nrows Number of rows in game board (i.e. number of holes vertically)
#' @param ncols Number of columns in game board (i.e. number of holes horizontally)
#' @param x0 X coordinate for the center of the first hole
#' @param y0 Y coordinate for the center of the first hole
#' @param ... Should be left empty.
#' @param suit Suit value (color) of board and bits.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#'             Will be coerced by [piece_suit()].
#' @param rank Determines the size of the marble from 0.5 inch diameter (rank 1) to 1.0 inch diamter (rank 9).
#'             The default is 1.0 inch diameter marbles which match the holed boards.
#' @param scale_factor Used by `{ppn}`.  Should usually be `0.5` for square pyramidal games but could be `1` if not playing a pyramidal game.
#' @return `r return_df()`
#' @name marble_pieces
#' @examples
#' dfb <- marble_board(nrows = 2L, scale_factor = 1.0)
#' dfm <- marble_bits(x = c(1:2, 1:2, 1.5),
#'                    y = c(1, 1, 2, 2, 1.5),
#'                    suit = c(1:3, 5:6))
#' df <- rbind(dfb, dfm)
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1") {
#'   grid::grid.newpage()
#'   envir = game_systems()
#'   pmap_piece(df, envir = envir, default.units = "in")
#' }
NULL

#' @rdname marble_pieces
#' @export
marble_board <- function(nrows = 4L, ncols = nrows,
                          x0 = 1.0 * scale_factor, 
                          y0 = 1.0 * scale_factor,
                          ..., 
                          suit = "cyan",
                          scale_factor = 0.5) {
    check_dots_empty()
    stopifnot("Don't support non-square marbles boards yet" = nrows == ncols)
    check_dots_empty()
    x <- x0 - 0.5 + 0.5 * ncols
    y <- y0 - 0.5 + 0.5 * nrows
    df_board <- tibble(piece_side = "board_face", 
                       suit = piece_suit(suit), 
                       rank = as.integer(nrows),
                       cfg = "marbles",
                       x = as.double(x), 
                       y = as.double(y), 
                       angle = 0.0)
    attr(df_board, "scale_factor") <- scale_factor
    df_board
}

#' @inheritParams piecepack_tiles
#' @rdname marble_pieces
#' @export
marble_bits <- function(...,
                         suit = 1:6, 
                         rank = 9L,
                         x = as.double(1:6),
                         y = 1.0,
                         angle = 0.0,
                         length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep("bit_back", length.out = length.out),
                      suit = rep(piece_suit(suit), length.out = length.out),
                      rank = rep(as.integer(rank), length.out = length.out),
                      cfg = rep("marbles", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(piece_angle(angle), length.out = length.out))
    df_bits
}

marble_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
    )
}
