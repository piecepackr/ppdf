#' Generate meeples
#'
#' `meeple_bits()` returns meeples.
#'
#' @param ... Should be left empty.
#' @param suit Suit value (color) of meeples.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".  Will be coerced by [piece_suit()].
#' @examples
#' a <- c(0, 90, 180, 270, 0, 90)
#' df1 <- meeple_bits(side = "face", suit = 1:6, x = 1:6, y = 1, angle = a)
#' df2 <- meeple_bits(side = "top", suit = 1:6, x = 1:6, y = 2, angle = a)
#' df <- rbind(df1, df2)
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1") {
#'   grid::grid.newpage()
#'   envir = game_systems()
#'   pmap_piece(df, envir = envir, default.units = "in", op_scale = 0.5)
#' }
#' @return `r return_df()`
#' @name meeple_pieces
NULL

#' @inheritParams piecepack_tiles
#' @rdname meeple_pieces
#' @export
meeple_bits <- function(...,
                        side = "face",
                        piece_side = paste0("bit_", side),
                        suit = 1:6, x = as.double(1:6), y = 1,
                        angle = 0,
                        length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep(piece_side, length.out = length.out),
                      suit = rep(piece_suit(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      cfg = rep("meeples", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(piece_angle(angle), length.out = length.out))
    df_bits
}
