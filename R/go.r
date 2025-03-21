#' Generate go boards and pieces
#'
#' `go_board()` returns an go board.
#' `go_bits()` returns go pieces.
#'
#' @param x0 X coordinate for the center of the first point
#' @param y0 Y coordinate for the center of the first point
#' @param ... Should be left empty.
#' @param suit Suit value (color) of board/bit.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#' @return `r return_df()`
#' @name go_pieces
NULL

#' @rdname go_pieces
#' @export
go_board <- function(x0 = 1, y0 = 1, ..., suit = 2L) {
    check_dots_empty()
    x <- x0 + 2
    y <- y0 + 2
    df_board <- tibble(piece_side = "board_face", 
                       suit = as.integer(suit), 
                       rank = as.integer(1L),
                       cfg = "go",
                       x = as.double(x), 
                       y = as.double(y), 
                       angle = 0.0)
    df_board
}

#' @inheritParams piecepack_tiles
#' @rdname go_pieces
#' @export
go_bits <- function(...,
                          suit = 1:6, x = as.double(1:6), y = 1,
                          angle = 0,
                          length.out = NA_integer_) {
    check_dots_empty()
    df_bits <- tibble(piece_side = rep("bit_back", length.out = length.out),
                      suit = rep(as.integer(suit), length.out = length.out),
                      rank = rep(1L, length.out = length.out),
                      cfg = rep("go", length.out = length.out),
                      x = rep(as.double(x), length.out = length.out),
                      y = rep(as.double(y), length.out = length.out),
                      angle = rep(as.double(angle), length.out = length.out))
    df_bits
}

#' Setups for go variants
#'
#' \code{tibble} data frames of setups for `r nrow(go_games_variant())` go variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(go_games_variant())`
#'
#' @name go_games_variant
#' @rdname go_games_variant
#' @return `r return_df()`
#' @examples
#' df <- go_go()
#' if (requireNamespace("ppcli", quietly = TRUE) &&
#'     packageVersion("ppcli") >= "0.2.0-1") {
#'   ppcli::cat_piece(df)
#' }
NULL

go_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Go"
            , "``go_go()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Go_(game)"
    )
}

#' @rdname go_games_variant
#' @export
go_go <- function() {
    go_board()
}
