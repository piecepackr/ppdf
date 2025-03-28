#' Generate dice.
#'
#' `dice_dice()` generates data frames for standard six-sided dice.
#' `d4_dice()`, `numeral_dice()`, `d8_dice()`, `d10_dice()`, `percentile_dice()`, `d12_dice()`, and `d20_dice()` generates data frames for the classic seven polyhedral dice.
#' `fudge_dice()` generates data frames for fudge dice.
#'
#' @param ... Should be left empty.
#' @param suit Suit value (color) of dice.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".
#'             Will be coerced by [piece_suit()].
#' @param rank Rank value of dice from `1L` (one) on up.  
#'             d10 zeroes are `10L`.
#'             Will be coerced by [dice_rank()] except `percentile_dice()` will coerce with [percentile_dice_rank()] and `fudge_dice()` will coerce with [fudge_dice_rank()].
#' @param angle Rotation of dice (numeric vector of degrees, counter-clockwise).
#'              Will be coerced by [piece_angle()].
#' @inheritParams piecepack_tiles
#' @return `r return_df()`
#' @examples
#' dfd <- dice_dice(suit = 1:6, rank = 1:6, x = 1:6, y = 1)
#' dfn <- numeral_dice(suit = 1:6, rank = 1:6, x = 1:6, y = 2)
#' dff <- fudge_dice(suit = 1:6, rank = rep(c("-", " ", "+"), 2L),
#'                   x = 1:6, y = 3)
#' dfd12 <- d12_dice(suit = 1:6, rank = 1:6, x = 1:6, y = 4)
#' df <- rbind(dfd, dfn, dff, dfd12)
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1" &&
#'     piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   envir = game_systems()
#'   pmap_piece(df, envir = envir, default.units = "in", op_scale = 0.01)
#' }
#' @name dice_pieces
NULL

#' @rdname dice_pieces
#' @export
dice_dice <- function(...,
                      suit = 1:6, 
                      rank = 1L,
                      x = as.double(1:6),
                      y = 1,
                      angle = 0,
                      length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
d4_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d4", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
fudge_dice <- function(...,
                       suit = 1:6, 
                       rank = 1L,
                       x = as.double(1:6),
                       y = 1,
                       angle = 0,
                       length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(fudge_dice_rank(rank), length.out = length.out),
           cfg = rep("dice_fudge", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}


#' @rdname dice_pieces
#' @export
numeral_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_numeral", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
d8_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d8", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
d10_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d10", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}
#' @rdname dice_pieces
#' @export
percentile_dice <- function(...,
                                suit = 1:6, 
                                rank = 1L,
                                x = as.double(1:6),
                                y = 1,
                                angle = 0,
                                length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(percentile_dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d10_percentile", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
d12_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d12", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname dice_pieces
#' @export
d20_dice <- function(...,
                    suit = 1:6, 
                    rank = 1L,
                    x = as.double(1:6),
                    y = 1,
                    angle = 0,
                    length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(dice_rank(rank), length.out = length.out),
           cfg = rep("dice_d20", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}
