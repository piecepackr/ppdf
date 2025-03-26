#' Generate tarot cards
#'
#' `tarot_cards()` returns tarot cards.
#'
#' @param ... Should be left empty.
#' @param suit Suit value of cards.
#'             `1L` is "hearts", `2L` is "spades", `3L` is "clubs", `4L` is "diamonds", and `5L` is "trumps".
#'             Will be coerced by [tarot_suit()].
#' @param rank Rank value of cards.
#'             Normal suits are from `1L` (ace) to `14L` (king) plus a `15L` joker.
#'             Trump suits are from `1L` to `21L` plus a `22L` fool/excuse.
#'             Will be coerced by [tarot_rank()].
#' @param angle Rotation of piece (numeric vector of degrees, counter-clockwise).
#'              Will be coerced by [tarot_angle()].
#' @inheritParams piecepack_tiles
#' @return `r return_df()`
#' @name tarot_pieces
NULL

#' @rdname tarot_pieces
#' @export
tarot_cards <- function(...,
                        side = "face",
                        piece_side = paste0("card_", side),
                        suit = rep(1:4, each = 13L),
                        rank = rep.int(c(1:11, 13:14), 4L),
                        x = 2.5 * rep.int(1:13, 4L),
                        y = 3.5 * rep(4:1, each = 13L),
                        angle = 0,
                        length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep(piece_side, length.out = length.out),
           suit = rep(tarot_suit(suit), length.out = length.out),
           rank = rep(tarot_rank(rank), length.out = length.out),
           cfg = rep("playing_cards_tarot", length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(tarot_angle(angle), length.out = length.out))
}

tarot_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
    )
}
