#' Cast to suit integers
#'
#' These functions help make sure suit values are the
#' right integer values expected by the configurations used by
#' [piecepackr::game_systems()]  and [ppn::read_ppn()].
#' If the input is numeric it simply uses [as.integer()] and if the
#' input is character it tries to cast to a reasonable value.
#' @param suit A numeric or character vector of suit values.
#' @return An integer vector of suit values.
#' @examples
#' piece_suit(c("red", "black", "green", "blue", "yellow", "white"))
#' piece_suit(c("R", "K", "G", "B", "Y", "W"))
#' piece_suit(c("suns", "moons", "crowns", "arms"))
#' piece_suit(c("S", "M", "C", "A"))
#' domino_suit(c("\U0001f06e", "\U0001f038"))
#' domino_suit(c("zero", "one", "eighteen"))
#' tarot_suit(c("\u2665", "\u2660", "\u2663", "\u2666"))
#' tarot_suit(c("hearts", "spades", "clubs", "diamonds", "trumps"))
#' tarot_suit(c("H", "S", "C", "D"))
#' @export
piece_suit <- function(suit) suit_helper(suit, suit_list)

suit_helper <- function(suit, suit_list) {
    if (is.numeric(suit)) {
        as.integer(suit)
    } else {
        vapply(tolower(suit), function(s) {
                   v <- suit_list[[s]]
                   if (is.null(v))
                       abort(paste("Unknown suit", dQuote(s)),
                             class = "ppdf_unknown_suit")
                   v
               }, FUN.VALUE = integer(1L), USE.NAMES = FALSE)
    }
}

#' @rdname piece_suit
#' @export
domino_suit <- function(suit) rank_helper(suit, domino_suit_list)

#' @rdname piece_suit
#' @export
tarot_suit <- function(suit) suit_helper(suit, tarot_suit_list)
