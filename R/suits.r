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
#' go_suit(c("red", "black", "green", "blue", "yellow", "white"))
#' go_suit(c("R", "K", "G", "B", "Y", "W"))
#' piecepack_suit(c("suns", "moons", "crowns", "arms"))
#' piecepack_suit(c("S", "M", "C", "A"))
#' domino_suit(c("\U0001f06e", "\U0001f038"))
#' domino_suit(c("zero", "one", "eighteen"))
#' tarot_suit(c("\u2665", "\u2660", "\u2663", "\u2666"))
#' tarot_suit(c("hearts", "spades", "clubs", "diamonds", "trumps"))
#' @name suit_helper
NULL

suit_helper <- function(suit) {
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

#' @rdname suit_helper
#' @export
alquerque_suit <- suit_helper

#' @rdname suit_helper
#' @export
checker_suit <- suit_helper

#' @rdname suit_helper
#' @export
chess_suit <- suit_helper

#' @rdname suit_helper
#' @export
domino_suit <- function(suit) rank_helper(suit, domino_suit_list)

#' @rdname suit_helper
#' @export
go_suit <- suit_helper

#' @rdname suit_helper
#' @export
marble_suit <- suit_helper

#' @rdname suit_helper
#' @export
morris_suit <- suit_helper

#' @rdname suit_helper
#' @export
piecepack_suit <- suit_helper

#' @rdname suit_helper
#' @export
tarot_suit <- suit_helper
