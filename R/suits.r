# assume `tolower()` will have been used on keys
#### unicode glyphs that use option of default "black" and "white" colors
suit_list <- list(
    suns = 1L,
    sun = 1L,
    s = 1L,
    moons = 2L,
    moon = 2L,
    m = 2L,
    crowns = 3L,
    crown = 3L,
    c = 3L,
    arms = 4L,
    arm = 4L,
    a = 4L,
    hearts = 1L,
    heart = 1L,
    "\u2665" = 1L,
    "\u2661" = 1L,
    spades = 2L,
    spade = 2L,
    "\u2664" = 1L,
    "\u2660" = 1L,
    clubs = 3L,
    club = 3L,
    "\u2667" = 1L,
    "\u2663" = 1L,
    diamonds = 4L,
    diamond = 4L,
    "\u2666" = 1L,
    "\u2662" = 1L,
    stars = 5L,
    star = 5L,
    "\u2605" = 5L, #### Support in PPN
    "\u2606" = 5L,
    reds = 1L,
    red = 1L,
    r = 1L,
    blacks = 2L,
    black = 2L,
    k = 2L,
    greens = 3L,
    green = 3L,
    g = 3L,
    blues = 4L,
    blue = 4L,
    b = 4L,
    yellows = 5L,
    yellow = 5L,
    y = 5L,
    whites = 6L,
    white = 6L,
    w = 6L
)

#' @importFrom stringr str_replace
clean_suit <- function(suit) {
    tolower(suit) %>%
        str_replace("\u25cb", "white") %>% # white (default go) bit
        str_replace("\u25cf", "black") %>% # black (default go) bit
        str_replace("\u2654", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u2655", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u2656", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u2657", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u2658", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u2659", getOption("ppdf.white_checkers_color", "white")) %>% # white chess bit
        str_replace("\u265a", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u265b", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u265c", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u265d", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u265e", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u265f", getOption("ppdf.black_checkers_color", "black")) %>% # black chess bit
        str_replace("\u26c0", getOption("ppdf.white_checkers_color", "white")) %>% # white checkers bit
        str_replace("\u26c1", getOption("ppdf.white_checkers_color", "white")) %>% # white checkers bit
        str_replace("\u26c2", getOption("ppdf.black_checkers_color", "black")) %>% # black checkers bit
        str_replace("\u26c3", getOption("ppdf.black_checkers_color", "black")) # black checkers bit
}


#' Cast to suit integers
#'
#' These functions help make sure suit values are the
#' right integer values expected by the configurations used by
#' [piecepackr::game_systems()]  and [ppn::read_ppn()].
#' If the input is numeric simply use [as.integer()] and if the
#' input is character try to cast to a reasonable value.
#' @param suit A numeric or character vector of suit values.
#' @return An integer vector of suit values.
#' @examples
#' go_suit(c("R", "K", "G", "B", "Y", "W"))
#' go_suit(c("red", "black", "green", "blue", "yellow", "white"))
#' piecepack_suit(c("S", "M", "C", "A"))
#' piecepack_suit(c("suns", "moons", "crowns", "arms"))
#' @name suit_helper
NULL

suit_helper <- function(suit) {
    if (is.numeric(suit)) {
        as.integer(suit)
    } else {
        vapply(clean_suit(suit), function(s) {
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
go_suit <- suit_helper

#' @rdname suit_helper
#' @export
marble_suit <- suit_helper

#' @rdname suit_helper
#' @export
piecepack_suit <- suit_helper
