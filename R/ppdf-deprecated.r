#' Deprecated functions
#'
#' These functions are Deprecated in this release of ppdf,
#' they will be marked as Defunct and removed in a future version.
#'
#' `r deprecated_table()`
#' @name ppdf-deprecated
NULL

#' @rdname ppdf-deprecated
#' @inheritParams setup_by_name
#' @export
checkers_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "checkers", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @export
chess_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "chess", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "domino", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @inheritParams domino_concentration
#' @export
dominoes_concentration <- function(seed = NULL) {
    .Deprecated("domino_concentration")
    domino_concentration(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_domino_finder <- function(seed = NULL) {
    .Deprecated("domino_finder")
    domino_finder(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_domino_runners <- function(seed = NULL) {
    .Deprecated("domino_runners")
    domino_runners(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_fujisan <- function(seed = NULL) {
    .Deprecated("domino_fujisan")
    domino_fujisan(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_luzon <- function(seed = NULL) {
    .Deprecated("domino_luzon")
    domino_luzon(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_none <- function() {
    .Deprecated("domino_none")
    domino_none()
}

#' @rdname ppdf-deprecated
#' @export
dominoes_patience <- function(seed = NULL) {
    .Deprecated("domino_patience")
    domino_patience(seed = seed)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_the_jubilee <- function(seed = NULL) {
    .Deprecated("domino_the_jubilee")
    domino_the_jubilee(seed = seed)
}

#' @rdname ppdf-deprecated
#' @inheritParams domino_tiles
#' @export
dominoes_tiles <- function(n = 7, ...,
                           side = "face",
                           piece_side = paste0("tile_", side),
                           suit = sequence(n:1, from = 1:n),
                           rank = rep.int(1:n, n:1),
                           cfg = "dominoes",
                           x = sequence(n:1, from = 1:n),
                           y = 2 * rep.int(n:1, n:1) - 0.5,
                           angle = 0) {
    .Deprecated("domino_tiles")
    domino_tiles(n = n, piece_side = piece_side, suit = suit, rank = rank, cfg = cfg,
                 x = x, y = y, angle = angle)
}

#' @rdname ppdf-deprecated
#' @export
games_checkers <- function() {
    .Deprecated("checkers_games()")
    checkers_games()
}

#' @rdname ppdf-deprecated
#' @export
games_chess <- function() {
    .Deprecated("chess_games()")
    chess_games()
}

#' @rdname ppdf-deprecated
#' @export
games_dominoes <- function() {
    .Deprecated("domino_games()")
    domino_games()
}

#' @rdname ppdf-deprecated
#' @export
games_piecepack <- function() {
    .Deprecated("piecepack_games()")
    piecepack_games()
}

#' @rdname ppdf-deprecated
#' @export
games_stackpack <- function() {
    .Deprecated("stackpack_games()")
    stackpack_games()
}

#' @rdname ppdf-deprecated
#' @export
piecepack_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "piecepack", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @inheritParams piecepack_accordion
#' @export
piecepack_piecepack_accordion <- function(seed = NULL, tiles = NULL) {
    .Deprecated("piecepack_accordion")
    piecepack_accordion(seed = seed, tiles = tiles)
}

#' @rdname ppdf-deprecated
#' @export
piecepack_piecepack_halma <- function() {
    .Deprecated("piecepack_halma")
    piecepack_halma()
}

#' @rdname ppdf-deprecated
#' @export
piecepack_piecepack_klondike <- function(seed = NULL) {
    .Deprecated("piecepack_klondike")
    piecepack_klondike(seed = seed)
}

#' @rdname ppdf-deprecated
#' @inheritParams piecepack_rectangular_board
#' @export
piecepack_rect_board_tiles <- function(nrows = 8L, ncols = 8L, x0 = 1, y0 = 1, max_tiles = 24L,
                                       suit = rep.int(1:4, 6L), rank = rep(1:6, each = 4L)) {
    .Deprecated("piecepack_rectangular_board")
    piecepack_rectangular_board(nrows = nrows, ncols = ncols, x0 = x0, y0 = y0,
                                max_tiles = max_tiles, suit = suit, rank = rank)
}

#' @rdname ppdf-deprecated
#' @export
stackpack_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "piecepack", ..., getter = getter)
}
