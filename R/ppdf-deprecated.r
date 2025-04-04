# nocov start
#' Deprecated functions
#'
#' These functions are Deprecated in this release of ppdf,
#' they will be marked as Defunct and removed in a future version.
#'
#' `r deprecated_table()`
#' @name ppdf-deprecated
NULL

## Unreleased angle helpers
#' @rdname ppdf-deprecated
#' @export
chess_angle <- function(angle) {
    .Deprecated("piece_angle")
    piece_angle(angle)
}

#' @rdname ppdf-deprecated
#' @export
domino_angle <- function(angle) {
    .Deprecated("piece_angle")
    piece_angle(angle)
}

#' @rdname ppdf-deprecated
#' @export
piecepack_angle <- function(angle) {
    .Deprecated("piece_angle")
    piece_angle(angle)
}

#' @rdname ppdf-deprecated
#' @export
tarot_angle <- function(angle) {
    .Deprecated("piece_angle")
    piece_angle(angle)
}

## Unreleased suit helpers
#' @rdname ppdf-deprecated
#' @export
alquerque_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
checker_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
chess_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}


#' @rdname ppdf-deprecated
#' @export
go_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
marble_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
morris_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
piecepack_suit <- function(suit) {
    .Deprecated("piece_suit")
    piece_suit(suit)
}

#' @rdname ppdf-deprecated
#' @export
domino_rank <- function(rank) {
    .Deprecated("piece_rank")
    piece_rank(rank)
}

#' @rdname ppdf-deprecated
#' @export
piecepack_rank <- function(rank) {
    .Deprecated("piece_rank")
    piece_rank(rank)
}

## In an official release
#' @rdname ppdf-deprecated
#' @inheritParams checker_american_checkers
#' @export
checkers_american_checkers <- function(cell_width = 1) {
    .Deprecated("checker_american_checkers")
    checker_american_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_american_pool_checkers <- function(cell_width = 1) {
    .Deprecated("checker_american_pool_checkers")
    checker_american_pool_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_board <- function(cell_width = 1) {
    .Deprecated("checker_board")
    checker_board(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_brazilian_checkers <- function(cell_width = 1) {
    .Deprecated("checker_brazilian_checkers")
    checker_brazilian_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_breakthrough <- function(cell_width = 1) {
    .Deprecated("checker_breakthrough")
    checker_breakthrough(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @inheritParams setup_by_name
#' @export
checkers_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    .Deprecated("checker_setup_by_name")
    setup_by_name(name, "checker", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @export
checkers_canadian_checkers <- function(cell_width = 1) {
    .Deprecated("checker_canadian_checkers")
    checker_canadian_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_crossings <- function(cell_width = 1) {
    .Deprecated("checker_crossings")
    checker_crossings(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_czech_checkers <- function(cell_width = 1) {
    .Deprecated("checker_czech_checkers")
    checker_czech_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_checkers <- function(cell_width = 1) {
    .Deprecated("checker_checkers")
    checker_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_dameo <- function(cell_width = 1) {
    .Deprecated("checker_dameo")
    checker_dameo(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_english_checkers <- function(cell_width = 1) {
    .Deprecated("checker_english_checkers")
    checker_english_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_focus <- function(cell_width = 1) {
    .Deprecated("checker_focus")
    checker_focus(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_four_field_kono <- function(cell_width = 1) {
    .Deprecated("checker_four_field_kono")
    checker_four_field_kono(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_frisian_checkers <- function(cell_width = 1) {
    .Deprecated("checker_frisian_checkers")
    checker_frisian_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_gothic_checkers <- function(cell_width = 1) {
    .Deprecated("checker_gothic_checkers")
    checker_gothic_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_grasshopper <- function(cell_width = 1) {
    .Deprecated("checker_grasshopper")
    checker_grasshopper(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_international_checkers <- function(cell_width = 1) {
    .Deprecated("checker_international_checkers")
    checker_international_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_italian_checkers <- function(cell_width = 1) {
    .Deprecated("checker_italian_checkers")
    checker_italian_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_jamaican_checkers <- function(cell_width = 1) {
    .Deprecated("checker_jamaican_checkers")
    checker_jamaican_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_julgonu <- function(cell_width = 1) {
    .Deprecated("checker_julgonu")
    checker_julgonu(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_lines_of_action <- function(cell_width = 1) {
    .Deprecated("checker_lines_of_action")
    checker_lines_of_action(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_none <- function() {
    .Deprecated("checker_none")
    checker_none()
}

#' @rdname ppdf-deprecated
#' @export
checkers_portuguese_checkers <- function(cell_width = 1) {
    .Deprecated("checker_portuguese_checkers")
    checker_portuguese_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_russian_checkers <- function(cell_width = 1) {
    .Deprecated("checker_russian_checkers")
    checker_russian_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_spanish_checkers <- function(cell_width = 1) {
    .Deprecated("checker_spanish_checkers")
    checker_spanish_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_thai_checkers <- function(cell_width = 1) {
    .Deprecated("checker_thai_checkers")
    checker_thai_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
checkers_turkish_checkers <- function(cell_width = 1) {
    .Deprecated("checker_turkish_checkers")
    checker_turkish_checkers(cell_width = cell_width)
}

#' @rdname ppdf-deprecated
#' @export
chess_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    .Deprecated("chess_setup_by_name")
    setup_by_name(name, "chess", ..., getter = getter)
}

#' @rdname ppdf-deprecated
#' @export
dominoes_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    .Deprecated("domino_setup_by_name")
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
    checker_games()
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
# nocov end
