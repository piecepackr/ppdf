#' Deprecated functions
#'
#' These functions are Deprecated in this release of ppdf,
#' they will be marked as Defunct and removed in a future version.
#'
#' `r deprecated_table()`
#' @name ppdf-deprecated
NULL

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
#' @inheritParams piecepack_rectangular_board
#' @export
piecepack_rect_board_tiles <- function(nrows = 8L, ncols = 8L, x0 = 1, y0 = 1, max_tiles = 24L,
                                       suit = rep.int(1:4, 6L), rank = rep(1:6, each = 4L)) {
    .Deprecated("piecepack_rectangular_board")
    piecepack_rectangular_board(nrows = nrows, ncols = ncols, x0 = x0, y0 = y0,
                                max_tiles = max_tiles, suit = suit, rank = rank)
}
