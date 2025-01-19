#' Deprecated functions
#'
#' These functions are Deprecated in this release of ppdf,
#' they will be marked as Defunct and removed in a future version.
#'
#' @name ppdf-deprecated
NULL

#' @rdname ppdf-deprecated
#' @param nrows,ncols,x0,y0,max_tiles,suit,rank See [piecepack_rectangular_board()].
#' @export
piecepack_rect_board_tiles <- function(nrows = 8L, ncols = 8L, x0 = 1, y0 = 1, max_tiles = 24L,
                                       suit = rep.int(1:4, 6L), rank = rep(1:6, each = 4L)) {
    .Deprecated("piecepack_rectangular_board()")
    piecepack_rectangular_board(nrows = nrows, ncols = ncols, x0 = x0, y0 = y0,
                                max_tiles = max_tiles, suit = suit, rank = rank)
}
