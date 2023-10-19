#' Zero-row data frames
#'
#' `checkers_none()`, `chess_none()`, `dominoes_none()`,
#' `piecepack_none()`, and `stackpack_none()` return zero-row data frames.
#' @examples
#' checkers_none()
#' @return A data frame with zero rows.
#' @name setup_none
#' @rdname setup_none
NULL

df_none <- function() {
    tibble::tibble(piece_side = character(0L),
                   suit = integer(0L), rank = integer(0L),
                   cfg = character(0),
                   x = numeric(0), y = numeric(0), angle = numeric(0))
}

#' @rdname setup_none
#' @export
checkers_none <- df_none

#' @rdname setup_none
#' @export
chess_none <- df_none

#' @rdname setup_none
#' @export
dominoes_none <- df_none

#' @rdname setup_none
#' @export
piecepack_none <- df_none

#' @rdname setup_none
#' @export
stackpack_none <- df_none
