#' Zero-row data frames
#'
#' `alquerque_none()`, `checker_none()`, `chess_none()`, `domino_none()`,
#' `go_none()`, `marble_none()`, `piecepack_none()`, and `stackpack_none()` return zero-row data frames.
#' @examples
#' checker_none()
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
alquerque_none <- df_none

#' @rdname setup_none
#' @export
checker_none <- df_none

#' @rdname setup_none
#' @export
chess_none <- df_none

#' @rdname setup_none
#' @export
domino_none <- df_none

#' @rdname setup_none
#' @export
go_none <- df_none

#' @rdname setup_none
#' @export
marble_none <- df_none

#' @rdname setup_none
#' @export
morris_none <- df_none

#' @rdname setup_none
#' @export
piecepack_none <- df_none

#' @rdname setup_none
#' @export
reversi_none <- df_none

#' @rdname setup_none
#' @export
stackpack_none <- df_none

#' @rdname setup_none
#' @export
tarot_none <- df_none
