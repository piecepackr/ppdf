#' Cast to angle doubles
#'
#' These functions help make sure angle values are the
#' right double values expected by the configurations used by
#' [piecepackr::game_systems()]  and [ppn::read_ppn()].
#' If the input is numeric it simply uses [as.double()] and if the
#' input is character it tries to cast to a reasonable value.
#' @param angle A numeric or character vector of angle values.
#' @return An double vector of angle values.
#' @examples
#' domino_angle(c("\U0001f06e", "\U0001f038"))
#' piecepack_angle(c("^", "<", "v", ">"))
#' @name angle_helper
NULL

angle_helper <- function(angle) {
    if (is.numeric(angle)) {
        as.double(angle)
    } else {
        vapply(tolower(angle), function(s) {
                   v <- angle_list[[s]]
                   if (is.null(v))
                       abort(paste("Unknown angle", dQuote(s)),
                             class = "ppdf_unknown_angle")
                   v
               }, FUN.VALUE = numeric(1L), USE.NAMES = FALSE)
    }
}

#' @rdname angle_helper
#' @export
chess_angle <- angle_helper

#' @rdname angle_helper
#' @export
domino_angle <- angle_helper

#' @rdname angle_helper
#' @export
piecepack_angle <- angle_helper
