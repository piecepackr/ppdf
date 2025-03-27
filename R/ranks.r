#' Cast to rank integers
#'
#' These functions help make sure rank values are the
#' right integer values expected by the configurations used by
#' [piecepackr::game_systems()]  and [ppn::read_ppn()].
#' If the input is numeric simply use [as.integer()] and if the
#' input is character try to cast to a reasonable value.
#' @param rank A numeric or character vector of rank values.
#' @return An integer vector of rank values.
#' @examples
#' chess_rank(c("P", "N", "B", "R", "Q", "K"))
#' chess_rank(c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a"))
#' piece_rank(c("n", "a", "2", "3", "4", "5"))
#' piece_rank(c("\U0001f06e", "\U0001f038"))
#' piece_rank(c("zero", "one", "eighteen"))
#' tarot_rank(c("ace", "ten", "jack", "queen", "knight", "king"))
#' @name piece_rank
NULL

#' @rdname piece_rank
#' @export
chess_rank <- function(rank) rank_helper(rank, chess_rank_list)

#' @rdname piece_rank
#' @export
piece_rank <- function(rank) rank_helper(rank, start_from_zero_rank_list)

#' @rdname piece_rank
#' @export
tarot_rank <- function(rank) rank_helper(rank, tarot_rank_list)

rank_helper <- function(rank, rank_list) {
    if (is.numeric(rank)) {
        as.integer(rank)
    } else {
        vapply(tolower(rank), function(r) {
                   v <- rank_list[[r]]
                   if (is.null(v))
                       abort(paste("Unknown rank", dQuote(r)),
                             class = "ppdf_unknown_rank")
                   v
               }, FUN.VALUE = integer(1L), USE.NAMES = FALSE)
    }
}
