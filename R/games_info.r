#' Data frames with game info
#'
#' `chess_games()`, ``domino_games()``, `checker_games()`, `piecepack_games()`, and `stackpack_games()`
#' contain information about the games
#' whose setups are provided by this package.
#'
#' @examples
#' head(piecepack_games())
#' # Number of games for each game system
#' nrow(alquerque_games())
#' nrow(chess_games())
#' nrow(checker_games())
#' nrow(domino_games())
#' nrow(go_games())
#' nrow(marble_games())
#' nrow(morris_games())
#' nrow(piecepack_games())
#' nrow(stackpack_games())
#' @return A [tibble::tibble()] data frame with character columns "game", "methods", "comment", and "url"
#'         for game name, setup function name(s), possible comment, and url for more information.
#' @rdname games_info
#' @name games_info
NULL

#' @rdname games_info
#' @export
alquerque_games <- function() {
    bind_rows(alquerque_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
chess_games <- function() {
    bind_rows(chess_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
checker_games <- function() {
    bind_rows(checker_games_variant(),
              checker_games_other()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
domino_games <- function() {
    bind_rows(domino_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
go_games <- function() {
    bind_rows(go_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
marble_games <- function() {
    bind_rows(marble_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
morris_games <- function() {
    bind_rows(morris_games_variant()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
piecepack_games <- function() {
    bind_rows(piecepack_games_checkers(),
              piecepack_games_chess(),
              piecepack_games_original(),
              piecepack_games_other(),
              piecepack_games_traditional()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
stackpack_games <- function() {
    bind_rows(stackpack_games_other()) %>%
        arrange_games()
}

#' @rdname games_info
#' @export
tarot_games <- function() {
    bind_rows(tarot_games_variant()) %>%
        arrange_games()
}

arrange_games <- function(df) {
    mutate(df, name = gsub("\\(|\\)", "", .data$game)) %>%
        arrange(.data$name) %>%
        select(-"name")
}

readme_markdown_list <- function(df) {
    stopifnot(requireNamespace("knitr", quietly = TRUE))
    paste(paste("*", stringr::str_glue_data(df, "[{game}]({url})")), collapse = "\n")
}
