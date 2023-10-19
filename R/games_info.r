#' Data frames with game info
#'
#' `games_chess()`, ``games_dominoes()``, `games_checkers()`, `games_piecepack()`, and `games_stackpack()`
#' contain information about the games
#' whose setups are provided by this package.
#'
#' @examples
#' head(games_piecepack())
#' # Number of games for each game system
#' nrow(games_chess())
#' nrow(games_checkers())
#' nrow(games_dominoes())
#' nrow(games_piecepack())
#' nrow(games_stackpack())
#' @return A [tibble::tibble()] data frame with character columns "game", "methods", "comment", and "url"
#'         for game name, setup function name(s), possible comment, and url for more information.
#' @rdname games_info
#' @name games_info
NULL

#' @rdname games_info
#' @export
games_chess <- function() {
    bind_rows(games_chess_variant()) %>%
        arrange(.data$game)
}

#' @rdname games_info
#' @export
games_checkers <- function() {
    bind_rows(games_checkers_variant(),
                    games_checkers_other()) %>%
        arrange(.data$game)
}

#' @rdname games_info
#' @export
games_dominoes <- function() {
    bind_rows(games_dominoes_variant()) %>%
        arrange(.data$game)
}

#' @rdname games_info
#' @export
games_piecepack <- function() {
    bind_rows(games_piecepack_checkers(),
              games_piecepack_chess(),
              games_piecepack_original(),
              games_piecepack_other(),
              games_piecepack_traditional()) %>%
        arrange(.data$game)
}

#' @rdname games_info
#' @export
games_stackpack <- function() {
    bind_rows(games_stackpack_other()) %>%
        arrange(.data$game)
}

man_markdown_table <- function(df) {
    stopifnot(requireNamespace("knitr", quietly = TRUE))
    df$url <- paste0("<", df$url, ">")
    df <- dplyr::select(df, -.data$comment, -.data$methods)
    names(df) <- c("**Game**", "**URL**")
    knitr::kable(df, "pipe")
}

readme_markdown_list <- function(df) {
    stopifnot(requireNamespace("knitr", quietly = TRUE))
    paste(paste("*", stringr::str_glue_data(df, "[{game}]({url})")), collapse = "\n")
}
