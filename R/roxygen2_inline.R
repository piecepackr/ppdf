man_markdown_table <- function(df) {
    stopifnot(requireNamespace("knitr", quietly = TRUE))
    df$url <- paste0("<", df$url, ">")
    df <- dplyr::select(df, -.data$comment, -.data$methods)
    names(df) <- c("**Game**", "**URL**")
    knitr::kable(df, "pipe")
}

return_df <- function() {
'A [tibble::tibble()] data frame with the following columns:

* "piece_side"
* "suit"
* "rank"
* "cfg" (optional, defaults to `"piecepack"`)
* "x"
* "y"
* "angle" (optional, defaults to `0`).

This data frame is compatible with [piecepackr::render_piece()] and likely [ppcli::cat_piece()].'
}

deprecated_table <- function() {
    df <- tribble(~deprecated
            , ~replacement
            , "``dominoes_concentration()``"
            , "``domino_concentration()``"
            , "``dominoes_domino_finder()``"
            , "``domino_finder()``"
            , "``dominoes_domino_runners()``"
            , "``domino_runners()``"
            , "``dominoes_fujisan()``"
            , "``domino_fujisan()``"
            , "``dominoes_luzon()``"
            , "``domino_luzon()``"
            , "``dominoes_none()``"
            , "``domino_none()``"
            , "``dominoes_patience()``"
            , "``domino_patience()``"
            , "``dominoes_the_jubilee()``"
            , "``domino_the_jubilee()``"
            , "``dominoes_tiles()``"
            , "``domino_tiles()``"
            , "``games_checkers()``"
            , "``checkers_games()``"
            , "``games_chess()``"
            , "``chess_games()``"
            , "``games_dominoes()``"
            , "``domino_games()``"
            , "``games_piecepack()``"
            , "``piecepack_games()``"
            , "``games_stackpack()``"
            , "``stackpack_games()``"
            , "``piecepack_piecepack_accordion()``"
            , "``piecepack_accordion()``"
            , "``piecepack_piecepack_halma()``"
            , "``piecepack_halma()``"
            , "``piecepack_piecepack_klondike()``"
            , "``piecepack_klondike()``"
            , "``piecepack_rect_board_tiles()``"
            , "``piecepack_rectangular_board()``"
    )
    names(df) <- c("**Deprecated function**", "**Replacement function**")
    knitr::kable(df, "pipe")
}
