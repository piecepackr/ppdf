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
