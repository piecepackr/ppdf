#' @importFrom dplyr %>% bind_rows mutate slice
#' @importFrom rlang .data %||% abort check_dots_empty
#' @importFrom tibble tibble tribble
#' @importFrom utils hasName packageVersion
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L

less_than <- function(x, y) 1e-06 < y - x

assert_suggested <- function (package) {
    calling_fn <- deparse(sys.calls()[[sys.nframe() - 1]])
    if (!requireNamespace(package, quietly = TRUE)) {
        msg <- c(sprintf("You need to install the suggested package %s to use %s.",
            sQuote(package), sQuote(calling_fn)), i = sprintf("Use %s.",
            sQuote(sprintf("install.packages(\"%s\")", package))))
        abort(msg, class = "piecepackr_suggested_package")
    }
}

cat_piece <- function(df, ..., color = FALSE) {
    ppnames <- c("piece_side", "suit", "rank", "cfg", "x", "y", "angle")
    stopifnot(all(names(df) == ppnames), !anyNA(df), validate_types(df))
    ppcli::cat_piece(df, ..., color = color)
}

validate_types <- function(df) {
    pptypes <- c("character", "integer", "integer", "character", "numeric", "numeric", "numeric")
    all(vapply(df, class, FUN.VALUE = character(1L), USE.NAMES = FALSE) == pptypes)
}

validate_df <- function(df) {
    ppnames <- c("piece_side", "suit", "rank", "cfg", "x", "y", "angle")
    testthat::expect_true(all(names(df) == ppnames) && !anyNA(df) && validate_types(df))
}
