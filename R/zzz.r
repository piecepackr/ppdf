#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom rlang .data %||% abort check_dots_empty
#' @importFrom tibble tibble tribble
#' @importFrom utils packageVersion
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
