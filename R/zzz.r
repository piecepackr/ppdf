#' @importFrom dplyr %>% bind_rows mutate
#' @importFrom rlang .data %||% abort
#' @importFrom tibble tibble tribble
#' @importFrom utils packageVersion
NULL

hasName <- function(x, name) match(name, names(x), nomatch = 0L) > 0L

less_than <- function(x, y) 1e-06 < y - x
