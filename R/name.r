#' Normalize name(s)
#'
#' `normalize_name()` normalizes (game) name(s).
#'
#' `normalize_name(x, sep = "_") is used to help create R variable names
#' while `normalize_name(x, sep = "-")` is used to help create filenames and LaTeX labels.
#'
#' @param x Name to normalize.
#' @param sep Separator character.
#' @return Character vector of normalized names.
#' @examples
#'   normalize_name("Fuji-san")
#'   normalize_name("Nine Men's Morris", sep = "-")
#' @export
normalize_name <- function(x, sep = "_") {
    assert_suggested("snakecase")
    x <- gsub('"|\'|-', "", x) # e.g. The "In" Crowd -> the_in_crowd
    x <- snakecase::to_snake_case(x, sep_out = sep, numerals = "left")
    x <- gsub("draughts", "checkers", x)
    x
}

known_game_systems <- c("piecepack",
                  "checkers", "draughts",
                  "chess",
                  "dominoes",
                  "icehouse", "icehouse_pieces", "looney_pyramids",
                  "stackpack", "piecepack_stackpack")

normalize_system <- function(system) {
    system <- normalize_name(system[1])
    switch(system,
           checkers = "checkers",
           draughts = "checkers",
           chess = "chess",
           dominoes = "dominoes",
           icehouse = "icehouse",
           icehouse_pieces = "icehouse",
           looney_pyramids = "icehouse",
           piecepack = "piecepack",
           piecepack_stackpack = "stackpack",
           stackpack = "stackpack",
           {
               rlang::inform(
                   c(paste("Don't recognize game system", sQuote(system), "yet."),
                     paste("Normalizing as", sQuote(system), "for now."),
                     paste("This normalization may change when this game system is recognized by {ppdf}.")),
                   class = "unrecognized_game_system"
               )
               system
           })
}

#' Setups by game name
#'
#' `setup_by_name()` returns setup data frames by game name and game system.
#' `checkers_by_name()`, `chess_by_name()`, `dominoes_by_name()`, and `piecepack_by_name()` are
#' aliases that set the game system.
#' @param name Game name.  Will be normalized by [normalize_name()].
#' @param system Game system.
#' @param ... Additional arguments to pass to the underlying setup function.
#' @param getter Function with a single function name argument to use.
#'               By default will just look for packages in this package but
#'               a developer may want to consider [dynGet()] or a
#'               wrapper around [get()] with a custom `envir` argument.
#' @return `r return_df()`
#' @export
setup_by_name <- function(name, system = known_game_systems, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    game <- normalize_name(name)
    system <- normalize_system(system)
    fn_name <- paste0(system, "_", game)
    fn <- getter(fn_name)
    do.call(fn, list(...))
}

#' @rdname setup_by_name
#' @export
checkers_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "checkers", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
chess_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "chess", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
dominoes_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "dominoes", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
piecepack_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "piecepack", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
stackpack_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "stackpack", ..., getter = getter)
}
