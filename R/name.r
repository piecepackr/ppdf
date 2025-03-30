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
    x <- gsub(paste0("^checker", sep), "", x)
    x <- gsub(paste0("^chess", sep), "", x)
    x <- gsub(paste0("^domino", sep), "", x)
    x <- gsub(paste0("^icehouse", sep), "", x)
    x <- gsub(paste0("^go", sep), "", x)
    x <- gsub(paste0("^piecepack", sep), "", x)
    x <- gsub(paste0("^stackpack", sep), "", x)
    x <- gsub(paste0("^reversi", sep), "", x)
    x <- gsub(paste0("^tarot", sep), "", x)
    x
}

known_game_systems <- c(
    "piecepack", "stackpack", "piecepack_stackpack",
    "alquerque",
    "checkers", "draughts",
    "chess",
    "domino", "dominoes",
    "go",
    "icehouse", "icehouse_pieces", "icehouse_pyramids", "looney_pyramids",
    "marble", "marbles",
    "morris", "reversi", "tarot"
)

normalize_system <- function(system) {
    system_ <- normalize_name(system[1L])
    if (system_ %in% c("pieces", "pyramids") && grepl("icehouse", system[1L], ignore.case = TRUE))
        system_ <- "icehouse"
    switch(system_,
           alquerque = "alquerque",
           checker = "checker",
           checkers = "checker",
           draughts = "checker",
           chess = "chess",
           domino = "domino",
           dominoes = "domino",
           go = "go",
           icehouse = "icehouse",
           looney_pyramids = "icehouse",
           marble  = "marble",
           marbles = "marble",
           morris = "morris",
           piecepack = "piecepack",
           piecepack_stackpack = "stackpack",
           reversi = "reversi",
           stackpack = "stackpack",
           tarot = "tarot",
           {
               rlang::inform(
                   c(paste("Don't recognize game system", sQuote(system), "yet."),
                     paste("Normalizing as", sQuote(system_), "for now."),
                     paste("This normalization may change when this game system is recognized by {ppdf}.")),
                   class = "unrecognized_game_system"
               )
               system
           })
}

#' Setups by game name
#'
#' `setup_by_name()` returns setup data frames by game name and game system.
#' `alquerque_setup_by_name()`, `checker_setup_by_name()`, `chess_setup_by_name()`,
#' `dominoes_setup_by_name()`, `marble_setup_by_name()`, `piecepack_setup_by_name()`
#' and `stackpack_setup_by_name()` are aliases that set the game system.
#' @param name Game name.  Will be normalized by [normalize_name()].
#' @param system Game system.
#' @param ... Additional arguments to pass to the underlying setup function.
#' @param getter Function with a single function name argument to use.
#'               By default will just look for functions in this package but
#'               a developer may want to consider [dynGet()] or a
#'               wrapper around [get()] with a custom `envir` argument.
#' @return `r return_df()`
#' @export
setup_by_name <- function(name, system = known_game_systems, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    game <- normalize_name(name)
    system <- normalize_system(system)
    fn_name <- paste0(system, "_", game)
    # fall back on piecepack setups if no special setup exists
    tryCatch({
                 fn <- getter(fn_name)
                 do.call(fn, list(...))
    }, error = function(e1) {
             tryCatch({
                          fn_name <- paste0("piecepack", "_", game)
                          fn <- getter(fn_name)
                          do.call(fn, list(...))
                      }, error = function(e2) stop(e1))
    })
}

#' @rdname setup_by_name
#' @export
alquerque_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "alquerque", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
checker_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "checker", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
chess_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "chess", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
domino_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "domino", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
go_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "go", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
marble_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "marble", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
morris_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "morris", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
piecepack_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "piecepack", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
reversi_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "reversi", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
stackpack_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "stackpack", ..., getter = getter)
}

#' @rdname setup_by_name
#' @export
tarot_setup_by_name <- function(name, ...,
                          getter = function(x) get(x, envir=getNamespace("ppdf"))) {
    setup_by_name(name, "tarot", ..., getter = getter)
}
