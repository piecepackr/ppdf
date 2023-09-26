#' Setups for other games playable with a checkers set
#'
#' \code{tibble} data frames of setups for `r nrow(games_checkers_other())` other games playable with a checkers set.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(games_checkers_other())`
#'
#' @param cell_width Width of board cell.  Most renderers support `1` or `2`.
#' @name checkers_games_other
#' @rdname checkers_games_other
NULL

games_checkers_other <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Breakthrough"
            , "``checkers_breakthrough()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
            , "Crossings"
            , "``checkers_crossings()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Crossings_(game)"
            , "Focus"
            , "``checkers_focus()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Focus_(board_game)"
            , "Four Field Kono"
            , "``checkers_four_field_kono()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Four_Field_Kono"
            , "Grasshopper"
            , "``checkers_grasshopper()``"
            , NA_character_
            , "http://www.cyningstan.com/game/71/grasshopper"
            , "Jul-Gonu"
            , "``checkers_julgonu()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Jul-Gonu"
            , "Lines of Action"
            , "``checkers_lines_of_action()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Lines_of_Action"
            )
}

#' @rdname checkers_games_other
#' @export
checkers_breakthrough <- function(cell_width = 1)
    to_checkers(piecepack_breakthrough(), cell_width)

#' @rdname checkers_games_other
#' @export
checkers_crossings <- checkers_breakthrough

#' @rdname checkers_games_other
#' @export
checkers_focus <- function(cell_width = 1) {
    df_board <- tibble(piece_side = "board_face", suit = 3, rank = 8,
                   x = 4.5, y = 4.5)
    df_w <- tibble(piece_side = "bit_back", suit = 6, rank = 1,
                   x = rep(c(2:7), each=3),
                   y = c(rep(c(2, 4, 6), 2), rep(c(3, 5, 7), 2), rep(c(2, 4, 6), 2)))
    df_b <- tibble(piece_side = "bit_back", suit = 1, rank = 1,
                   x = rep(c(2:7), each=3),
                   y = c(rep(c(3, 5, 7), 2), rep(c(2, 4, 6), 2), rep(c(3, 5, 7), 2)))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checkers_games_other
#' @export
checkers_four_field_kono <- function(cell_width = 1)
    to_checkers(piecepack_four_field_kono(), cell_width,
                piece_side = "board_back", suit = 2L)

#' @rdname checkers_games_other
#' @export
checkers_grasshopper <- function(cell_width = 1)
    to_checkers(piecepack_grasshopper(), cell_width)

#' @rdname checkers_games_other
#' @export
checkers_julgonu <- function(cell_width = 1)
    to_checkers(piecepack_julgonu(), cell_width, piece_side = "board_back", suit = 2L)

#' @rdname checkers_games_other
#' @export
checkers_lines_of_action <- function(cell_width = 1)
    to_checkers(piecepack_lines_of_action(), cell_width)
