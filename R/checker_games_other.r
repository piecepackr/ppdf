#' Setups for other games playable with a checkers set
#'
#' \code{tibble} data frames of setups for `r nrow(checker_games_other())` other games playable with a checkers set.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(checker_games_other())`
#'
#' @param cell_width Width of board cell.  Most renderers support `1` or `2`.
#' @name checker_games_other
#' @rdname checker_games_other
#' @return `r return_df()`
NULL

checker_games_other <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Breakthrough"
            , "``checker_breakthrough()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
            , "Crossings"
            , "``checker_crossings()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Crossings_(game)"
            , "Dao"
            , "``piecepack_dao()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/948/dao"
            , "Focus"
            , "``checker_focus()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Focus_(board_game)"
            , "Four Field Kono"
            , "``checker_four_field_kono()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Four_Field_Kono"
            , "Grasshopper"
            , "``checker_grasshopper()``"
            , NA_character_
            , "http://www.cyningstan.com/game/71/grasshopper"
            , "Jul-Gonu"
            , "``checker_julgonu()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Jul-Gonu"
            , "Lines of Action"
            , "``checker_lines_of_action()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Lines_of_Action"
            )
}

#' @rdname checker_games_other
#' @export
checker_breakthrough <- function(cell_width = 1)
    to_checkers(piecepack_breakthrough(), cell_width)

#' @rdname checker_games_other
#' @export
checker_crossings <- checker_breakthrough

#' @rdname checker_games_other
#' @export
checker_dao <- function(cell_width = 1)
    to_checkers(piecepack_dao(), cell_width)

#' @rdname checker_games_other
#' @export
checker_focus <- function(cell_width = 1) {
    df_board <- checker_board()
    df_w <- checker_bits(suit = 6L, x = rep(2:7, each = 3L),
                          y = c(rep(c(2, 4, 6), 2), rep(c(3, 5, 7), 2), rep(c(2, 4, 6), 2)))
    df_b <- checker_bits(suit = 1L, x = rep(2:7, each = 3L),
                          y = c(rep(c(3, 5, 7), 2), rep(c(2, 4, 6), 2), rep(c(3, 5, 7), 2)))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checker_games_other
#' @export
checker_four_field_kono <- function(cell_width = 1)
    to_checkers(piecepack_four_field_kono(), cell_width,
                piece_side = "board_back", suit = 2L)

#' @rdname checker_games_other
#' @export
checker_grasshopper <- function(cell_width = 1)
    to_checkers(piecepack_grasshopper(), cell_width)

#' @rdname checker_games_other
#' @export
checker_julgonu <- function(cell_width = 1)
    to_checkers(piecepack_julgonu(), cell_width, piece_side = "board_back", suit = 2L)

#' @rdname checker_games_other
#' @export
checker_lines_of_action <- function(cell_width = 1)
    to_checkers(piecepack_lines_of_action(), cell_width)
