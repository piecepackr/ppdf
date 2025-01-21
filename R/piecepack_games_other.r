#' Setups for other modern games playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_other())` other games playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various other games:
#'
#' `r man_markdown_table(piecepack_games_other())`
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param coins String of coin layout
#' @rdname piecepack_games_other
#' @name piecepack_games_other
#' @return `r return_df()`
NULL

piecepack_games_other <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Breakthrough"
            , "``piecepack_breakthrough()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
            , "Crossings"
            , "``piecepack_crossings()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Crossings_(game)"
            , "Change Change"
            , "``piecepack_change_change()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/ChangeChange"
            , "Grasshopper"
            , "``piecepack_grasshopper()``"
            , NA_character_
            , "http://www.cyningstan.com/game/71/grasshopper"
            , "Evade"
            , "``piecepack_evade()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Evade"
            , "Lines of Action"
            , "``piecepack_lines_of_action()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Lines_of_Action"
            , "Quatri"
            , "``piecepack_quatri()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Quatri"
            )
}

#' @rdname piecepack_games_other
#' @export
piecepack_breakthrough <- piecepack_gothic_checkers

#' @rdname piecepack_games_other
#' @export
piecepack_change_change <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(coins)) {
       suits <- rep.int(1:4, c(1, 2, 4, 4))[sample.int(11L)]
    } else {
       suits <- process_suits(coins)
    }
    df <- piecepack_coins(side = "back",
                          x = c(1:4, 1:4, 1:3),
                          y = rep.int(3:1, c(4, 4, 3)),
                          suit = suits, rank = NA_integer_) %>%
        fill_piece_rank()
    df
}

#' @rdname piecepack_games_other
#' @export
piecepack_crossings <- piecepack_breakthrough

#' @rdname piecepack_games_other
#' @export
piecepack_evade <- function() piecepack_rectangular_board(ncols = 6, nrows = 6)

#' @rdname piecepack_games_other
#' @export
piecepack_grasshopper <- function() {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_c <- piecepack_coins(side = "back",
                   x = c(5:8, 6:8, 7:8, 8, 1, 1:2, 1:3, 1:4),
                   y = c(8,8,8,8, 7,7,7, 6,6, 5, 4, 3,3, 2,2,2, 1,1,1,1),
                   suit = c(1,2,1,2,1,2,1,1,2,1, 3,4,3,3,4,3,4,3,4,3),
                   rank = c(1,1,2,2,3,3,4,5,4,6, 1,1,2,3,2,4,3,5,4,6),
                   angle = rep(c(180, 0), each = 10)
    )
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_lines_of_action <- function() {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_c <- piecepack_coins(side = "back",
                   x = c(2:7, rep(8, 6), 2:7, rep(1, 6)),
                   y = c(rep(8, 6), 2:7, rep(1, 6), 2:7),
                   suit = rep(c(1,3,2,4), each = 6L),
                   rank = rep.int(1:6, 4L),
                   angle = rep(c(180, 90, 0, 270), each = 6L)
    )
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_quatri <- function() {
    df_tiles <- piecepack_rectangular_board(4L, 4L)
    df_coins <- piecepack_coins(side = "back",
                       x = rep(1:4, 2),
                       y = rep(c(4,1), each=4),
                       suit = c(1,2,1,2, 2,1,2,1),
                       rank = c(2,2,3,3, 5,5,4,4),
                       angle = c(180, 0, 180, 0, 0, 180, 0, 180)
    )
    bind_rows(df_tiles, df_coins)
}
