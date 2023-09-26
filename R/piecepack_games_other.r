#' Setups for other modern games playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(games_piecepack_other())` other games playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various other games:
#'
#' `r man_markdown_table(games_piecepack_other())`
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param coins String of coin layout
#' @rdname piecepack_games_other
#' @name piecepack_games_other
NULL

games_piecepack_other <- function() {
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
piecepack_breakthrough <- function() {
    df_t <- piecepack_rect_board_tiles(8, 8)
    df_p <- tibble(piece_side = "pawn_face", x = c(1, 8, 8, 1),
                   y = c(7, 7, 2, 2), angle = c(180, 180, 0, 0), suit = 1:4)
    df_d <- tibble(piece_side = "die_face", x = c(1, 8, 8, 1),
                   y = c(8, 8, 1, 1), angle = c(180, 180, 0, 0),
                   suit = 1:4, rank = 2)
    df_c <- tibble(piece_side = "coin_back", x = rep(c(2:7, 7:2), each=2),
                   y = c(rep(c(8, 7), 6), rep(c(2, 1), 6)),
                   angle = rep(c(180, 0), each=12),
                   suit = rep(1:4, each = 6), rank = rep(1:6, 4))
    bind_rows(df_t, df_p, df_d, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_change_change <- function(seed = NULL, coins = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    if (is.null(coins)) {
       suits <- rep.int(1:4, c(1, 2, 4, 4))[sample.int(11L)]
    } else {
       suits <- process_suits(coins)
    }
    df <- tibble(piece_side = "coin_back",
                 x = c(1:4, 1:4, 1:3),
                 y = rep.int(3:1, c(4, 4, 3)),
                 suit = suits)
    # Plausible coin ranks
    df <- df %>%
        group_by(.data$suit) %>%
        mutate(rank = sample.int(n())) %>%
        ungroup()
    df
}

#' @rdname piecepack_games_other
#' @export
piecepack_crossings <- piecepack_breakthrough

#' @rdname piecepack_games_other
#' @export
piecepack_evade <- function() piecepack_rect_board_tiles(ncols = 6, nrows = 6)

#' @rdname piecepack_games_other
#' @export
piecepack_grasshopper <- function() {
    df_t <- piecepack_rect_board_tiles(8, 8)
    df_c <- tibble(piece_side = "coin_back",
                   x = c(5:8, 6:8, 7:8, 8, 1, 1:2, 1:3, 1:4),
                   y = c(8,8,8,8, 7,7,7, 6,6, 5, 4, 3,3, 2,2,2, 1,1,1,1),
                   suit = c(1,2,1,2,1,2,1,1,2,1, 3,4,3,3,4,3,4,3,4,3),
                   rank = c(1,1,2,2,3,3,4,5,4,6, 1,1,2,3,2,4,3,5,4,6),
                   angle = rep(c(180, 0), each = 10))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_lines_of_action <- function() {
    df_t <- piecepack_rect_board_tiles(8, 8)
    df_c <- tibble(piece_side = "coin_back",
                   x = c(2:7, rep(8, 6), 2:7, rep(1, 6)),
                   y = c(rep(8, 6), 2:7, rep(1, 6), 2:7),
                   suit = rep(c(1,3,2,4), each = 6), rank = rep(1:6, 4),
                   angle = rep(c(180, 90, 0, 270), each = 6))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_quatri <- function() {
    df_tiles <- piecepack_rect_board_tiles(nrows=4, ncols=4)
    df_coins <- tibble(piece_side = "coin_back",
                       x = rep(1:4, 2), y = rep(c(4,1), each=4),
                       suit = c(1,2,1,2, 2,1,2,1),
                       rank = c(2,2,3,3, 5,5,4,4),
                       angle = c(180, 0, 180, 0, 0, 180, 0, 180))
    bind_rows(df_tiles, df_coins)
}
