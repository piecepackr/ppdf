#' Setups for checkers variants playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_checkers())` checkers variants playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various checkers variants:
#'
#' `r man_markdown_table(piecepack_games_checkers())`
#'
#' @rdname piecepack_games_checkers
#' @name piecepack_games_checkers
#' @return `r return_df()`
NULL

piecepack_games_checkers <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "(American) Checkers AKA (English) Draughts"
            , "``piecepack_american_checkers()``, ``piecepack_english_checkers()``, ``piecepack_checkers()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Checkers"
            , "American Pool Checkers"
            , "``piecepack_american_pool_checkers()``"
            , NA_character_
            , "https://draughts.github.io/american-pool-checkers.html"
            , "Bashni AKA Column Checkers"
            , "``piecepack_bashni()`` aka ``piecepack_column_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Bashni"
            , "Brazilian Checkers AKA Brazilian Draughts"
            , "``piecepack_brazilian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Brazilian_draughts"
            , "Czech Checkers AKA Czech Draughts"
            , "``piecepack_czech_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Czech_draughts"
            , "Corner Checkers AKA Corner Draughts"
            , "``piecepack_corner_checkers()``"
            , NA_character_
            , "https://brainking.com/en/GameRules?tp=30"
            , "Gothic Checkers AKA Gothic Draughts"
            , "``piecepack_gothic_checkers()``"
            , NA_character_
            , "http://mlwi.magix.net/bg/gothiccheckersvariants.htm"
            , "Italian Checkers AKA Italian Draughts"
            , "``piecepack_italian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Italian_draughts"
            , "Jamaican Checkers AKA Jamaican Draughts"
            , "``piecepack_jamaican_checkers()``"
            , NA_character_
            , "https://web.archive.org/web/20230605023244/http://poolcheckers.com/jamaica/"
            , "One Way Checkers AKA One Way Draughts"
            , "``piecepack_one_way_checkers()``"
            , NA_character_
            , "https://brainking.com/en/GameRules?tp=31"
            , "Portuguese Checkers AKA Portuguese Draughts"
            , "``piecepack_portuguese_checkers()``"
            , NA_character_
            , "http://www.fpdamas.pt/regras/"
            , "Russian Checkers AKA Russian Draughts"
            , "``piecepack_russian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Russian_draughts"
            , "Spanish Checkers AKA Spanish Draughts"
            , "``piecepack_spanish_checkers()``"
            , NA_character_
            , "https://mindsports.nl/index.php/on-the-evolution-of-draughts-variants/draughts-variants/497-dama_s"
            , "Thai Checkers AKA Thai Draughts AKA Mak-hot AKA Makhos"
            , "``piecepack_thai_checkers()``"
            , NA_character_
            , "https://checkers.fandom.com/wiki/Mak-hot"
            , "Turkish Checkers AKA Turkish Draughts AKA Dama"
            , "``piecepack_turkish_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Turkish_draughts"
            , "Unified Pool Checkers"
            , "``piecepack_unified_pool_checkers()``"
            , NA_character_
            , "https://wiegerw.github.io/pdn/gametype.html"
            , "Zimbabwean Pool Checkers"
            , "``piecepack_zimbabwean_pool_checkers()``"
            , NA_character_
            , "https://wiegerw.github.io/pdn/gametype.html"
            )
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_american_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_c <- piecepack_coins(side = "back",
                   x = c(2,4,1,3,2,4,  6,8,5,7,6,8,
                       5,7,6,8,5,7,  1,3,2,4,1,3),
                   y = rep(c(8,7,6,8,7,6,3,2,1,3,2,1), each = 2),
                   angle = rep(c(180,0), each = 12))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_american_pool_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_bashni <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_brazilian_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_column_checkers <- piecepack_bashni

#' @rdname piecepack_games_checkers
#' @export
piecepack_corner_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_c <- piecepack_coins(side = "back",
                   x = c(1,1,1,2,2,2,  3,3,4,4,5,6,
                       8,8,8,7,7,7,  6,6,5,5,4,3),
                   y = c(3,5,7,4,6,8,  5,7,6,8,7,8,
                         6,4,2,5,3,1,  4,2,3,1,2,1),
                   angle = rep(c(180,0), each = 12))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_czech_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_english_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_gothic_checkers <- function() {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_p <- piecepack_pawns(x = c(1, 8, 8, 1), y = c(7, 7, 2, 2),
                            angle = c(180, 180, 0, 0))
    df_d <- piecepack_dice(x = c(1, 8, 8, 1), y = c(8, 8, 1, 1),
                           angle = c(180, 180, 0, 0), rank = 2)
    df_c <- piecepack_coins(side = "back",
                            x = rep(c(2:7, 7:2), each=2),
                            y = c(rep(c(8, 7), 6), rep(c(2, 1), 6)),
                            angle = rep(c(180, 0), each=12))
    bind_rows(df_t, df_p, df_d, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_italian_checkers <- function() {
    df_t <- piecepack_rectangular_board(8L, 8L)
    df_c <- piecepack_coins(side = "back",
                   x = c(1,3,2,4,1,3,  5,7,6,8,5,7,
                       6,8,5,7,6,8,  2,4,1,3,2,4),
                   y = rep(c(8,7,6,8,7,6,3,2,1,3,2,1), each = 2),
                   angle = rep(c(180, 0), each = 12))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_jamaican_checkers <- piecepack_italian_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_one_way_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_c <- piecepack_coins(side = "back",
                   x = c(6,8,5,7,6,8,  5,7,6,8,5,7,
                       1,3,2,4,1,3,  2,4,1,3,2,4),
                   y = rep(c(6:1, 1:6), each = 2),
                   angle = rep(c(180, 0), each = 12))
    bind_rows(df_t, df_c)
}


#' @rdname piecepack_games_checkers
#' @export
piecepack_portuguese_checkers <- piecepack_italian_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_russian_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_spanish_checkers <- piecepack_italian_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_thai_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_c <- piecepack_coins(side = "back",
                   suit = rep(1:4, each = 4),
                   rank = rep(1:4, 4),
                   x = c(2,4,1,3,  6,8,5,7,
                       6,8,5,7,  2,4,1,3),
                   y = rep(c(8,7,8,7,2,1,2,1), each = 2),
                   angle = rep(c(180,0), each = 8))
    bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_turkish_checkers <- function() {
    df_t <- piecepack_rectangular_board(8, 8)
    df_p <- piecepack_pawns(x = c(1, 8, 8, 1), y = c(6, 6, 3, 3), angle = c(180, 180, 0, 0))
    df_d <- piecepack_dice(x = c(1, 8, 8, 1), y = c(7, 7, 2, 2),
                           angle = c(180, 180, 0, 0), rank = 2L)
    df_c <- piecepack_coins(side = "back",
                            x = rep(c(2:7, 7:2), each=2),
                            y = c(rep(c(7, 6), 6), rep(c(3, 2), 6)),
                            angle = rep(c(180, 0), each=12))
    bind_rows(df_t, df_p, df_d, df_c)
}

#' @rdname piecepack_games_checkers
#' @export
piecepack_unified_pool_checkers <- piecepack_american_checkers

#' @rdname piecepack_games_checkers
#' @export
piecepack_zimbabwean_pool_checkers <- piecepack_american_checkers
