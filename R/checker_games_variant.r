#' Setups for checkers variants
#'
#' \code{tibble} data frames of setups for `r nrow(checker_games_variant())` checkers variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(checker_games_variant())`
#'
#' @param cell_width Width of board cell.  Most renderers support `1` or `2`.
#' @name checker_games_variant
#' @rdname checker_games_variant
#' @return `r return_df()`
NULL

checker_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "(American) Checkers AKA (English) Draughts"
            , "``checker_american_checkers()`` aka ``checker_english_checkers()`` aka ``checker_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Draughts"
            , "American Pool Checkers"
            , "``checker_american_pool_checkers()``"
            , NA_character_
            , "https://draughts.github.io/american-pool-checkers.html"
            , "Bashni AKA Column Checkers"
            , "``checker_bashni()`` aka ``checker_column_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Bashni"
            , "Brazilian Checkers AKA Brazilian Draughts"
            , "``checker_brazilian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Brazilian_draughts"
            , "Canadian Checkers AKA Canadian Draughts"
            , "``checker_canadian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Canadian_checkers"
            , "Corner Checkers AKA Corner Draughts"
            , "``checker_corner_checkers()``"
            , NA_character_
            , "https://brainking.com/en/GameRules?tp=30"
            , "Czech Checkers AKA Czech Draughts"
            , "``checker_czech_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Czech_draughts"
            , "Dameo"
            , "``checker_dameo()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Dameo"
            , "Frisian Checkers AKA Frisian Draughts"
            , "``checker_frisian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Frisian_draughts"
            , "Gothic Checkers AKA Gothic Draughts"
            , "``checker_gothic_checkers()``"
            , NA_character_
            , "http://mlwi.magix.net/bg/gothiccheckersvariants.htm"
            , "International Checkers AKA International Draughts"
            , "``checker_international_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/International_draughts"
            , "Italian Checkers AKA Italian Draughts"
            , "``checker_italian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Italian_draughts"
            , "Jamaican Checkers AKA Jamaican Draughts"
            , "``checker_jamaican_checkers()``"
            , NA_character_
            , "https://web.archive.org/web/20230605023244/http://poolcheckers.com/jamaica/"
            , "Lasca"
            , "``checker_lasca()``"
            , NA_character_
            , "http://www.lasca.org"
            , "Malaysian Checkers AKA Singaporean Checkers"
            , "``checker_malaysian_checkers()`` aka ``checker_singaporean_checkers()"
            , NA_character_
            , "https://brainking.com/en/GameRules?tp=31"
            , "One Way Checkers AKA One Way Draughts"
            , "``checker_one_way_checkers()``"
            , NA_character_
            , "https://brainking.com/en/GameRules?tp=31"
            , "Portuguese Checkers AKA Portuguese Draughts"
            , "``checker_portuguese_checkers()``"
            , NA_character_
            , "http://www.fpdamas.pt/regras/"
            , "Russian Checkers AKA Russian Draughts"
            , "``checker_russian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Russian_draughts"
            , "Spanish Checkers AKA Spanish Draughts"
            , "``checker_spanish_checkers()``"
            , NA_character_
            , "https://mindsports.nl/index.php/on-the-evolution-of-draughts-variants/draughts-variants/497-dama_s"
            , "Thai Checkers AKA Thai Draughts AKA Mak-hot AKA Makhos"
            , "``checker_thai_checkers()``"
            , NA_character_
            , "https://checkers.fandom.com/wiki/Mak-hot"
            , "Turkish Checkers AKA Turkish Draughts AKA Dama"
            , "``checker_turkish_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Turkish_draughts"
            , "Unified Pool Checkers"
            , "``checker_unified_pool_checkers()``"
            , NA_character_
            , "https://wiegerw.github.io/pdn/gametype.html"
            , "Zimbabwean Pool Checkers"
            , "``checker_zimbabwean_pool_checkers()``"
            , NA_character_
            , "https://wiegerw.github.io/pdn/gametype.html"
    )
}

#' @rdname checker_games_variant
#' @export
checker_american_checkers <- function(cell_width = 1)
    to_checkers(piecepack_american_checkers(), cell_width)

#' @rdname checker_games_variant
#' @export
checker_american_pool_checkers <- function(cell_width = 1)
    to_checkers(piecepack_american_checkers(), cell_width, black_first = TRUE)

#' @rdname checker_games_variant
#' @export
checker_bashni <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_brazilian_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_canadian_checkers <- function(cell_width = 1) {
    df_board <- checker_board(12L)
    df_w <- checker_bits(suit = 6L,
                          x = c(rep(seq.int(1L, 11L, 2L), 3L),
                              rep(seq.int(2L, 12L, 2L), 2L)),
                          y = rep(c(1, 3, 5, 2, 4), each = 6L))
    df_b <- checker_bits(suit = 2L,
                          x = c(rep(seq.int(1L, 11L, 2L), 2L),
                                rep(seq.int(2L, 12L, 2L), 3L)),
                          y = rep(c(9, 11, 8, 10, 12), each = 6L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checker_games_variant
#' @export
checker_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_column_checkers <- checker_bashni

#' @rdname checker_games_variant
#' @export
checker_corner_checkers <- function(cell_width = 1)
    to_checkers(piecepack_corner_checkers(), cell_width)

#' @rdname checker_games_variant
#' @export
checker_czech_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_dameo <- function(cell_width = 1) {
    df_board <- checker_board(8L)
    df_w <- checker_bits(suit = 6L,
                          x = c(1:8, 2:7, 3:6),
                          y = rep.int(1:3, c(8, 6, 4)))
    df_b <- checker_bits(suit = 2L,
                          x = c(3:6, 2:7, 1:8),
                          y = rep.int(6:8, c(4, 6, 8)))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checker_games_variant
#' @export
checker_english_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_frisian_checkers <- function(cell_width = 1) {
    df_board <- checker_board(10L)
    df_w <- checker_bits(suit = 6L,
                          x = rep(c(seq.int(1L, 9L, 2L), seq.int(2L, 10L, 2L)), 2L),
                          y = rep(1:4, each = 5L))
    df_b <- checker_bits(suit = 2L,
                          x = rep(c(seq.int(1L, 9L, 2L), seq.int(2L, 10L, 2L)), 2L),
                          y = rep(7:10, each = 5L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checker_games_variant
#' @export
checker_gothic_checkers <- checker_crossings

#' @rdname checker_games_variant
#' @export
checker_international_checkers <- checker_frisian_checkers

#' @rdname checker_games_variant
#' @export
checker_italian_checkers <- function(cell_width = 1)
    to_checkers(piecepack_italian_checkers(), cell_width, angle = 90)

#' @rdname checker_games_variant
#' @export
checker_jamaican_checkers <- checker_italian_checkers

#' @rdname checker_games_variant
#' @export
checker_lasca <- function(cell_width = 1) {
    df_board <- checker_board(7L)
    df_w <- checker_bits(suit = 6L,
                          x = c(5,7,4,6,5,7,  1,3,2,1,3),
                          y = c(3,3,2,2,1,1,  3,3,2,1,1))
    df_b <- checker_bits(suit = 2L,
                          x = c(1,3,2,4,1,3,  5,7,6,5,7),
                          y = c(7,7,6,6,5,5,  7,7,6,5,5))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checker_games_variant
#' @export
checker_malaysian_checkers <- checker_canadian_checkers

#' @rdname checker_games_variant
#' @export
checker_one_way_checkers <- function(cell_width = 1)
    to_checkers(piecepack_one_way_checkers(), cell_width)

#' @rdname checker_games_variant
#' @export
checker_portuguese_checkers <- checker_italian_checkers

#' @rdname checker_games_variant
#' @export
checker_russian_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_singaporean_checkers <- checker_malaysian_checkers

#' @rdname checker_games_variant
#' @export
checker_spanish_checkers <- checker_italian_checkers

#' @rdname checker_games_variant
#' @export
checker_thai_checkers <- function(cell_width = 1)
    to_checkers(piecepack_thai_checkers(), cell_width, black_first = TRUE)

#' @rdname checker_games_variant
#' @export
checker_turkish_checkers <- function(cell_width = 1)
    to_checkers(piecepack_turkish_checkers(), cell_width,
                piece_side = "board_back", suit = 2L)

#' @rdname checker_games_variant
#' @export
checker_unified_pool_checkers <- checker_american_checkers

#' @rdname checker_games_variant
#' @export
checker_zimbabwean_pool_checkers <- function(cell_width = 1)
    to_checkers(piecepack_zimbabwean_pool_checkers(), cell_width, angle = 90)
