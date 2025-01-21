#' Setups for checkers variants
#'
#' \code{tibble} data frames of setups for `r nrow(checkers_games_variant())` checkers variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(checkers_games_variant())`
#'
#' @param cell_width Width of board cell.  Most renderers support `1` or `2`.
#' @name checkers_games_variant
#' @rdname checkers_games_variant
#' @return `r return_df()`
NULL

checkers_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "(American) Checkers AKA (English) Draughts"
            , "``checkers_american_checkers()`` aka ``checkers_english_checkers()`` aka ``checkers_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Draughts"
            , "American Pool Checkers"
            , "``checkers_american_pool_checkers()``"
            , NA_character_
            , "https://draughts.github.io/american-pool-checkers.html"
            , "Brazilian Checkers AKA Brazilian Draughts"
            , "``checkers_brazilian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Brazilian_draughts"
            , "Canadian Checkers AKA Canadian Draughts"
            , "``checkers_canadian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Canadian_checkers"
            , "Czech Checkers AKA Czech Draughts"
            , "``checkers_czech_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Czech_draughts"
            , "Dameo"
            , "``checkers_dameo()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Dameo"
            , "Frisian Checkers AKA Frisian Draughts"
            , "``checkers_frisian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Frisian_draughts"
            , "Gothic Checkers AKA Gothic Draughts"
            , "``checkers_gothic_checkers()``"
            , NA_character_
            , "http://mlwi.magix.net/bg/gothiccheckersvariants.htm"
            , "International Checkers AKA International Draughts"
            , "``checkers_international_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/International_draughts"
            , "Italian Checkers AKA Italian Draughts"
            , "``checkers_italian_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Italian_draughts"
            , "Jamaican Checkers AKA Jamaican Draughts"
            , "``checkers_jamaican_checkers()``"
            , NA_character_
            , "https://web.archive.org/web/20230605023244/http://poolcheckers.com/jamaica/"
            , "Portuguese Checkers AKA Portuguese Draughts"
            , "``checkers_portuguese_checkers()``"
            , NA_character_
            , "http://www.fpdamas.pt/regras/"
            , "Russian Checkers AKA Russian Draughts"
            , "``checkers_russian_checkers()``"
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
            , "``checkers_turkish_checkers()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Turkish_draughts"
    )
}

#' @rdname checkers_games_variant
#' @export
checkers_american_checkers <- function(cell_width = 1)
    to_checkers(piecepack_american_checkers(), cell_width)

#' @rdname checkers_games_variant
#' @export
checkers_american_pool_checkers <- function(cell_width = 1)
    to_checkers(piecepack_american_checkers(), cell_width, black_first = TRUE)

#' @rdname checkers_games_variant
#' @export
checkers_brazilian_checkers <- checkers_american_checkers

#' @rdname checkers_games_variant
#' @export
checkers_canadian_checkers <- function(cell_width = 1) {
    df_board <- checkers_board(12L)
    df_w <- checkers_bits(suit = 6L,
                          x = c(rep(seq.int(1L, 11L, 2L), 3L),
                              rep(seq.int(2L, 12L, 2L), 2L)),
                          y = rep(c(1, 3, 5, 2, 4), each = 6L))
    df_b <- checkers_bits(suit = 1L,
                          x = c(rep(seq.int(1L, 11L, 2L), 2L),
                                rep(seq.int(2L, 12L, 2L), 3L)),
                          y = rep(c(9, 11, 8, 10, 12), each = 6L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checkers_games_variant
#' @export
checkers_checkers <- checkers_american_checkers

#' @rdname checkers_games_variant
#' @export
checkers_czech_checkers <- checkers_american_checkers

#' @rdname checkers_games_variant
#' @export
checkers_dameo <- function(cell_width = 1) {
    df_board <- checkers_board(8L)
    df_w <- checkers_bits(suit = 6L,
                          x = c(1:8, 2:7, 3:6),
                          y = rep.int(1:3, c(8, 6, 4)))
    df_b <- checkers_bits(suit = 1L,
                          x = c(3:6, 2:7, 1:8),
                          y = rep.int(6:8, c(4, 6, 8)))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checkers_games_variant
#' @export
checkers_english_checkers <- checkers_american_checkers

#' @rdname checkers_games_variant
#' @export
checkers_frisian_checkers <- function(cell_width = 1) {
    df_board <- checkers_board(10L)
    df_w <- checkers_bits(suit = 6L,
                          x = rep(c(seq.int(1L, 9L, 2L), seq.int(2L, 10L, 2L)), 2L),
                          y = rep(1:4, each = 5L))
    df_b <- checkers_bits(suit = 1L,
                          x = rep(c(seq.int(1L, 9L, 2L), seq.int(2L, 10L, 2L)), 2L),
                          y = rep(7:10, each = 5L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "checkers")
}

#' @rdname checkers_games_variant
#' @export
checkers_gothic_checkers <- checkers_crossings

#' @rdname checkers_games_variant
#' @export
checkers_international_checkers <- checkers_frisian_checkers

#' @rdname checkers_games_variant
#' @export
checkers_italian_checkers <- function(cell_width = 1)
    to_checkers(piecepack_italian_checkers(), cell_width, angle = 90)

#' @rdname checkers_games_variant
#' @export
checkers_jamaican_checkers <- checkers_italian_checkers

#' @rdname checkers_games_variant
#' @export
checkers_portuguese_checkers <- checkers_italian_checkers

#' @rdname checkers_games_variant
#' @export
checkers_russian_checkers <- checkers_american_checkers

#' @rdname checkers_games_variant
#' @export
checkers_spanish_checkers <- checkers_italian_checkers

#' @rdname checkers_games_variant
#' @export
checkers_thai_checkers <- function(cell_width = 1)
    to_checkers(piecepack_thai_checkers(), cell_width, black_first = TRUE)

#' @rdname checkers_games_variant
#' @export
checkers_turkish_checkers <- function(cell_width = 1)
    to_checkers(piecepack_turkish_checkers(), cell_width,
                piece_side = "board_back", suit = 2L)
