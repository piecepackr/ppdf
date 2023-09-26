#' Setups for dominoes variants
#'
#' \code{tibble} data frames of setups for `r nrow(games_dominoes_variant())` dominoes variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(games_dominoes_variant())`
#'
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @name dominoes_games_variant
#' @rdname dominoes_games_variant
NULL

games_dominoes_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "(Domino) Fuji-san"
            , "``dominoes_fujisan()``"
            , NA_character_
            , "http://donkirkby.github.io/donimoes/rules.html#fujisan"
    )
}

#' @rdname dominoes_games_variant
#' @export
dominoes_fujisan <- function(seed = NULL) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_tiles <- dominoes_double6() %>%
        filter(.data$suit != .data$rank, .data$suit != 7, .data$rank != 7)
    df_tiles <- df_tiles[sample.int(15), ]
    df_tiles$x <- c(7.5, 7.5, 7.5, seq(2, 13, 1))
    df_tiles$y <- c(c(0.5, 1.5, 2.5), rep_len(1.5, 12))
    df_tiles$angle <- c(90, 90, 90, sample(c(0, 180), 12, replace = TRUE))
    df_tiles[1:3, "piece_side"] <- "tile_back"
    #### Where best to get pawns from?
    df_pawns <- tibble(piece_side = "pawn_face", suit = c(4, 1, 3, 2),
                       x = rep(c(1, 14), each = 2), y = rep(1:2, 2),
                       cfg = "piecepack")
    bind_rows(df_tiles, df_pawns)
}

dominoes_double6 <- function(piece_side = "tile_face") {
    tibble(piece_side = piece_side,
           suit = rep.int(1:7, 7:1),
           rank = c(1:7, 2:7, 3:7, 4:7, 5:7, 6:7, 7),
           cfg = "dominoes")
}
