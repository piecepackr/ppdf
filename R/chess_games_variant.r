#' Setups for chess variants
#'
#' \code{tibble} data frames of setups for `r nrow(chess_games_variant())` chess variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(chess_games_variant())`
#'
#' @param cell_width Width of board cell.  Most renderers support `1` or `2`.
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param ... Should be left empty.
#' @name chess_games_variant
#' @rdname chess_games_variant
#' @return `r return_df()`
NULL

chess_games_variant <- function() {
    tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "(International) Chess"
            , "``chess_international_chess()`` aka ``chess_chess()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Chess"
            , "Fischer Random Chess AKA Chess960"
            , "``chess_fischer_random_chess()`` aka ``chess_chess960()``"
            , NA_character_
            , "https://www.chessvariants.com/diffsetup.dir/fischer.html"
            , "Horde Chess"
            , "``chess_horde_chess()``"
            , NA_character_
            , "https://lichess.org/variant/horde"
            , "Monochrome Chess"
            , "``chess_monochrome_chess()``"
            , NA_character_
            , "https://wunderland.com/WTS/Andy/Games/monochess.html"
            , "Racing Kings"
            , "``chess_racing_kings()``"
            , NA_character_
            , "https://www.chessvariants.com/diffobjective.dir/racing.html"
    )
}

#' @rdname chess_games_variant
#' @export
chess_chess <- function(cell_width = getOption("ppdf.chess_cell_width", 1)) {
    force(cell_width)
    local_options(ppdf.chess_cell_width = NULL)
    df_board <- chess_board()
    df_w <- chess_bits(suit = 6L,
                       rank = c(rep(1L, 8L), 4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                       x = rep(1:8, 2L),
                       y = rep(2:1, each = 8L))
    df_b <- chess_bits(suit = 2L,
                       rank = c(rep(1L, 8L), 4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                       x = rep(1:8, 2L),
                       y = rep(7:8, each = 8L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "chess")
}

#' @rdname chess_games_variant
#' @export
chess_chess960 <- function(seed = NULL, cell_width = getOption("ppdf.chess_cell_width", 1)) {
    if (!is.null(seed)) withr::local_seed(seed)
    force(cell_width)
    local_options(ppdf.chess_cell_width = NULL)
    df_board <- chess_board()
    ranks <- fischer_random_ranks()
    df_w <- chess_bits(suit = 6L,
                       rank = c(rep(1L, 8L), ranks),
                       x = rep(1:8, 2L),
                       y = rep(2:1, each = 8L))
    df_b <- chess_bits(suit = 2L,
                       rank = c(rep(1L, 8L), ranks),
                       x = rep(1:8, 2L),
                       y = rep(7:8, each = 8L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "chess")
}

#' @rdname chess_games_variant
#' @export
chess_fischer_random_chess <- chess_chess960

# From: https://www.chessvariants.com/diffsetup.dir/fischer.html
# > Terumi Kaneyasu (Sam Sloan?) writes:
# >     Fischer Random Chess has 960 legal arrays. This number is determined as follows:
# >     First, place the two Bishops. There are 16 different ways for one bishop to be on a white square and the other Bishop to be on a black square.
# >     That leaves six empty squares. Now, place the King somewhere between the two Rooks. There are 20 different ways for a King and two Rooks to occupy six squares with the King in between.
# >     That leaves three squares for the two Knights and the Queen. There are three possible ways to place these pieces.
# >     Thus, there are 16 x 20 x 3 (960) legal arrays in Fischer Random Chess.
fischer_random_ranks <- function() {
    cells <- 1:8
    bb <- seq.int(1, 7, 2)[sample.int(4, 1)] # black bishop
    wb <- seq.int(2, 8, 2)[sample.int(4, 1)] # white bishop
    cells <- setdiff(cells, c(bb, wb))
    rkr <- sort(cells[sample.int(6, 3)]) # rook, king, rook
    nnq <- sample(setdiff(cells, rkr), 3L) # knight, knight, queen
    df <- data.frame(cell = c(bb, wb, rkr, nnq),
                     rank = c(3L, 3L, 4L, 6L, 4L, 2L, 2L, 5L))
    arrange(df, .data$cell)$rank
}

#' @rdname chess_games_variant
#' @export
chess_horde_chess <- function(..., cell_width = getOption("ppdf.chess_cell_width", 1)) {
    check_dots_empty()
    force(cell_width)
    local_options(ppdf.chess_cell_width = NULL)
    df_board <- chess_board()
    df_w <- chess_bits(suit = 6L, rank = 1L,
                       x = c(rep(1:8, 4L), 2:3, 6:7),
                       y = c(rep(1:4, each = 8L), rep_len(5L, 4L)))
    df_b <- chess_bits(suit = 2L,
                       rank = c(rep(1L, 8L), 4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                       x = rep(1:8, 2L),
                       y = rep(7:8, each = 8L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "chess")

}


#' @rdname chess_games_variant
#' @export
chess_international_chess <- chess_chess

#' @rdname chess_games_variant
#' @export
chess_monochrome_chess <- function(cell_width = getOption("ppdf.chess_cell_width", 1)) {
    force(cell_width)
    local_options(ppdf.chess_cell_width = NULL)
    df_board <- chess_board()
    df_b <- chess_bits(suit = 6L,
                       rank = c(rep(1L, 8L), 4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                       x = rep(1:8, 2L),
                       y = rep(2:1, each = 8L))
    df_t <- chess_bits(suit = 6L,
                       rank = c(rep(1L, 8L), 4L, 2L, 3L, 5L, 6L, 3L, 2L, 4L),
                       x = rep(1:8, 2L),
                       y = rep(7:8, each = 8L))
    bind_rows(df_board, df_b, df_t) %>%
        set_cell_width(cell_width, "chess")
}

#' @rdname chess_games_variant
#' @export
chess_racing_kings <- function(cell_width = getOption("ppdf.chess_cell_width", 1)) {
    force(cell_width)
    local_options(ppdf.chess_cell_width = NULL)
    df_board <- chess_board()
    df_w <- chess_bits(suit = 6L,
                       rank = c(2L, 3L, 4L, 6L, 2L, 3L, 4L, 5L),
                       x = rep(5:8, 2L),
                       y = rep(2:1, each = 4L))
    df_b <- chess_bits(suit = 2L,
                       rank = c(6L, 4L, 3L, 2L, 5L, 4L, 3L, 2L),
                       x = rep(1:4, 2L),
                       y = rep(2:1, each = 4L))
    bind_rows(df_board, df_w, df_b) %>%
        set_cell_width(cell_width, "chess")
}
