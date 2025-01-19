set_cell_width <- function(df, cell_width, system) {
    if (is.null(cell_width)) return(df)
    df <- mutate(df, cfg = paste0(system, cell_width),
           x = cell_width * .data$x,
           y = cell_width * .data$y) %>% 
        select_piece()
    attr(df, "scale_factor") <- cell_width
    df
}

# function that converts piecepack game to checkers
# use suit = 1 (red) for "black"
# use suit = 6 (white) for "white"
# use green checkered board if checkered board and black lined board if not checkered
to_checkers <- function(df, cell_width = 1, ..., n_players = 2, black_first = FALSE) {
    white <- 6L
    black <- 1L # red actually
    dft <- filter(df, grepl("tile", .data$piece_side))
    width <- max(dft$x) - min(dft$x) + 2L
    df_board <- checkers_board(width, cell_width = NULL, ...)
    df_pieces <- filter(df, !grepl("tile", .data$piece_side))
    df_pieces$piece_side <- "bit_back"
    df_pieces$angle <- 0
    df_pieces$rank <- 1L
    if (n_players == 2) {
        # keep suit == 1L as 1L
        if (length(unique(df_pieces$suit)) == 4L) {
            df_pieces$suit <- ifelse(df_pieces$suit == 2L, black, df_pieces$suit)
            df_pieces$suit <- ifelse(df_pieces$suit == 3L, white, df_pieces$suit)
            df_pieces$suit <- ifelse(df_pieces$suit == 4L, white, df_pieces$suit)
        } else if (length(unique(df_pieces$suit)) == 2L) {
            df_pieces$suit <- ifelse(df_pieces$suit == 2L, white, df_pieces$suit)
        }
        if (black_first) {
            i_white <- which(df_pieces$suit == white)
            df_pieces$suit[i_white] <- black
            df_pieces$suit[-i_white] <- white
        }
    }
    bind_rows(df_board, df_pieces) %>%
        set_cell_width(cell_width, "checkers")
}
