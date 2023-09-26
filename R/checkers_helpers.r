set_cell_width <- function(df, cell_width, system) {
    df <- mutate(df, cfg = paste0(system, cell_width),
           x = cell_width * .data$x,
           y = cell_width * .data$y)
    attr(df, "scale_factor") <- cell_width
    df
}

# function that converts piecepack game to checkers
to_checkers <- function(df, cell_width = 1, ..., n_players = 2) {
    dft <- filter(df, grepl("tile", .data$piece_side))
    width <- max(dft$x) - min(dft$x) + 2
    df_board <- checkers_board(width, cell_width = NULL, ...)
    df_pieces <- filter(df, !grepl("tile", .data$piece_side))
    df_pieces$piece_side <- "bit_back"
    df_pieces$angle <- NULL
    df_pieces$rank <- 1
    if (n_players == 2 && length(unique(df_pieces$suit)) == 4) {
        if (length(unique(df_pieces$suit)) == 4) {
            df_pieces$suit <- ifelse(df_pieces$suit == 2, 1, df_pieces$suit)
            df_pieces$suit <- ifelse(df_pieces$suit == 3, 6, df_pieces$suit)
            df_pieces$suit <- ifelse(df_pieces$suit == 4, 6, df_pieces$suit)
        } else if (length(unique(df_pieces$suit)) == 2) {
            df_pieces$suit <- ifelse(df_pieces$suit == 2, 6, df_pieces$suit)
        }
    }
    bind_rows(df_board, df_pieces) %>%
        set_cell_width(cell_width, "checkers")
}
