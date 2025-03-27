process_angles <- function(angles) {
    angles <- gsub("\\^", "0", angles)
    angles <- gsub("<", "90", angles)
    angles <- gsub("v", "180", angles)
    angles <- gsub(">", "270", angles)
    as.numeric(angles)
}

process_ranks <- function(ranks) {
    if (is.null(ranks)) {
        return(rep.int(1:6, 4L)[sample.int(24L)])
    }
    if (is.character(ranks)) {
        ranks <- gsub("[[:space:]]", "", ranks)
        ranks <- gsub("[[:punct:]]", "", ranks)
        ranks <- gsub("n", "0", ranks)
        ranks <- gsub("a", "1", ranks)
        if (length(ranks) == 1) ranks <- stringr::str_split(ranks, "")[[1]]
    }
    as.integer(ranks) + 1
}

process_suits <- function(suits) {
    if (is.character(suits)) {
        suits <- gsub("[[:space:]]", "", suits)
        suits <- gsub("[[:punct:]]", "", suits)
        suits <- toupper(suits)
        suits <- gsub("S|R", "1", suits)
        suits <- gsub("M|K", "2", suits)
        suits <- gsub("C|G", "3", suits)
        suits <- gsub("A|B", "4", suits)
        if (length(suits) == 1) suits <- stringr::str_split(suits, "")[[1]]
    }
    as.integer(suits)
}

generate_sra <- function(df, filter_ = "^tile", which_ = "sra") {
    df <- dplyr::filter(df, str_detect(.data$piece_side, filter_))
    l <- purrr::pmap(df, generate_helper, which_ = which_)
    paste(unlist(l), collapse = "")
}
generate_helper <- function(suit, rank, angle = 0, which_ = "sra", ...) {
    s <- switch(suit, "S", "M", "C", "A")
    r <- rank - 1
    a <- "^"
    if (near(angle %% 360, 90)) a <- "<"
    if (near(angle %% 360, 180)) a <- "v"
    if (near(angle %% 360, 270)) a <- ">"
    switch(which_,
           sra = paste0(s, r, a),
           sr = paste(s, r),
           r = r)
}

process_tiles <- function(tiles = NULL, n_tiles = 24) {
    if (is.null(tiles)) {
        return(tibble(suit = rep(1:4, each = 6L),
                      rank = rep.int(1:6, 4L),
                      angle = 0)[sample.int(24L), ])
    }
    tiles <- gsub("[[:space:]]", "", tiles)
    tiles <- gsub("[/:;\\\\|]", "", tiles)
    tiles <- stringr::str_split(tiles, "")[[1]]
    if (length(tiles) == 2 * n_tiles) {
        suits <- tiles[which(seq(2 * n_tiles) %% 2 == 1)]
        needs_ranks <- str_detect(tiles[2], "[\\^<v>]")
        if (needs_ranks) {
            angles <- tiles[which(seq(2 * n_tiles) %% 2 == 0)]
            ranks <- rep_len(NA_integer_, n_tiles)
        } else {
            ranks <- tiles[which(seq(2 * n_tiles) %% 2 == 0)]
            angles <- rep("^", n_tiles)
        }
    } else if (length(tiles) == 3 * n_tiles) {
        suits <- tiles[which(seq(3 * n_tiles) %% 3 == 1)]
        ranks <- tiles[which(seq(3 * n_tiles) %% 3 == 2)]
        angles <- tiles[which(seq(3 * n_tiles) %% 3 == 0)]
        needs_ranks <- FALSE
    } else {
        abort(paste("Don't know how to handle tiles string", tiles), class = "board_setup")
    }
    suits <- process_suits(suits)
    if (needs_ranks) {
        n_ranks <- n_tiles %/% 4
        ranks[which(suits==1)] <- sample.int(n_ranks)
        ranks[which(suits==2)] <- sample.int(n_ranks)
        ranks[which(suits==3)] <- sample.int(n_ranks)
        ranks[which(suits==4)] <- sample.int(n_ranks + n_tiles %% 4)
    } else {
        ranks <- process_ranks(ranks)
    }
    angles <- process_angles(angles)
    tibble(suit = as.integer(suits),
           rank = as.integer(ranks),
           angle = as.double(angles))
}

random_dice <- function(n_dice = 4L, n_ranks = 6L) {
    sample.int(n_ranks, n_dice, replace = TRUE)
}

fill_piece_rank <- function(df) {
    df %>% separate_piece_side() %>%
        group_by(.data$piece, .data$suit) %>%
        mutate(rank = replace_na_piece(.data$rank)) %>%
        ungroup() %>%
        unite_piece_side()
}

fill_piece_suit <- function(df) {
    df %>% separate_piece_side() %>%
        group_by(.data$piece, .data$rank) %>%
        mutate(suit = replace_na_piece(.data$suit)) %>%
        ungroup() %>%
        unite_piece_side()
}

# Avoids importing {stats}
na_omit <- function(x) Filter(Negate(is.na), x)

replace_na_piece <- function(x) {
    ina <- which(is.na(x))
    if (length(ina)) {
        x[ina] <- sample(setdiff(seq.int(length(x)), as.integer(na_omit(x))),
                         length(ina))
        x
    } else {
        x
    }
}

separate_piece_side <- function(df) {
    tidyr::separate_wider_delim(df, "piece_side", "_", names = c("piece", "side"))
}

unite_piece_side <- function(df) {
    tidyr::unite(df, "piece_side", "piece", "side")
}

select_piece <- function(df) {
    dplyr::select(df, "piece_side", "suit", "rank", "cfg", "x", "y", "angle")
}

# randomly shuffle paired suit/rank within a data frame
# but keep x,y,angle,piece_side fixed
slice_sample_piece <- function(df, ..., n = nrow(df), size = n, names = c("rank", "suit")) {
    stopifnot(all(hasName(df, names)) && nrow(df))
    idx <- sample.int(nrow(df))
    for (n in names) {
        df[[n]] <- df[[n]][idx]
    }
    df
}

#' Generate piecepack pieces
#'
#' `piecepack_coins()` generates a data frame of piecepack coins.
#' `piecepack_dice()` generates a data frame of piecepack dice.
#' `piecepack_matchsticks()` generates a data frame of piecepack matchsticks.
#' `piecepack_pawns()` generates a data frame of piecepack pawns.
#' `piecepack_tiles()` generates a data frame of piecepack tiles.
#' @param ... Should be left empty.
#' @param side Either "face" or "back".
#' @param piece_side Either "tile_face" or "tile_back".
#' @param suit Integer vector with values between `1L` and `4L`.
#'             `1L` is "suns", `2L` is "moons", `3L` is "crowns", `4L` is "arms".
#'             Will be coerced by [piece_suit()].
#' @param rank Integer vector with values between `1L` (null) and `6L` (five).
#'             Will be coerced by [piece_rank()].
#' @param cfg "piecepack" or perhaps "playing_cards_expansion", "dual_piecepacks_expansion", or "subpack".
#' @param x,y Cartesian coordinates (numeric vectors)
#' @param angle Rotation of piece (numeric vector of degrees, counter-clockwise).
#'              Will be coerced by [piece_angle()].
#' @param length.out The number of pieces.
#'                   Not needed if all the arguments are the same length (or of length one)
#'                   and this length is the same as the number of desired pieces.
#' @return `r return_df()`
#' @examples
#' df_coins <- piecepack_coins()
#' df_dice <- piecepack_dice()
#' df_pawns <- piecepack_pawns()
#' df_tiles <- piecepack_tiles()
#' @name piecepack_pieces
NULL

#' @rdname piecepack_pieces
#' @export
piecepack_coins <- function(...,
                            side = "face",
                            piece_side = paste0("coin_", side),
                            suit = rep(1:4, each = 6L),
                            rank = rep.int(1:6, 4L),
                            cfg = "piecepack",
                            x = as.double(rep.int(1:6, 4L)),
                            y = as.double(rep(4:1, each = 6L)),
                            angle = 0,
                            length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep(piece_side, length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(piece_rank(rank), length.out = length.out),
           cfg = rep(cfg, length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname piecepack_pieces
#' @export
piecepack_dice <- function(...,
                           suit = 1:4,
                           rank = 1L,
                           cfg = "piecepack",
                           x = as.double(1:4),
                           y = 1,
                           angle = 0,
                           length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep("die_face", length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(piece_rank(rank), length.out = length.out),
           cfg = rep(cfg, length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname piecepack_pieces
#' @export
piecepack_matchsticks <- function(...,
                                  side = "face",
                                  piece_side = paste0("matchstick_", side),
                                  suit = rep(1:4, each = 6L),
                                  rank = rep.int(1:6, 4L),
                                  cfg = "piecepack",
                                  x = as.double(rep.int(1:6, 4L)),
                                  y = 3 * rep(4:1, each = 6L),
                                  angle = 0,
                                  length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep(piece_side, length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(piece_rank(rank), length.out = length.out),
           cfg = rep(cfg, length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname piecepack_pieces
#' @export
piecepack_pawns <- function(...,
                            side = "face",
                            piece_side = paste0("pawn_", side),
                            suit = 1:4,
                            cfg = "piecepack",
                            x = as.double(1:4),
                            y = 1,
                            angle = 0,
                            length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep(piece_side, length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(1L, length.out = length.out),
           cfg = rep(cfg, length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}

#' @rdname piecepack_pieces
#' @export
piecepack_tiles <- function(...,
                           side = "face",
                           piece_side = paste0("tile_", side),
                           suit = rep(1:4, each = 6L),
                           rank = rep.int(1:6, 4L),
                           cfg = "piecepack",
                           x = 2 * rep.int(1:6, 4L),
                           y = 2 * rep(4:1, each = 6L),
                           angle = 0,
                           length.out = NA_integer_) {
    check_dots_empty()
    tibble(piece_side = rep(piece_side, length.out = length.out),
           suit = rep(piece_suit(suit), length.out = length.out),
           rank = rep(piece_rank(rank), length.out = length.out),
           cfg = rep(cfg, length.out = length.out),
           x = rep(as.double(x), length.out = length.out),
           y = rep(as.double(y), length.out = length.out),
           angle = rep(piece_angle(angle), length.out = length.out))
}
