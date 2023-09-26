#### Export
# 24 tiles arranged 5x5 with hole in middle, center of bottom-left tile at (x0, y0)
piecepack_donut_tiles <- function(seed = NULL, tiles = NULL, x0 = 1.5, y0 = 1.5, face = TRUE) {
    if (!is.null(seed)) withr::local_seed(seed)
    df_txy <- tibble(piece_side = ifelse(face, "tile_face", "tile_back"),
                     x = x0 + c(rep(seq(0, 8, 2), 2), 0, 2, 6, 8, rep(seq(0, 8, 2), 2)),
                     y = y0 + c(rep(8, 5), rep(6, 5), rep(4, 4), rep(2, 5), rep(0, 5)))
    if (is.null(tiles)) {
        df_tsr <- tibble(suit = rep(1:4, each = 6), rank = rep(1:6, 4))[sample.int(24L), ]
    } else {
        df_tsr <- process_tiles(tiles)
    }
    bind_cols(df_txy, df_tsr)
}

df_none <- function() {
    tibble::tibble(piece_side = character(0L),
                   suit = integer(0L), rank = integer(0L),
                   cfg = character(0),
                   x = numeric(0), y = numeric(0), angle = numeric(0))
}

#### Export
piecepack_none <- df_none

#' Setup various game boards using piecepack tiles
#'
#' \code{df_rect_board_tiles} returns a tibble \code{data_frame} of a rectangular board of desired size
#' made using a maximum number of piecepack tiles.
#' It will use either \dQuote{cells}, \dQuote{points}, and/or \dQuote{rivers} as necessary.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell/point
#' @param y0 Y coordinate for the center of the first cell/point
#' @param max_tiles Maximum number of tiles that can be used
#' @param suit Vector of suit values to use for tile back (will be repeated).
#' @param rank Vector of rank values to use for tile back (will be repeated).
#' @rdname piecepack_board
#' @export
piecepack_rect_board_tiles <- function(nrows = 8, ncols = 8, x0 = 1, y0 = 1, max_tiles = 24,
                               suit = rep(1:4, 6), rank = rep(1:6, each = 4)) {
    if (can_use_squares(nrows, ncols, max_tiles)) {
        x <- seq(0.5, by = 2, length.out = ncols/2)
        y <- seq(0.5, by = 2, length.out = nrows/2)
    } else if (can_use_lines(nrows, ncols, max_tiles)) {
        ntiles <- 0
        for (rr in as.integer(even(nrows)):(nrows %/% 3 - 1)) {
            for (rc in as.integer(even(ncols)):(ncols %/% 3 - 1)) {
                tr <- n_tiles(nrows, rr)
                tc <- n_tiles(ncols, rc)
                if (n_lines(tr, rr) == nrows &&
                    n_lines(tc, rc) == ncols &&
                    tr * tc > ntiles &&
                    tr * tc <= max_tiles) {
                    ntiles <- tr * tc

                    x3 <- rep(3, rc)
                    x2 <- rep(2, tc-rc-1)
                    y3 <- rep(3, rr)
                    y2 <- rep(2, tr-rr-1)
                    x <- 1 + cumsum(c(0, adjust_rivers(c(x3, x2))))
                    y <- 1 + cumsum(c(0, adjust_rivers(c(y3, y2))))
                }
            }
        }
    } else {
       abort(str_glue("Don't know how to form a {ncols}x{nrows} board with {max_tiles} tiles"),
             class = "board_setup")
    }
    xr <- x0 + rep(x, length(y))
    yr <- y0 + rep(y, each = length(x))
    df <- tibble(piece_side = "tile_back", x = xr, y = yr, angle = 0)
    df$suit <- rep(suit, length.out = nrow(df))
    df$rank <- rep(rank, length.out = nrow(df))
    df
}

even <- function(x) x %% 2 == 0

can_use_squares <- function(nrows, ncols, max_tiles) {
    even(nrows) && even(ncols) && (nrows*ncols/4 <= max_tiles)
}

n_lines <- function(tiles, rivers) {
    2 * tiles + 1 + rivers
}
n_tiles <- function(n, rivers) {
    ceiling((n - rivers - 1)/2) #### Not quite right for illegal n, river values
}
n_rivers <- function(n, tiles) {
    n - 2*tiles - 1
}

min_line_tiles <- function(n) {
    mod3 <- n %% 3
    if (mod3 == 0) {
        n / 3
    } else {
        n %/% 3 + 1
    }
}

line_score <- function(x) {
    prev <- Inf
    score <- 0
    streak <- 0
    for (e in x) {
        if (e == prev) {
            streak <- streak + 1
            score <- score + streak * e
        } else {
            streak <- 0
        }
        prev <- e
    }
    score
}

adjust_rivers <- function(x) {
    if (length(x) < 3) return(x)
    score <- Inf
    new_score <- line_score(x)
    n <- length(x)
    while (new_score < score) {
        score <- new_score
        for (ii in seq(n)) {
            for (jj in seq(ii-1)) {
                 if (ii > 1 && x[ii] != x[jj] && new_score == score) {
                      xp <- swap(x, ii, jj)
                      sp <- line_score(xp)
                      if (sp < score) {
                          new_score <- sp
                          x <- xp
                      }
                 }
            }
        }
    }
    x
}

swap <- function(x, ii, jj) {
    tmp <- x[ii]
    x[ii] <- x[jj]
    x[jj] <- tmp
    x
}

can_use_lines <- function(nrows, ncols, max_tiles) {
    minr <- min_line_tiles(nrows)
    minc <- min_line_tiles(ncols)
    !(nrows %in% c(2, 4)) &&
        !(ncols %in% c(2, 4)) &&
        minr * minc <= max_tiles
}
