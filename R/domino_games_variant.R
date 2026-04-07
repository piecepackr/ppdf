#' Setups for dominoes variants
#'
#' \code{tibble} data frames of setups for `r nrow(domino_games_variant())` dominoes variants.
#' Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various games:
#'
#' `r man_markdown_table(domino_games_variant())`
#'
#' @param n The number of doubles in a set e.g. `n = 7` for double-6 set,
#'   `n = 10` for a double-9 set, `n = 13` for a double-12 set.
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @name domino_games_variant
#' @return `r return_df()`
#' @rdname domino_games_variant
#' @examples
#' df <- domino_the_jubilee()
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-9") {
#'   grid::grid.newpage()
#'   envir = game_systems(round = TRUE)
#'   pmap_piece(df, envir = envir, default.units = "in",
#'              trans = op_transform, op_scale = 0.5)
#' }
NULL

domino_games_variant <- function() {
	tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "Concentration"
            , "``domino_concentration()``"
            , NA_character_
            , "http://www.domino-play.com/Games/Concentration.htm"
            , "Domino Finder"
            , "``domino_finder()``"
            , NA_character_
            , "https://donkirkby.github.io/donimoes/rules.html#domino-finder"
            , "Dominoes Freecell"
            , "``domino_freecell()``"
            , NA_character_
            , "https://www.pagat.com/invented/domfreecell.html"
            , "(Domino) Fuji-san"
            , "``domino_fujisan()``"
            , NA_character_
            , "http://donkirkby.github.io/donimoes/rules.html#fujisan"
            , "Luzon"
            , "``domino_luzon()``"
            , NA_character_
            , "http://www.domino-play.com/Games/Luzon.htm"
            , "(Domino) Patience"
            , "``domino_patience()``"
            , NA_character_
            , "http://www.domino-play.com/Games/Patience.htm"
            , "Domino Runners"
            , "``domino_runners()``"
            , NA_character_
            , "https://donkirkby.github.io/donimoes/rules.html#domino-runners"
            , "The Jubilee"
            , "``domino_the_jubilee()``"
            , NA_character_
            , "http://www.domino-play.com/Games/Jubilee.htm"
    )
}

#' @rdname domino_games_variant
#' @export
domino_concentration <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(side = "back", x = 1 * rep(1:7, each = 4), y = 2 * rep(1:4, 7)) |>
		slice_sample_piece() |>
		mutate(angle = sample(c(0, 180), 28, replace = TRUE))
	df_tiles
}

#' @rdname domino_games_variant
#' @export
domino_finder <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(side = "back") |>
		mutate(angle = sample(c(90, 270), 28, replace = TRUE)) |>
		# First 7 tiles are the ones with a null side
		slice(c(sample.int(7), 7 + sample.int(21))) |>
		# Each 4 tiles will have one tile with a null side
		slice(sequence(
			rep(c(1, 3), 7),
			from = c(1, 8, 2, 11, 3, 14, 4, 17, 5, 20, 6, 23, 7, 26)
		)) |>
		# Within a row shuffle each 4 tiles (with exactly one tile with a null side)
		mutate(x = 2 * as.integer(replicate(7, sample.int(4))) - 0.5, y = 1 * rep(7:1, each = 4))
	df_tiles
}

#' @rdname domino_games_variant
#' @export
domino_freecell <- function(n = 7, seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}

	# n*(n-1)/2 non-blank tiles distributed across n-1 non-empty columns.
	# Column 0 starts empty (only its header tile is placed).
	total_non_blank <- (n * (n - 1L)) %/% 2L
	non_empty_cols <- n - 1L
	base_count <- total_non_blank %/% non_empty_cols
	extra_cols <- total_non_blank %% non_empty_cols

	max_tiles <- base_count + if (extra_cols > 0L) 1L else 0L
	header_y <- 2L * max_tiles + 2L

	df_all <- domino_tiles(n = n)

	# Header tiles: rank == 1 (blank top half), one per column, ordered by suit
	# suit=1 -> [0-0] (col 0), suit=2 -> [1-0] (col 1), etc.
	# angle=180 puts the higher pip (suit) at the top
	df_headers <- df_all |>
		filter(.data$rank == 1L) |>
		mutate(
			x = as.double(seq_len(n)),
			y = as.double(header_y),
			angle = 180
		)

	# Non-blank tiles: rank >= 2 (both ends non-blank)
	# Rightmost extra_cols non-empty columns get base_count+1 tiles, rest get base_count,
	# so the partial row appears at the bottom right
	col_counts <- c(
		rep(base_count, non_empty_cols - extra_cols),
		rep(base_count + 1L, extra_cols)
	)
	# Non-empty columns are at x = 2, 3, ..., n
	col_x <- rep(seq_len(non_empty_cols) + 1L, times = col_counts)
	# Tiles stack downward from the header so shorter columns have empty space at bottom
	y_within <- unlist(lapply(col_counts, function(cnt) header_y - 2L * seq_len(cnt)))

	df_non_blank <- df_all |>
		filter(.data$rank >= 2L) |>
		slice_sample_piece() |>
		mutate(
			x = as.double(col_x),
			y = as.double(y_within),
			angle = 180
		)

	bind_rows(df_headers, df_non_blank)
}

#' @rdname domino_games_variant
#' @export
domino_fujisan <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(n = 6) |>
		filter(.data$suit != .data$rank) |>
		slice_sample_piece() |>
		mutate(
			x = c(7.5, 7.5, 7.5, seq(2, 13, 1)),
			y = c(c(0.5, 1.5, 2.5), rep_len(1.5, 12)),
			angle = c(90, 90, 90, sample(c(180, 0), 12, replace = TRUE))
		)
	df_tiles[1:3, "piece_side"] <- "tile_back"
	#### Where best to get pawns from?
	df_pawns <- piecepack_pawns(suit = c(4, 1, 3, 2), x = rep(c(1, 14), each = 2), y = rep(1:2, 2))
	bind_rows(df_tiles, df_pawns)
}

#' @rdname domino_games_variant
#' @export
domino_luzon <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(
		side = "face",
		x = 2 * c(rep(1:5, 5), 7, 7, 7),
		y = c(rep(1:5, each = 5), 1, 3, 5)
	) |>
		slice_sample_piece() |>
		mutate(angle = sample(c(90, 270), 28L, replace = TRUE))
	df_tiles
}

#' @rdname domino_games_variant
#' @export
domino_patience <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(side = "back") |>
		slice_sample_piece() |>
		mutate(angle = sample(c(0, 180), 28, replace = TRUE))
	df_tiles[c(1, 8, 14, 19, 23, 26, 28), "piece_side"] <- "tile_face"
	df_tiles
}

#' @rdname domino_games_variant
#' @export
domino_runners <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(x = 2 * rep.int(1:4, 7L) - 0.5, y = 1 * rep(7:1, each = 4L)) |>
		slice_sample_piece() |>
		mutate(angle = sample(c(90, 270), 28, replace = TRUE))
	df_tiles
}

#' @rdname domino_games_variant
#' @export
domino_the_jubilee <- function(seed = NULL) {
	if (!is.null(seed)) {
		withr::local_seed(seed)
	}
	df_tiles <- domino_tiles(side = "back", y = 2) |>
		slice_sample_piece() |>
		mutate(angle = sample(c(0, 180), 28, replace = TRUE))
	df_tiles[c(1, 8, 14, 19, 23, 26, 28), "piece_side"] <- "tile_face"
	df_tiles
}

#' Generate dominoes tiles
#'
#' `domino_tiles()` generates a data frame of dominoes tiles.
#' By default will generate all the dominoes tiles of a double-6 set.
#' @param n The number of doubles in a set e.g. `n = 7` for double-6 set, `n = 10` for a double-9 set, `n = 13` for a double-12 set.
#' @param ... Should be left empty.
#' @param side Either "face" or "back".
#' @param piece_side Either "tile_face" or "tile_back".
#' @param suit "Bottom" half of domino.  Will be coerced by [domino_suit()].
#' @param rank "Top" half of domino.  Will be coerced by [piece_rank()].
#' @param cfg "dominoes" or perhaps "dominoes_black", "dominoes_blue", "dominoes_green", "dominoes_red", "dominoes_white", or "dominoes_yellow".
#' @param x,y Cartesian coordinates (numeric vectors)
#' @param angle Rotation of dominoes (numeric vector of degrees, counter-clockwise).  Will be coerced by [piece_angle()].
#' @return `r return_df()`
#' @examples
#' df_double6 <- domino_tiles()
#' nrow(df_double6)
#' df_double9 <- domino_tiles(n = 10)
#' nrow(df_double9)
#' df_double12 <- domino_tiles(n = 13)
#' nrow(df_double12)
#' df_double15 <- domino_tiles(n = 16)
#' nrow(df_double15)
#' @export
domino_tiles <- function(
	n = 7,
	...,
	side = "face",
	piece_side = paste0("tile_", side),
	suit = sequence(n:1, from = 1:n),
	rank = rep.int(1:n, n:1),
	cfg = "dominoes",
	x = sequence(n:1, from = 1:n),
	y = 2 * rep.int(n:1, n:1) - 0.5,
	angle = 0
) {
	check_dots_empty()
	tibble(
		piece_side = piece_side,
		suit = domino_suit(suit),
		rank = piece_rank(rank),
		cfg = cfg,
		x = as.double(x),
		y = as.double(y),
		angle = piece_angle(angle)
	)
}
