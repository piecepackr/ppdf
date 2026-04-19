#' Generate chess boards and pieces
#'
#' `chess_board()` returns a square checkered board of desired size and type.
#' `chess_bits()` returns chess pieces.
#'
#' @param nrows Number of rows in game board
#' @param ncols Number of columns in game board
#' @param x0 X coordinate for the center of the first cell
#' @param y0 Y coordinate for the center of the first cell
#' @param ... Should be left empty.
#' @param side Either "face" or "back"
#' @param piece_side If `"board_face"` a checkered board (as in American Checkers).
#'                   If `"board_back"` a lined board (as in Turkish Checkers).
#' @param suit Suit value (color) of board and pieces.
#'             `r color_suit_values()`
#' @param rank Rank value of chess pieces from `1L` (pawn) to `6L` (king).
#'             Will be coerced by [chess_rank()].
#' @param angle Rotation of bit/board (numeric vector of degrees, counter-clockwise).
#'              Will be coerced by [piece_angle()].
#' @param cell_width Width of board cell.
#'                   [piecepackr::game_systems()] supports `1` or `2`.
#' @inheritParams piecepack_tiles
#' @return `r return_df()`
#' @examples
#' # David Silverman's "Mini chess 4x4"
#' dfb <- chess_board(nrows = 4L, ncol = 4L)
#' df1 <- chess_bits(suit = "white", rank = c("R", "Q", "K", "R"),
#'                   x = 1:4, y= 1)
#' df2 <- chess_bits(suit = "white", rank = "P", x = 1:4, y= 2)
#' df3 <- chess_bits(suit = "black", rank = "P", x = 1:4, y= 3)
#' df4 <- chess_bits(suit = "black", rank = c("r", "q", "k", "r"),
#'                   x = 1:4, y= 4)
#' df <- rbind(dfb, df1, df2, df3, df4)
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1" &&
#'     requireNamespace("systemfonts", quietly = TRUE) &&
#'     piecepackr::has_font("Dejavu Sans") &&
#'     piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   envir = game_systems("dejavu")
#'   pmap_piece(df, envir = envir, default.units = "in")
#' }
#'
#' @name chess_pieces
NULL

#' @rdname chess_pieces
#' @export
chess_board <- function(
	nrows = 8L,
	ncols = nrows,
	x0 = 1,
	y0 = 1,
	...,
	side = "face",
	piece_side = paste0("board_", side),
	suit = "green",
	angle = 0,
	cell_width = getOption("ppdf.chess_cell_width", 1)
) {
	stopifnot("Don't support non-square checkers boards yet" = nrows == ncols)
	check_dots_empty()
	x <- x0 - 0.5 + 0.5 * ncols
	y <- y0 - 0.5 + 0.5 * nrows
	df_board <- tibble(
		piece_side = piece_side,
		suit = piece_suit(suit),
		rank = as.integer(nrows),
		x = as.double(x),
		y = as.double(y),
		angle = piece_angle(angle)
	)
	set_cell_width(df_board, cell_width, "chess")
}

#' @rdname chess_pieces
#' @export
chess_bits <- function(
	...,
	suit = rep(1:6, each = 6L),
	rank = rep(1:6, 6L),
	x = rep(1:6, 6L),
	y = rep(1:6, each = 6L),
	angle = 0,
	cell_width = getOption("ppdf.chess_cell_width", 1),
	length.out = NA_integer_
) {
	check_dots_empty()
	df_bits <- tibble(
		piece_side = rep("bit_face", length.out = length.out),
		suit = rep(piece_suit(suit), length.out = length.out),
		rank = rep(chess_rank(rank), length.out = length.out),
		x = rep(as.double(x), length.out = length.out),
		y = rep(as.double(y), length.out = length.out),
		angle = rep(piece_angle(angle), length.out = length.out)
	)
	set_cell_width(df_bits, cell_width, "chess")
}

df_parse_fen_pieces <- function(fen) {
	placement <- strsplit(fen, " ", fixed = TRUE)[[1L]][1L]
	rows <- strsplit(placement, "/", fixed = TRUE)[[1L]]
	char <- character(0)
	x <- integer(0)
	y <- integer(0)
	for (i in seq_along(rows)) {
		yi <- length(rows) + 1L - as.integer(i)
		xi <- 1L
		tokens <- regmatches(rows[i], gregexpr("[A-Za-z]|[0-9]+", rows[i]))[[1L]]
		for (tok in tokens) {
			if (grepl("^[0-9]+$", tok)) {
				xi <- xi + as.integer(tok)
			} else {
				char <- c(char, tok)
				x <- c(x, xi)
				y <- c(y, yi)
				xi <- xi + 1L
			}
		}
	}
	tibble(char = char, x = x, y = y)
}

fen_to_chess_bits <- function(fen) {
	df <- df_parse_fen_pieces(fen)
	chess_bits(
		suit = ifelse(str_detect(df$char, "[[:upper:]]"), 6L, 2L),
		rank = df$char,
		x = df$x,
		y = df$y
	)
}
