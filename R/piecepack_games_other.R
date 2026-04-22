#' Setups for other modern games playable with a piecepack
#'
#' \code{tibble} data frames of setups for `r nrow(piecepack_games_other())` other games playable with a piecepack.
#'   Data frame output can usually be plotted with \code{pmap_piece(df, default.units = "in")}.
#'
#' Here are links for more information about the various other games:
#'
#' `r man_markdown_table(piecepack_games_other())`
#'
#' @param nrows Number of rows (and columns) in game board.
#' @param variant Name of variant.
#' @param seed Seed that determines setup, either an integer or \code{NULL}
#' @param coins String of coin layout
#' @rdname piecepack_games_other
#' @name piecepack_games_other
#' @return `r return_df()`
#' @examples
#' df <- piecepack_dao()
#' if (requireNamespace("ppcli", quietly = TRUE) &&
#'     packageVersion("ppcli") >= "0.3.0-1") {
#'   ppcli::cat_piece(df, annotate = TRUE)
#' }
NULL

piecepack_games_other <- function() {
	tribble(~game
            , ~methods
            , ~comment
            , ~url
            , "12345ive!"
            , "``piecepack_12345ive()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/154644/12345ive"
            , "Breakthrough"
            , "``piecepack_breakthrough()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Breakthrough_(board_game)"
            , "Crossings"
            , "``piecepack_crossings()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Crossings_(game)"
            , "Change Change"
            , "``piecepack_change_change()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/ChangeChange"
            , "Dao"
            , "``piecepack_dao()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/948/dao"
            , "Dodgem"
            , "``piecepack_dodgem()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Dodgem"
            , "Grasshopper"
            , "``piecepack_grasshopper()``"
            , NA_character_
            , "http://www.cyningstan.com/game/71/grasshopper"
            , "Evade"
            , "``piecepack_evade()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Evade"
            , "Kangaroo"
            , "``piecepack_kangaroo()``"
            , NA_character_
            , "https://boardgamegeek.com/boardgame/6132/kangaroo-the-jumping-game"
            , "King's Valley"
            , "``piecepack_kings_valley()``"
            , NA_character_
            , "https://www.logygames.com/english/kingsvalley.html"
            , "Lines of Action"
            , "``piecepack_lines_of_action()``"
            , NA_character_
            , "https://en.wikipedia.org/wiki/Lines_of_Action"
            , "Lukawan"
            , "``piecepack_lukawan()``"
            , NA_character_
            , "https://ludism.org/ppwiki/Lukawan"
            , "Quatri"
            , "``piecepack_quatri()``"
            , NA_character_
            , "https://www.ludism.org/ppwiki/Quatri"
            )
}

#' @rdname piecepack_games_other
#' @export
piecepack_12345ive <- function() {
	df_t <- piecepack_rectangular_board(14L, 5L)
	df_c <- piecepack_coins(
		side = "back",
		x = rep.int(1:5, 2L),
		y = rep(c(14L, 1L), each = 5L),
		suit = rep(c(1L, 2L), each = 5L),
		rank = rep.int(1:5, 2L),
		angle = rep(c(180, 0), each = 5L)
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_breakthrough <- piecepack_gothic_checkers

#' @rdname piecepack_games_other
#' @export
piecepack_change_change <- function(seed = NULL, coins = NULL) {
	maybe_local_seed(seed)
	if (is.null(coins)) {
		suits <- rep.int(1:4, c(1, 2, 4, 4))[sample.int(11L)]
	} else {
		suits <- process_suits(coins)
	}
	df <- piecepack_coins(
		side = "back",
		x = c(1:4, 1:4, 1:3),
		y = rep.int(3:1, c(4, 4, 3)),
		suit = suits,
		rank = NA_integer_
	) |>
		fill_piece_rank()
	df
}

#' @rdname piecepack_games_other
#' @export
piecepack_crossings <- piecepack_breakthrough

#' @rdname piecepack_games_other
#' @export
piecepack_dao <- function() {
	df_t <- piecepack_rectangular_board(4L, 4L)
	df_c <- piecepack_coins(
		side = "back",
		x = c(1:4, 1:4),
		y = c(4:1, 1:4),
		suit = rep(c(1L, 2L), each = 4L),
		rank = rep.int(1:4, 2L),
		angle = rep(c(180, 0), each = 4L)
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_dodgem <- function(nrows = 6L, variant = "vout") {
	nrows <- as.integer(nrows)
	stopifnot("`nrows` must be 12 or less" = nrows <= 12)
	variant <- match.arg(variant, c("vout", "schoessow"))
	n_pieces <- nrows - 1L
	df_t <- piecepack_rectangular_board(nrows, nrows)
	if (n_pieces <= 6L) {
		rank_seq <- seq_len(n_pieces)
		suit_left <- rep(1L, n_pieces)
		suit_right <- rep(4L, n_pieces)
	} else {
		rank_seq <- c(1:6, seq_len(n_pieces - 6L))
		suit_left <- c(rep(1L, 6L), rep(2L, n_pieces - 6L))
		suit_right <- c(rep(4L, 6L), rep(3L, n_pieces - 6L))
	}
	if (variant == "vout") {
		x <- c(rep(1L, n_pieces), seq.int(2L, nrows))
		y <- c(seq.int(2L, nrows), rep(1L, n_pieces))
		angle <- c(rep(270, n_pieces), rep(0, n_pieces))
	} else {
		x <- c(rep(1L, n_pieces), rep(nrows, n_pieces))
		y <- c(seq_len(n_pieces), seq.int(2L, nrows))
		angle <- c(rep(270, n_pieces), rep(90, n_pieces))
	}
	df_c <- piecepack_coins(
		side = "back",
		x = x,
		y = y,
		suit = c(suit_left, suit_right),
		rank = c(rank_seq, rank_seq),
		angle = angle
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_evade <- function() piecepack_rectangular_board(ncols = 6, nrows = 6)

#' @rdname piecepack_games_other
#' @export
piecepack_kangaroo <- piecepack_breakthrough

#' @rdname piecepack_games_other
#' @export
piecepack_kings_valley <- function(variant = "standard") {
	variant <- match.arg(variant, c("standard", "retrieve the king"))
	df_t <- piecepack_donut_board(x0 = 1.5, y0 = 1.5)
	df_sc <- piecepack_coins(
		side = "back",
		x = c(2, 4, 8, 10),
		y = 10,
		suit = 1L,
		rank = 1:4,
		angle = 180
	)
	df_ac <- piecepack_coins(
		side = "back",
		x = c(2, 4, 8, 10),
		y = 2,
		suit = 4L,
		rank = 1:4,
		angle = 0
	)
	if (variant == "standard") {
		df_sp <- piecepack_pawns(suit = 1L, x = 6, y = 10, angle = 180)
		df_ap <- piecepack_pawns(suit = 4L, x = 6, y = 2, angle = 0)
	} else {
		df_sp <- piecepack_pawns(suit = 1L, x = 6, y = 2, angle = 180)
		df_ap <- piecepack_pawns(suit = 4L, x = 6, y = 10, angle = 0)
	}
	df <- bind_rows(df_t, df_sc, df_sp, df_ac, df_ap)
	attr(df, "scale_factor") <- 2
	df
}

#' @rdname piecepack_games_other
#' @export
piecepack_grasshopper <- function() {
	df_t <- piecepack_rectangular_board(8L, 8L)
	df_c <- piecepack_coins(
		side = "back",
		x = c(5:8, 6:8, 7:8, 8, 1, 1:2, 1:3, 1:4),
		y = c(8, 8, 8, 8, 7, 7, 7, 6, 6, 5, 4, 3, 3, 2, 2, 2, 1, 1, 1, 1),
		suit = c(1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 3, 4, 3, 3, 4, 3, 4, 3, 4, 3),
		rank = c(1, 1, 2, 2, 3, 3, 4, 5, 4, 6, 1, 1, 2, 3, 2, 4, 3, 5, 4, 6),
		angle = rep(c(180, 0), each = 10)
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_lines_of_action <- function() {
	df_t <- piecepack_rectangular_board(8L, 8L)
	df_c <- piecepack_coins(
		side = "back",
		x = c(2:7, rep(8, 6), 2:7, rep(1, 6)),
		y = c(rep(8, 6), 2:7, rep(1, 6), 2:7),
		suit = rep(c(1, 3, 2, 4), each = 6L),
		rank = rep.int(1:6, 4L),
		angle = rep(c(180, 90, 0, 270), each = 6L)
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_lukawan <- function() {
	df_t <- piecepack_rectangular_board(14L, 6L)
	df_c <- piecepack_coins(
		side = "back",
		x = rep.int(1:6, 2L),
		y = rep(c(14L, 1L), each = 6L),
		suit = rep(c(1L, 2L), each = 6L),
		rank = rep.int(1:6, 2L),
		angle = rep(c(180, 0), each = 6L)
	)
	bind_rows(df_t, df_c)
}

#' @rdname piecepack_games_other
#' @export
piecepack_quatri <- function() {
	df_tiles <- piecepack_rectangular_board(4L, 4L)
	df_coins <- piecepack_coins(
		side = "back",
		x = rep(1:4, 2),
		y = rep(c(4, 1), each = 4),
		suit = c(1, 2, 1, 2, 2, 1, 2, 1),
		rank = c(2, 2, 3, 3, 5, 5, 4, 4),
		angle = c(180, 0, 180, 0, 0, 180, 0, 180)
	)
	bind_rows(df_tiles, df_coins)
}
