#' Preview layout of a piecepack set
#'
#' `piecepack_preview()` returns a data frame representing a preview layout of
#' the pieces in a standard piecepack set.
#'
#' @inheritParams piecepack_tiles
#' @return `r return_df()`
#' @examples
#' df <- piecepack_preview()
#' if (requireNamespace("ppcli", quietly = TRUE) &&
#'     packageVersion("ppcli") >= "0.3.0-2") {
#'   ppcli::cat_piece(df, annotate = TRUE)
#' }
#' @export
piecepack_preview <- function(cfg = "piecepack") {
	df_tiles <- piecepack_tiles(
		side = c(rep("face", 4L), "back"),
		suit = c(1L:4L, 1L),
		rank = c(rep(2L, 4L), 1L),
		cfg = cfg,
		x = c(1, 5, 5, 1, 3),
		y = c(5, 5, 3, 3, 3),
	)

	df_coins <- piecepack_coins(
		side = rep(c("face", "back"), each = 3L),
		suit = c(1:3, c(4L, 2L, 3L)),
		rank = 1:6,
		cfg = cfg,
		x = rep(1:3 - 0.5, 2L),
		y = rep(2:1 - 0.5, each = 3L)
	)

	df_saucers <- piecepack_saucers(
		suit = c(1, 4),
		cfg = cfg,
		x = 2.5,
		y = 6:5 - 0.5,
		side = c("face", "back")
	)

	df_pawns <- piecepack_pawns(
		suit = 2:3,
		cfg = cfg,
		x = 3.5,
		y = 6:5 - 0.5,
		side = c("face", "back"),
		length.out = 2L
	)

	df_die <- piecepack_dice(
		suit = c(1L, 1L, 2L, 4L, 3L, 3L),
		rank = 1:6,
		cfg = cfg,
		x = rep(4:6 - 0.5, 2L),
		y = rep(2:1 - 0.5, each = 3L)
	)

	bind_rows(df_tiles, df_coins, df_saucers, df_pawns, df_die)
}
