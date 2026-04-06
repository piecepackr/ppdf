#' Generate cubes
#'
#' `cube_bits()` returns cubes.
#'
#' @param ... Should be left empty.
#' @param suit Suit value (color) of cubes.
#'             `1L` is "red", `2L` is "black", `3L` is "green", `4L` is "blue", `5L` is "yellow", and `6L` is "white".  Will be coerced by [piece_suit()].
#' @param rank Determines the size of the cube from 8mm (rank 1) to 25mm (rank 6).  Default is 10mm cubes.
#' @examples
#' df <- cube_bits(suit = 6:1, rank = 6:1, x = 6:1, y = 1)
#' if (requireNamespace("ppcli", quietly = TRUE) &&
#'     packageVersion("ppcli") >= "0.3.0-1") {
#'   ppcli::cat_piece(df)
#' }
#' if (require("piecepackr", quietly = TRUE) &&
#'     packageVersion("piecepackr") >= "1.15.0-1") {
#'   grid::grid.newpage()
#'   envir = game_systems()
#'   pmap_piece(df, envir = envir, default.units = "in", op_scale = 0.5)
#' }
#' @return `r return_df()`
#' @name cube_pieces
NULL

#' @inheritParams piecepack_tiles
#' @rdname cube_pieces
#' @export
cube_bits <- function(
	...,
	suit = 1:6,
	rank = 2L,
	x = as.double(1:6),
	y = 1,
	angle = 0,
	length.out = NA_integer_
) {
	check_dots_empty()
	df_bits <- tibble(
		piece_side = rep("bit_back", length.out = length.out),
		suit = rep(piece_suit(suit), length.out = length.out),
		rank = rep(as.integer(rank), length.out = length.out),
		cfg = rep("cubes", length.out = length.out),
		x = rep(as.double(x), length.out = length.out),
		y = rep(as.double(y), length.out = length.out),
		angle = rep(piece_angle(angle), length.out = length.out)
	)
	df_bits
}
