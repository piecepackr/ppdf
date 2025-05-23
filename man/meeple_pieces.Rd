% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meeple.r
\name{meeple_pieces}
\alias{meeple_pieces}
\alias{meeple_bits}
\title{Generate meeples}
\usage{
meeple_bits(
  ...,
  side = "face",
  piece_side = paste0("bit_", side),
  suit = 1:6,
  x = as.double(1:6),
  y = 1,
  angle = 0,
  length.out = NA_integer_
)
}
\arguments{
\item{...}{Should be left empty.}

\item{side}{Either "face" or "back".}

\item{piece_side}{Either "tile_face" or "tile_back".}

\item{suit}{Suit value (color) of meeples.
\code{1L} is "red", \code{2L} is "black", \code{3L} is "green", \code{4L} is "blue", \code{5L} is "yellow", and \code{6L} is "white".  Will be coerced by \code{\link[=piece_suit]{piece_suit()}}.}

\item{x, y}{Cartesian coordinates (numeric vectors)}

\item{angle}{Rotation of piece (numeric vector of degrees, counter-clockwise).
Will be coerced by \code{\link[=piece_angle]{piece_angle()}}.}

\item{length.out}{The number of pieces.
Not needed if all the arguments are the same length (or of length one)
and this length is the same as the number of desired pieces.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} data frame with the following columns:
\itemize{
\item "piece_side"
\item "suit"
\item "rank"
\item "cfg" (optional, defaults to \code{"piecepack"})
\item "x"
\item "y"
\item "angle" (optional, defaults to \code{0}).
}

This data frame is compatible with \code{\link[piecepackr:render_piece]{piecepackr::render_piece()}} and likely \code{\link[ppcli:cat_piece]{ppcli::cat_piece()}}.
}
\description{
\code{meeple_bits()} returns meeples.
}
\examples{
a <- c(0, 90, 180, 270, 0, 90)
df1 <- meeple_bits(side = "face", suit = 1:6, x = 1:6, y = 1, angle = a)
df2 <- meeple_bits(side = "top", suit = 1:6, x = 1:6, y = 2, angle = a)
df <- rbind(df1, df2)
if (require("piecepackr", quietly = TRUE) &&
    packageVersion("piecepackr") >= "1.15.0-1") {
  grid::grid.newpage()
  envir = game_systems()
  pmap_piece(df, envir = envir, default.units = "in", op_scale = 0.5)
}
}
