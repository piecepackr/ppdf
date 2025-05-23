% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marbles.r
\name{marble_pieces}
\alias{marble_pieces}
\alias{marble_board}
\alias{marble_bits}
\title{Generate marbles boards and pieces}
\usage{
marble_board(
  nrows = 4L,
  ncols = nrows,
  x0 = 1 * scale_factor,
  y0 = 1 * scale_factor,
  ...,
  suit = "cyan",
  scale_factor = 0.5
)

marble_bits(
  ...,
  suit = 1:6,
  rank = 9L,
  x = as.double(1:6),
  y = 1,
  angle = 0,
  length.out = NA_integer_
)
}
\arguments{
\item{nrows}{Number of rows in game board (i.e. number of holes vertically)}

\item{ncols}{Number of columns in game board (i.e. number of holes horizontally)}

\item{x0}{X coordinate for the center of the first hole}

\item{y0}{Y coordinate for the center of the first hole}

\item{...}{Should be left empty.}

\item{suit}{Suit value (color) of board and bits.
\code{1L} is "red", \code{2L} is "black", \code{3L} is "green", \code{4L} is "blue", \code{5L} is "yellow", and \code{6L} is "white".
Will be coerced by \code{\link[=piece_suit]{piece_suit()}}.}

\item{scale_factor}{Used by \code{{ppn}}.  Should usually be \code{0.5} for square pyramidal games but could be \code{1} if not playing a pyramidal game.}

\item{rank}{Determines the size of the marble from 0.5 inch diameter (rank 1) to 1.0 inch diamter (rank 9).
The default is 1.0 inch diameter marbles which match the holed boards.}

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
\code{marble_board()} returns a square holed board of desired size and color.
\code{marble_bits()} returns marble pieces.
}
\examples{
dfb <- marble_board(nrows = 2L, scale_factor = 1.0)
dfm <- marble_bits(x = c(1:2, 1:2, 1.5),
                   y = c(1, 1, 2, 2, 1.5),
                   suit = c(1:3, 5:6))
df <- rbind(dfb, dfm)
if (require("piecepackr", quietly = TRUE) &&
    packageVersion("piecepackr") >= "1.15.0-1") {
  grid::grid.newpage()
  envir = game_systems()
  pmap_piece(df, envir = envir, default.units = "in")
}
}
