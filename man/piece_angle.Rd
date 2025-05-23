% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/angles.r
\name{piece_angle}
\alias{piece_angle}
\title{Cast to angle doubles}
\usage{
piece_angle(angle)
}
\arguments{
\item{angle}{A numeric or character vector of angle values.}
}
\value{
An double vector of angle values.
}
\description{
These functions help make sure angle values are the
right double values expected by the configurations used by
\code{\link[piecepackr:game_systems]{piecepackr::game_systems()}}  and \code{\link[ppn:read_ppn]{ppn::read_ppn()}}.
If the input is numeric it simply uses \code{\link[=as.double]{as.double()}} and if the
input is character it tries to cast to a reasonable value.
}
\examples{
piece_angle(c("^", "<", "v", ">"))
piece_angle(c("\U0001f06e", "\U0001f038"))
}
