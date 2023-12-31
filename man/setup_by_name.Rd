% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name.r
\name{setup_by_name}
\alias{setup_by_name}
\alias{checkers_by_name}
\alias{chess_by_name}
\alias{dominoes_by_name}
\alias{piecepack_by_name}
\alias{stackpack_by_name}
\title{Setups by game name}
\usage{
setup_by_name(
  name,
  system = known_game_systems,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)

checkers_by_name(
  name,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)

chess_by_name(
  name,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)

dominoes_by_name(
  name,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)

piecepack_by_name(
  name,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)

stackpack_by_name(
  name,
  ...,
  getter = function(x) get(x, envir = getNamespace("ppdf"))
)
}
\arguments{
\item{name}{Game name.  Will be normalized by \code{\link[=normalize_name]{normalize_name()}}.}

\item{system}{Game system.}

\item{...}{Additional arguments to pass to the underlying setup function.}

\item{getter}{Function with a single function name argument to use.
By default will just look for packages in this package but
a developer may want to consider \code{\link[=dynGet]{dynGet()}} or a
wrapper around \code{\link[=get]{get()}} with a custom \code{envir} argument.}
}
\description{
\code{setup_by_name()} returns setup data frames by game name and game system.
\code{checkers_by_name()}, \code{chess_by_name()}, \code{dominoes_by_name()}, and \code{piecepack_by_name()} are
aliases that set the game system.
}
