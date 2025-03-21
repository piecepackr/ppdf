# ppdf

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/ppdf)](https://cran.r-project.org/package=ppdf)
[![R-CMD-check](https://github.com/piecepackr/ppdf/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/piecepackr/ppdf/actions)
[![codecov](https://codecov.io/github/piecepackr/ppdf/branch/main/graph/badge.svg)](https://app.codecov.io/github/piecepackr/ppdf)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

## Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Domino Fuji-san](#fujisan)
  + [Lines of Action](#loa)
  + [Piecepack Tablut](#tablut)
  + [Using PPN to animate a game of Relativity](#relativity)

* [Supported games](#supported)

  + [Checkers Sets](#checkers)
  + [Chess Sets](#chess)
  + [Dominoes](#dominoes)
  + [Piecepacks](#piecepack)
  + [Other games](#other)

* [External links](#links)

  + [R packages](#software)
  + [Boardgame rules](#rules)

## <a name="overview">Overview</a>

* This package contains *functions* that return `{tibble}` data frames with (possibly random) setup data for over a hundred board games playable with public domain game systems.
* This board game data can be visualized by [{piecepackr}](https://github.com/piecepackr/piecepackr) using the `{grid}`, `{ggplot2}`, `{rayrender}`, `{rayvertex}`, or `{rgl}` graphics systems or in a command-line interface using `{cli}` by `ppcli::cat_piece()`.
* If you use [Portable Piecepack Notation (PPN)](https://trevorldavis.com/piecepackr/portable-piecepack-notation.html) to record the moves for any of games supported by this package then you can visualize any/all of the moves for that game with the help of the PPN parser in [{ppn}](https://github.com/piecepackr/ppn).
* The package name "ppdf" is an acronym for **p**iece**p**ackr **d**ata **f**rames".
* Many of these functions were originally contained (under a slightly different name) in the experimental [{ppgames}](https://github.com/piecepackr/ppgames) and [{tradgames}](https://github.com/piecepackr/tradgames) packages.

## <a name="installation">Installation</a>


``` r
remotes::install_github("piecepackr/ppdf")
```

## <a name="examples">Examples</a>

### <a name="fujisan">Domino Fuji-san</a>


``` r
df_fujisan <- ppdf::domino_fujisan(seed = 42)

if (requireNamespace("piecepackr", quietly = TRUE) &&
    all(capabilities("png"))) {
  library("piecepackr")
  envir <- game_systems(pawn = "joystick", round = TRUE)
  render_piece(df_fujisan, file = "man/figures/README-fujisan.png",
               envir = envir, op_scale = 0.5, op_angle = 90,
               trans = op_transform, as_top = "pawn_face")
}
```

![Setup for playing a game of *Fuji-san* with a set of dominoes](man/figures/README-fujisan.png)

### <a name="loa">Lines of Action</a>


``` r
df_loa <- ppdf::checker_lines_of_action()

if (requireNamespace("piecepackr", quietly = TRUE) &&
    all(capabilities("png"))) {
  library("piecepackr")
  envir <- game_systems()
  render_piece(df_loa, file = "man/figures/README-lines-of-action.png",
               envir = envir, op_scale = 0.5, trans = op_transform)
}
```

![Setup for playing a game of *Lines of Action* with a checkers set](man/figures/README-lines-of-action.png)

### <a name="tablut">Piecepack Tablut</a>


``` r
df_tablut <- ppdf::piecepack_tablut()

if (requireNamespace("piecepackr", quietly = TRUE) &&
    requireNamespace("systemfonts", quietly = TRUE) &&
    piecepackr::has_font("Dejavu Sans") &&
    all(capabilities(c("cairo", "png")))) {
  library("piecepackr")
  envir <- game_systems("dejavu", pawn = "joystick")
  render_piece(df_tablut, file = "man/figures/README-tablut.png",
               envir = envir, op_scale = 0.5,
               trans = op_transform, as_top = "pawn_face")
}
```

![Setup for playing a game of *Tablut* with a piecepack deck](man/figures/README-tablut.png)

### <a name="relativity">Using PPN to animate a game of Relativity</a>

An example game of [Relativity](https://trevorldavis.com/piecepackr/relativity.html) recorded in the [Portable Piecepack Notation (PPN)](https://trevorldavis.com/piecepackr/portable-piecepack-notation.html#relativity) format:


``` r
ppn_file <- system.file("ppn/relativity.ppn", package = "ppn")
cat(paste("\t", readLines(ppn_file)), sep = "\n")
```

	 ---
	 Round: Night
	 GameType: 
	     Name: Relativity
	     Coins: "n45a5n/3a2243/3n4a2n/a25345"
	 ...
	 { Notes: Space has power of adjacency while Time has power of sequence }
	 1S. d3#e3 1T. d3#d1
	 2S. c2#c3 2T. d2#b1;M2@e6
	 3S. b2#a2 3T. e3#b2
	 4S. f1#f4 4T. e2#a3;M3@e6
	 5S. c4#d4 5T. f1#a1
	 6S. c1#c4;M5@b6 6T. b3#a3
	 7S. f3#a3 7T. d1#a3
	 8S. f2#f3 8T. b1#d2
	 9S. f3#a3;Mn@b6 9T. e2#c2
	 10S. e1#f1 10T. f1#d4
	 11S. c2#c3 11T. d4#c4
	 12S. b2#c2 12T. c1#e2;Ma@e6
	 



Since *Relativity* is one of the games supported by this package (i.e. the function `piecepack_relativity()`) then `ppn::read_ppn()` can be used to parse this PPN file and then `ppn::animate_game()` can be used to animate the parsed game:


``` r
library("gifski")
library("piecepackr")
library("ppn") # remotes::install_github("piecepackr/ppn")
library("tweenr")

envir <- game_systems("dejavu")
cfg <- as.list(envir$piecepack)
cfg$suit_color <- "black"
cfg$background_color.r1 <- "#E69F00"
cfg$background_color.r2 <- "#56B4E9"
cfg$background_color.r3 <- "#009E73"
cfg$background_color.r4 <- "#F0E442"
cfg$background_color.r5 <- "#D55E00"
cfg$background_color.r6 <- "#F079A7"
envir$piecepack <- pp_cfg(cfg)

ppn_file <- system.file("ppn/relativity.ppn", package = "ppn")
game <- read_ppn(ppn_file)[[1]]
animate_game(game, file = "man/figures/README-relativity.gif",
              annotate = FALSE,
              envir = envir, trans = op_transform, op_scale = 0.5,
              n_transitions = 3, n_pauses = 2, fps = 7)
```

