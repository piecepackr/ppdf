fill_piece <- function(df) {
	df |>
		separate_piece_side() |>
		group_by(.data$piece) |>
		group_modify(\(d, k) fill_piece_group(d, k$piece)) |>
		ungroup() |>
		unite_piece_side()
}

fill_piece_group <- function(d, piece) {
	needs_idx <- which(is.na(d$suit) | is.na(d$rank))
	if (!length(needs_idx)) {
		return(d)
	}
	if (piece %in% c("pawn", "die")) {
		d$suit <- replace_na_piece(d$suit)
		d$rank[is.na(d$rank)] <- 1L
		return(d)
	}
	avail <- dplyr::anti_join(
		expand.grid(suit = 1:4, rank = 1:6),
		d[!is.na(d$suit) & !is.na(d$rank), c("suit", "rank")],
		by = c("suit", "rank")
	)
	avail <- avail[sample(nrow(avail)), ]
	compat <- lapply(needs_idx, function(i) {
		which(
			(is.na(d$suit[i]) | avail$suit == d$suit[i]) &
				(is.na(d$rank[i]) | avail$rank == d$rank[i])
		)
	})
	match_row <- bipartite_match(compat, nrow(avail))
	if (is.null(match_row)) {
		abort(
			"Cannot fill pieces: piece constraints are not satisfiable.",
			class = "ppdf_error"
		)
	}
	for (j in seq_along(needs_idx)) {
		i <- needs_idx[j]
		d$suit[i] <- avail$suit[match_row[j]]
		d$rank[i] <- avail$rank[match_row[j]]
	}
	d
}

bipartite_match <- function(compat, n_pairs) {
	n_rows <- length(compat)
	match_pair <- integer(n_pairs)
	match_row <- integer(n_rows)
	try_augment <- function(ri) {
		for (pi in compat[[ri]]) {
			if (!visited[pi]) {
				visited[pi] <<- TRUE
				old_ri <- match_pair[pi]
				if (old_ri == 0L || try_augment(old_ri)) {
					match_pair[pi] <<- ri
					match_row[ri] <<- pi
					return(TRUE)
				}
			}
		}
		FALSE
	}
	for (ri in seq_len(n_rows)) {
		visited <- logical(n_pairs)
		if (!try_augment(ri)) return(NULL)
	}
	match_row
}

fill_piece_rank <- function(df) {
	df |>
		separate_piece_side() |>
		group_by(.data$piece, .data$suit) |>
		mutate(rank = replace_na_piece(.data$rank)) |>
		ungroup() |>
		unite_piece_side()
}

fill_piece_suit <- function(df) {
	df |>
		separate_piece_side() |>
		group_by(.data$piece, .data$rank) |>
		mutate(suit = replace_na_piece(.data$suit)) |>
		ungroup() |>
		unite_piece_side()
}

# Avoids importing {stats}
na_omit <- function(x) Filter(Negate(is.na), x)

replace_na_piece <- function(x) {
	ina <- which(is.na(x))
	if (length(ina)) {
		x[ina] <- sample(setdiff(seq.int(length(x)), as.integer(na_omit(x))), length(ina))
		x
	} else {
		x
	}
}

separate_piece_side <- function(df) {
	tidyr::separate_wider_delim(df, "piece_side", "_", names = c("piece", "side"))
}

unite_piece_side <- function(df) {
	tidyr::unite(df, "piece_side", "piece", "side")
}
