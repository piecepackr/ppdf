uc_seq <- function(glyph, seq) {
    intToUtf8(utf8ToInt(glyph) + seq, multiple = TRUE)
}

unicode_dice <- uc_seq("\u2680", 0:5)

unicode_cards <- c(uc_seq("\U0001f0a1", 0:13), # spades
                   uc_seq("\U0001f0b1", 0:13), # hearts
                   uc_seq("\U0001f0c1", 0:13), # diamonds
                   uc_seq("\U0001f0d1", 0:13), # clubs
                   "\U0001f0bf", "\U0001f0cf", "\U0001f0df", # jokers
                   uc_seq("\U0001f0e0", 0:21), # trumps
                   "\U0001f0a0") # card back
card2rank <- list()
for (r in 1:14) {
    card2rank[[unicode_cards[r]]] <- r
    card2rank[[unicode_cards[r+14L]]] <- r
    card2rank[[unicode_cards[r+28L]]] <- r
    card2rank[[unicode_cards[r+42L]]] <- r
}
card2rank[[unicode_cards[57L]]] <- 15L
card2rank[[unicode_cards[58L]]] <- 15L
card2rank[[unicode_cards[59L]]] <- 15L
card2rank[[unicode_cards[60L]]] <- 22L
for (r in 1:21) {
    card2rank[[unicode_cards[r+60L]]] <- r
}
card2rank[[unicode_cards[82L]]] <- NA_integer_

card2suit <- list()
for (r in 1:14) {
    card2suit[[unicode_cards[r]]] <- 2L
    card2suit[[unicode_cards[r+14L]]] <- 1L
    card2suit[[unicode_cards[r+28L]]] <- 4L
    card2suit[[unicode_cards[r+42L]]] <- 3L
}
card2suit[[unicode_cards[57L]]] <- 4L # 3rd "red" joker
card2suit[[unicode_cards[58L]]] <- 2L # 1st "black" joker
card2suit[[unicode_cards[59L]]] <- 1L # 2nd "white" joker
card2suit[[unicode_cards[60L]]] <- 5L
for (r in 1:21) {
    card2suit[[unicode_cards[r+60L]]] <- 5L
}
card2suit[[unicode_cards[82L]]] <- NA_integer_

unicode_chess_black <- uc_seq("\u265f", seq.int(0L, -5L))
unicode_chess_white <- uc_seq("\u2659", seq.int(0L, -5L))
unicode_chess_neutral <- uc_seq("\U0001fa05", seq.int(0L, -5L))
# unicode rotates clockwise while we rotate counter-clockwise
unicode_chess_white270 <- uc_seq("\U0001fa0e", seq.int(0L, -5L))
unicode_chess_black270 <- uc_seq("\U0001fa14", seq.int(0L, -5L))
unicode_chess_neutral270 <- uc_seq("\U0001fa1a", seq.int(0L, -5L))
unicode_chess_white180 <- uc_seq("\U0001fa23", seq.int(0L, -5L))
unicode_chess_black180 <- uc_seq("\U0001fa29", seq.int(0L, -5L))
unicode_chess_neutral180 <- uc_seq("\U0001fa2f", seq.int(0L, -5L))
unicode_chess_white90 <- uc_seq("\U0001fa38", seq.int(0L, -5L))
unicode_chess_black90 <- uc_seq("\U0001fa3e", seq.int(0L, -5L))
unicode_chess_neutral90 <- uc_seq("\U0001fa44", seq.int(0L, -5L))

# Chess knights rotated 45 degrees etc.
unicode_knights315 <- uc_seq("\U0001fa06", seq.int(0L, 2L))
unicode_knights225 <- uc_seq("\U0001fa1b", seq.int(0L, 2L))
unicode_knights135 <- uc_seq("\U0001fa30", seq.int(0L, 2L))
unicode_knights45 <- uc_seq("\U0001fa45", seq.int(0L, 2L))

unicode_dominoes <- uc_seq("\U0001f030", 0:99)
ranks <- c(NA_integer_, rep(0L, 7), # 0H
           0L, rep(1L, 6), # 1H
           0:1, rep(2L, 5), # 2H
           0:2, rep(3L, 4), # 3H
           0:3, rep(4L, 3), # 4H
           0:4, rep(5L, 2), # 5H
           0:5, 6L) # 6H
domino_ranks <- c(ranks, ranks)
suits <- c(NA_integer_, 0:6, # 0H
           rep(1L, 2), 2:6, # 1H
           rep(2L, 3), 3:6,
           rep(3L, 4), 4:6,
           rep(4L, 5), 5:6,
           rep(5L, 6), 6L,
           rep(6L, 7))
domino_suits <- c(suits, suits)
angles <- c(90, rep(90, 7),  # 0H
            rep(270, 1), rep(90, 6), # 1H
            rep(270, 2), rep(90, 5), # 2H
            rep(270, 3), rep(90, 4), # 3H
            rep(270, 4), rep(90, 3), # 4H
            rep(270, 5), rep(90, 2), # 5H
            rep(270, 6), rep(90, 1)) # 6H
domino_angles <- c(angles, angles - 90)

suit_list <- list(
    suns = 1L,
    sun = 1L,
    s = 1L,
    moons = 2L,
    moon = 2L,
    m = 2L,
    crowns = 3L,
    crown = 3L,
    c = 3L,
    arms = 4L,
    arm = 4L,
    a = 4L,
    hearts = 1L,
    heart = 1L,
    "\u2665" = 1L,
    "\u2661" = 1L,
    cups = 1L,
    cup = 1L,
    shields = 1L,
    shield = 1L,
    spades = 2L,
    spade = 2L,
    "\u2664" = 2L,
    "\u2660" = 2L,
    swords = 2L,
    sword = 2L,
    leaves = 2L,
    leaf = 2L,
    roses = 2L,
    rose = 2L,
    pikes = 2L,
    pike = 2L,
    clubs = 3L,
    club = 3L,
    "\u2667" = 3L,
    "\u2663" = 3L,
    acorns = 3L,
    acorn = 3L,
    clovers = 3L,
    clover = 3L,
    diamonds = 4L,
    diamond = 4L,
    "\u2666" = 4L,
    "\u2662" = 4L,
    coins = 4L,
    coin = 4L,
    bells = 4L,
    bell = 4L,
    tiles = 4L,
    tile = 4L,
    # stars = 5L,
    # star = 5L,
    # "\u2605" = 5L,
    # "\u2606" = 5L,
    atout = 5L,
    trumps = 5L,
    trump = 5L,
    reds = 1L,
    red = 1L,
    r = 1L,
    blacks = 2L,
    black = 2L,
    k = 2L,
    greens = 3L,
    green = 3L,
    g = 3L,
    blues = 4L,
    blue = 4L,
    b = 4L,
    yellows = 5L,
    yellow = 5L,
    y = 5L,
    whites = 6L,
    white = 6L,
    w = 6L,
    "\u25cb" <- 6L, # white (default go) bit
    "\u25cf" <- 2L, # black (default go) bit
    "\u26c0" <- 6L, # white checkers bit
    "\u26c1" <- 6L, # white checkers bit
    "\u26c2" <- 2L, # black checkers bit
    "\u26c3" <- 2L  # black checkers bit
)
for (g in c(unicode_chess_black, unicode_chess_black90, unicode_chess_black180, unicode_chess_black270)) {
    suit_list[[g]] <- 2L
}
for (g in c(unicode_chess_white, unicode_chess_white90, unicode_chess_white180, unicode_chess_white270)) {
    suit_list[[g]] <- 6L
}
# For now let "neutral" chess pieces be the "red" suit
for (g in c(unicode_chess_neutral, unicode_chess_neutral90, unicode_chess_neutral180, unicode_chess_neutral270)) {
    suit_list[[g]] <- 1L
}
knight_suits <- c(6L, 2L, 1L)
for (i in 1:3) {
    suit_list[[unicode_knights45[i]]] <- knight_suits[i]
    suit_list[[unicode_knights135[i]]] <- knight_suits[i]
    suit_list[[unicode_knights225[i]]] <- knight_suits[i]
    suit_list[[unicode_knights315[i]]] <- knight_suits[i]
}
for (g in c(unicode_cards)) {
    suit_list[[g]] <- as.integer(card2suit[[g]])
}

#### Percentile dice
#### Fudge dice

# Can't be combined with tarot rank list due to 0 being 10 on d10's
# Can't be combined with percentile dice due to 10
dice_rank_list <- list(
    zero = 10L,
    `0` = 10L,
    one = 1L,
    ace = 1L,
    a = 1L,
    two = 2L,
    deuce = 2L,
    three = 3L,
    four = 4L,
    five = 5L,
    six = 6L,
    seven = 7L,
    eight = 8L,
    nine = 9L,
    ten = 10L,
    eleven = 11L,
    twelve = 12L,
    thirteen = 13L,
    fourteen = 14L,
    fifteen = 15L,
    sixteen = 16L,
    seventeen = 17L,
    eighteen = 18L,
    nineteen = 19L,
    twenty = 20L
)
for (i in seq.int(20L)) { # d20
    dice_rank_list[[as.character(i)]] <- as.integer(i)
}
for (i in seq_along(unicode_dice)) {
    dice_rank_list[[unicode_dice[i]]] <- as.integer(i)
}

# assume `tolower()` will have been used on keys
# Can't be combined with chess ranks due to knight/queen/king
# Can't be combined with dice ranks due to 0 being fool (22L)
tarot_rank_list <- list(
    zero = 22L, # the fool/excuse
    `0` = 22L, # the fool/excuse
    excuse = 22L,
    e = 22L,
    fool = 22L,
    f = 22L,
    one = 1L,
    ace = 1L,
    a = 1L,
    two = 2L,
    deuce = 2L,
    three = 3L,
    four = 4L,
    five = 5L,
    six = 6L,
    seven = 7L,
    eight = 8L,
    nine = 9L,
    ten = 10L,
    eleven = 11L,
    jack = 11L,
    j = 11L,
    valet = 11L,
    v = 11L,
    twelve = 12L,
    knight = 12L,
    n = 12L,
    cavalier = 12L,
    chevalier = 12L,
    c = 12L,
    thirteen = 13L,
    queen = 13L,
    q = 13L,
    dame = 13L,
    d = 13L,
    fourteen = 14L,
    king = 14L,
    k = 14L,
    roi = 14L,
    r = 14L,
    fifteen = 15L,
    joker = 15L, # can't use `j`
    sixteen = 16L,
    seventeen = 17L,
    eighteen = 18L,
    nineteen = 19L,
    twenty = 20L,
    "twenty-one" = 21L,
    "twenty one" = 21L,
    "twenty-two" = 22L,
    "twenty two" = 22L
)
for (i in seq.int(22L)) { # 22 tarot trump cards
    tarot_rank_list[[as.character(i)]] <- as.integer(i)
}
for (g in c(unicode_cards)) {
    tarot_rank_list[[g]] <- as.integer(card2rank[[g]])
}

# Can't be combined with tarot ranks due to knight/queen/king
chess_rank_list <- list(
    pawn = 1L,
    p = 1L,
    knight = 2L,
    n = 2L,
    bishop = 3L,
    b = 3L,
    rook = 4L,
    r = 4L,
    queen = 5L,
    q = 5L,
    king = 6L,
    k = 6L
)
for (i in seq.int(6L)) {
    chess_rank_list[[unicode_chess_white[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_black[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_neutral[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_white90[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_black90[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_neutral90[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_white180[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_black180[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_neutral180[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_white270[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_black270[i]]] <- as.integer(i)
    chess_rank_list[[unicode_chess_neutral270[i]]] <- as.integer(i)
}
for (g in c(unicode_knights45, unicode_knights135, unicode_knights225, unicode_knights315)) {
    chess_rank_list[[g]] <- 2L
}

# piecepack and dominoes
# Domino rank is the "top" number
start_from_zero_rank_list <- list(
    zero = 1L,
    null = 1L,
    n = 1L,
    one = 2L,
    ace = 2L,
    a = 2L,
    two = 3L,
    three = 4L,
    four = 5L,
    five = 6L,
    six = 7L,
    seven = 8L,
    eight = 9L,
    nine = 10L,
    ten = 11L,
    eleven = 12L,
    twelve = 13L,
    thirteen = 14L,
    fourteen = 15L,
    fifteen = 16L,
    sixteen = 17L,
    seventeen = 18L,
    eighteen = 19L
)
for (i in seq.int(0, 18)) {
    start_from_zero_rank_list[[as.character(i)]] <- as.integer(i + 1L)
}

domino_suit_list <- start_from_zero_rank_list

angle_list <- list(
    "0" = 0,
    "^" = 0,
    "90" = 90,
    "-270" = 90,
    "<" = 90,
    "180" = 180,
    "-180" = 180,
    "v" = 180,
    "270" = 270,
    "-90" = 270,
    ">" = 270
)
for (g in c(unicode_chess_white, unicode_chess_black, unicode_chess_neutral)) {
    angle_list[[g]] <- 0
}
for (g in unicode_knights45) {
    angle_list[[g]] <- 45
}
for (g in c(unicode_chess_white90, unicode_chess_black90, unicode_chess_neutral90)) {
    angle_list[[g]] <- 90
}
for (g in unicode_knights135) {
    angle_list[[g]] <- 135
}
for (g in c(unicode_chess_white180, unicode_chess_black180, unicode_chess_neutral180)) {
    angle_list[[g]] <- 180
}
for (g in unicode_knights225) {
    angle_list[[g]] <- 225
}
for (g in c(unicode_chess_white270, unicode_chess_black270, unicode_chess_neutral270)) {
    angle_list[[g]] <- 270
}
for (g in unicode_knights315) {
    angle_list[[g]] <- 315
}

for (i in seq_along(unicode_dominoes)) {
    d <- unicode_dominoes[i]
    start_from_zero_rank_list[[d]] <- domino_ranks[i]
    domino_suit_list[[d]] <- domino_suits[i]
    angle_list[[d]] <- domino_angles[i]
}

save(angle_list, # angles
     chess_rank_list, # chess ranks
     dice_rank_list, # dice ranks
     domino_suit_list, # domino suits
     start_from_zero_rank_list, # piecepack/domino ranks
     suit_list, # non-domino suits
     tarot_rank_list, # (tarot) playing card ranks
     file = "R/sysdata.rda", version = 2)
