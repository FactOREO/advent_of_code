# Camel Cards
# Order: 2, 3, ..., 9, J, Q, K, A
# Five cards
# Hands: High - Pair - Two Pair - Three - Full House - Four - Five
library("data.table")

main <- function(file) {
  data <- fread(file, header = FALSE, colClasses = c("character", "numeric"), col.names = c("hand", "bid"))
  cards <- c(as.character(2:9), "T", "J", "Q", "K", "A")
  types <- c("highest card", "one pair", "two pairs",
             "three of a kind", "full house", "four of a kind", "five of a kind")
  ### PART 1
  data[, type := factor(get_hand_type(hand), levels = types)]
  # Expand the hand to cards
  data[, (paste0("card_", 1:5)) := tstrsplit(hand, "")]
  data[, (paste0("card_", 1:5)) := lapply(.SD, factor, levels = cards), .SDcols = paste0("card_", 1:5)]
  # Sort the frames by hand type and cards from left to right
  data <- collapse::rsplit(data, ~ type) |>
    collapse::rapply2d(function(data) {
      data[order(card_1, card_2, card_3, card_4, card_5)]
    }) |>
    collapse::unlist2d(idcols = "type", DT = TRUE)
  data[, rank := seq.default(1, .N)][]
  # Calculate sum(bid * rank)
  print(sprintf("Part 1: %s",
                collapse::fsum(data$bid * data$rank)
  ))
  ### PART 2
  data <- data[, .(hand, bid)]
  cards <- c("J", as.character(2:9), "T", "Q", "K", "A")
  data[, type := factor(get_best_hand_type(hand), levels = types)]
  # Expand the hand to cards
  data[, (paste0("card_", 1:5)) := tstrsplit(hand, "")]
  data[, (paste0("card_", 1:5)) := lapply(.SD, factor, levels = cards), .SDcols = paste0("card_", 1:5)]
  data <- collapse::rsplit(data, ~ type) |>
    collapse::rapply2d(function(data) {
      data[order(card_1, card_2, card_3, card_4, card_5)]
    }) |>
    collapse::unlist2d(idcols = "type", DT = TRUE)
  data[, rank := seq.default(1, .N)][]
  print(sprintf("Part 2: %s",
                collapse::fsum(data$bid * data$rank)
  ))
}

data  <- main("./input.txt")

# Function to get the correct hand type for a given hand consisting of 5 individual cards
get_hand_type <- Vectorize(function(hand) {
  fcase(
    # All different
    length(unique(strsplit(hand, "")[[1]])) == 5, "highest card",
    # 4 different cards == one pair present
    length(unique(strsplit(hand, "")[[1]])) == 4, "one pair",
    # 3 different cards == 2 pairs or three of a kind
    length(unique(strsplit(hand, "")[[1]])) == 3,
    fifelse(any(table(strsplit(hand, "")[[1]]) == 3), "three of a kind", "two pairs"),
    # 2 different cards == full house or 4 of a kind
    length(unique(strsplit(hand, "")[[1]])) == 2,
    fifelse(any(table(strsplit(hand, "")[[1]]) == 3), "full house", "four of a kind"),
    default = "five of a kind")
}, vectorize.args = c("hand"))

# Function to get the best possible hand type with a given hand
get_best_hand_type <- Vectorize(function(hand) {
  non_jokers <- strsplit(hand, "")[[1]]
  jokers <- non_jokers[non_jokers == "J"]
  non_jokers <- non_jokers[non_jokers != "J"]
  # Determine which set would be the best
  if (length(jokers) > 0) {
    fcase(
      # ===
      # only jokers and one distinct "non-joker" card given || only jokers
      length(jokers) %in% c(4, 5),
        "five of a kind",
      # ===
      # there are two "non-joker" cards present
      length(jokers) == 3,
        fcase(
          # 1 + 1 + 3 jokers
          length(unique(non_jokers)) == 2, "four of a kind",
          # 2 + 3 jokers
          length(unique(non_jokers)) == 1, "five of a kind"),
      # ===
      # there are three "non-joker" cards present
      length(jokers) == 2,
        fcase(
          # 1 + 1 + 1 + 2 jokers |
          length(unique(non_jokers)) == 3,  "three of a kind",
          # 1 + 2 + 2 jokers
          length(unique(non_jokers)) == 2,  "four of a kind",
          # 3 + 2 jokers
          length(unique(non_jokers)) == 1,  "five of a kind"),
      # ===
      # there are four "non-joker" cards present
      length(jokers) == 1,
        fcase(
          # 1 + 1 + 1 + 1 + 1 joker
          length(unique(non_jokers)) == 4, "one pair",
          # 1 + 1 + 2 + 1 joker
          length(unique(non_jokers)) == 3, "three of a kind",
          # 1 + 3 + 1 joker OR 2 + 2 + 1 joker
          length(unique(non_jokers)) == 2,
            fifelse(any(table(non_jokers) == 3), "four of a kind", "full house"),
          # 4 + 1 joker
          length(unique(non_jokers)) == 1, "five of a kind")
      )
  } else {
    get_hand_type(hand)
  }
}, vectorize.args = c("hand"))
