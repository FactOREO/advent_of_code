priorities <- seq.int(1L, 52L, 1L)
names(priorities) <- c(letters, LETTERS)
input <- readLines("./input.txt")

split_into_compartments <- function(rucksack) {
  rucksack_len <- nchar(rucksack)
  c(
    substr(rucksack, 1, rucksack_len / 2),
    substr(rucksack, 1 + rucksack_len / 2, rucksack_len)
  )
}

find_common_letters <- function(...) {
  Reduce(intersect, lapply(list(...) |> unlist(), \(x) strsplit(x, "")[[1L]]))
}

part1 <- input |>
  sapply(
    \(rucksack) {
      compartments <- split_into_compartments(rucksack)
      common_letters <- find_common_letters(compartments[[1L]], compartments[[2L]])
      sum(priorities[common_letters])
    }
  ) |>
  sum()

part2 <- split(input, rep(seq.int(1L, length(input) / 3), each = 3L)) |>
  sapply(
    \(group) {
      batch <- find_common_letters(group)
      priorities[batch]
    }
  ) |>
  sum()

print(sprintf("Part 1: %d | Part 2: %d", part1, part2))
