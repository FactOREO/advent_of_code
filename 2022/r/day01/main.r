proviants <- readLines("./input.txt") |>
  paste(collapse = "_") |>
  (\(x) strsplit(x, "__")[[1L]])() |>
  sapply(
    FUN = \(x) {
      strsplit(x, "_")[[1L]] |> as.integer() |> sum()
    }
  )

part1 <- max(proviants)
part2 <- sort(proviants, decreasing = TRUE) |> head(3) |> sum()

print(sprintf("Part 1: %d | Part 2: %d", part1, part2))
