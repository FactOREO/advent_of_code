input <- readLines("./input.txt")

range_fully_contained <- function(ranges) {
  m <- strsplit(ranges, ",")[[1L]] |>
    strsplit("-") |>
    unlist() |>
    as.integer() |>
    matrix(ncol = 2L)
  (
    m[[1, 1]] >= m[[1, 2]] && m[[2, 1]] <= m[[2, 2]]
  ) || (
    m[[1, 2]] >= m[[1, 1]] && m[[2, 2]] <= m[[2, 1]]
  )
}

get_overlapping_range <- function(ranges) {
  v <- strsplit(ranges, ",")[[1L]] |>
    strsplit("-") |>
    unlist() |>
    as.integer()
  l <- max(v[[1L]], v[[3L]])
  u <- min(v[[2L]], v[[4L]])
  # Number of overlapping assignment pairs
  # if (l > u) return(0L)
  # u - l + 1L
  if (l > u) return(FALSE)
  TRUE
}

part1 <- sum(sapply(input, range_fully_contained))
part2 <- sum(sapply(input, get_overlapping_range))
print(sprintf("Part 1: %d | Part 2: %d", part1, part2))
