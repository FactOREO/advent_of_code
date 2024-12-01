source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
input <- get_aoc(2024, 1, session_cookie)

lists <- strsplit(input, "   ", fixed=TRUE) |> unlist()

left_list  <- lists[seq.int(from=1, to=length(lists), by=2)] |> as.integer()
right_list <- lists[seq.int(from=2, to=length(lists), by=2)] |> as.integer()

p1 <- sum(abs(
    left_list[order(left_list)] - right_list[order(right_list)]
  ))

print("Part 1:")
print_result(2024, 1, p1)
# The result for day 1 of AOC 2024 is: 1222801

vals_left <- unique(left_list)
table_right <- table(right_list)

get_count_from_table_by_value <- function(v, t) {
  v <- as.character(v)
  if (v %in% names(t)) {
    return(t[[v]])
  }
  0L
}

p2 <- sapply(vals_left, \(v) {
  c <- get_count_from_table_by_value(v, table_right)
  v * c
}) |> sum()

print("Part 2:")
print_result(2024, 1, p2)
# The result for day 1 of AOC 2024 is: 22545250
