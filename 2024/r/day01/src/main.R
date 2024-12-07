source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 1, session_cookie)
input <- readLines("../input/input.txt")

lists <- strsplit(input, "   ", fixed=TRUE) |> unlist()

left_list  <- lists[seq.int(from=1, to=length(lists), by=2)] |> as.integer()
right_list <- lists[seq.int(from=2, to=length(lists), by=2)] |> as.integer()

tictoc::tic()
p1 <- sum(abs(
    left_list[order(left_list)] - right_list[order(right_list)]
  ))
tictoc::toc()

print_result(2024, 1, p1)
# 0.001 sec elapsed
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

tictoc::tic()
p2 <- sapply(vals_left, \(v) {
  c <- get_count_from_table_by_value(v, table_right)
  v * c
}) |> sum()
tictoc::toc()

print_result(2024, 1, p2)
# 0.012 sec elapsed
# The result for day 1 of AOC 2024 is: 22545250
