source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 2, session_cookie)
input <- readLines("../input/input.txt") |> strsplit(" ") |> lapply(as.integer)

tictoc::tic()
p1 <- lapply(input, \(r) {
  d <- diff(r)
  if (any(abs(d) > 3) || any(d == 0))
    return(FALSE)
  all(d > 0) || all(d < 0)
}) |> unlist() |> sum()
tictoc::toc()

print_result(2024, 2, p1)
# 0.01 sec elapsed
# The result for day 2 of AOC 2024 is: 314

tictoc::tic()
safe <- rep(FALSE, length(input))
for(i in seq_along(input)){
  for(j in seq_along(input[[i]])){
    d <- diff(input[[i]][-j])
    if((all(d<0)|all(d>0)) & !(any(abs(d) > 3) || any(d == 0))){safe[i] <- TRUE}
  }
}
p2 <- sum(safe)
tictoc::toc()

print_result(2024, 2, p2)
# 0.052 sec elapsed
# The result for day 2 of AOC 2024 is: 373
