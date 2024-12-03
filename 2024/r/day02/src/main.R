source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
input <- get_aoc(2024, 2, session_cookie) |> strsplit(" ") |> lapply(as.integer)

p1 <- lapply(input, \(r) {
  d <- diff(r)
  if (any(abs(d) > 3) || any(d == 0))
    return(FALSE)
  all(d > 0) || all(d < 0)
}) |> unlist() |> sum()
print_result(2024, 2, p1)

safe <- rep(FALSE, length(input))
for(i in seq_along(input)){
  for(j in seq_along(input[[i]])){
    d <- diff(input[[i]][-j])
    if((all(d<0)|all(d>0)) & !(any(abs(d) > 3) || any(d == 0))){safe[i] <- TRUE}
  }
}
p2 <- sum(safe)
print_result(2024, 2, p2)
