input <- readLines("./input.txt")
s <- strsplit(input, " ")
check <- function(x, nums) {
  r <- rle(x)
  identical(r[["lengths"]][r[["values"]] == "#"], nums)
}
chars <- strsplit(sapply(s, function(x) x[[1]]), "")
nums <- lapply(strsplit(sapply(s, function(x) x[[2]]), ","), as.integer)
res <- 0
for (i in seq_along(chars)){
  o <- chars[[i]]
  s <- sum(o == "?")
  O <- matrix(o, nrow = 2^s, ncol = length(o), byrow = TRUE)
  S <- 2^((s:1) - 1)
  q <- which(o == "?")
  for (j in seq_along(q)) {
    O[, q[[j]]] <- rep(c(".", "#"), each = S[[j]])
  }
  res <- res + sum(apply(O, 1, check, nums[[i]]))
  cat(i, "\r")
}
print(res)
