source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 13, session_cookie)

input <- readLines("../input/input.txt")

# Clean the input strings and convert into the claw machine settings
# Per entry: X_A, Y_A, X_B, Y_B, X, Y
claw_machines <- input |>
  gsub("Button (A|B):", "", x = _) |>
  gsub("Prize:", "", x = _) |>
  gsub("(X|Y)[\\\\+=]", "", x = _) |>
  trimws() |>
  strsplit(", ") |>
  unlist() |>
  (\(x) {
    lapply(seq.int(1L, length(x) / 6L) - 1L, \(i) {
      indices <- seq.int(i * 6L + 1L, i * 6L + 6L, 1L)
      as.integer(x[indices])
    })
  })() |>
  do.call(rbind, args = _)

#' Solve the optimization problem for cost optimal claw machine playing
#'
#' Given a Claw machine with two Buttons A and B which move the Claw by
#' `X_A` and `Y_A` / `X_B` and `Y_B`. Find the cost optimal solution to
#' reach the target position c(`X`, `Y`) s.t. the sum of moves for A and
#' B with cost `c_A` and `c_B` is minimal.
#'
#' min C = c_a * A + c_B * b
#'  where a is the number of moves from A with cost c_A
#'        b is the number of moves from B with cost c_B
#' s.t. X_A * a + X_B * b = X and
#'      Y_A * a + Y_B * b = Y
calc_optim <- function(X_A, Y_A, c_A, X_B, Y_B, c_B, X, Y, part = 1) {
  # Coefficient matrix and target vector
  A <- matrix(c(
    X_A, X_B,
    Y_A, Y_B
    ), ncol = 2, byrow = TRUE
  )
  B <- c(X, Y)
  naive_solution <- solve(A, B)
  if (
    length(naive_solution) && all(naive_solution > 0) &&
    round(naive_solution[[1L]]) * X_A + round(naive_solution[[2L]]) * X_B == X &&
    round(naive_solution[[1L]]) * Y_A + round(naive_solution[[2L]]) * Y_B == Y
  ) {
    solution <- round(linprog::solveLP(
      c(c_A, c_B), B, A, const.dir = c("==", "=="), lpSolve=TRUE, tol=1)$solution)
    if (part == 1 && all(solution <= 100L)) {
      sum(solution * c(c_A, c_B))
    } else if (part == 2) {
      sum(solution * c(c_A, c_B))
    }
  }
}

# Solve Part 1
tictoc::tic()
p1 <- apply(claw_machines, 1L, \(machine) {
  calc_optim(
    X_A = machine[[1L]], Y_A = machine[[2L]],
    X_B = machine[[3L]], Y_B = machine[[4L]],
    c_A = 3L           , c_B = 1L,
    X   = machine[[5L]], Y   = machine[[6L]]
  )
}) |> unlist() |> sum()
tictoc::toc()
print_result(2024, 13, p1)

# Solve Part 2
claw_machines[, 5L] <- claw_machines[, 5L] + 10000000000000
claw_machines[, 6L] <- claw_machines[, 6L] + 10000000000000
tictoc::tic()
p2 <- apply(claw_machines, 1L, \(machine) {
  calc_optim(
    X_A = machine[[1L]], Y_A = machine[[2L]],
    X_B = machine[[3L]], Y_B = machine[[4L]],
    c_A = 3L           , c_B = 1L,
    X   = machine[[5L]], Y   = machine[[6L]],
    part = 2
  )
}) |> unlist() |> sum()
tictoc::toc()
print_result(2024, 13, p2)
