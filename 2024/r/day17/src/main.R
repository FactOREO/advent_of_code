# Overwrite bitwise operations to handle int64 values
bitShiftL <- function(x, shift) {
  if (shift < 0 || shift > 64) stop("Shift value must be between 0 and 64")
  bit64::as.integer64(as.numeric(x) * (2 ^ shift))
}

bitShiftR <- function(x, shift) {
  if (shift < 0 || shift > 64) stop("Shift value must be between 0 and 64")
  bit64::as.integer64(as.numeric(x) %/% (2 ^ shift))
}

bitXor <- function(a, b) {
  a_bit_array <- bit64::as.integer64(a) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  b_bit_array <- bit64::as.integer64(b) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  res <- vector("integer", 64L)
  for (i in seq_along(a_bit_array)) {
    if ((a_bit_array[[i]] != b_bit_array[[i]]) && (a_bit_array[[i]] == 1 || b_bit_array[[i]] == 1))
      res[[i]] <- 1L
  }
  bit64::as.integer64.bitstring(paste(res, collapse = ""))
}

bitAnd <- function(a, b) {
  a_bit_array <- bit64::as.integer64(a) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  b_bit_array <- bit64::as.integer64(b) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  res <- vector("integer", 64L)
  for (i in seq_along(a_bit_array)) {
    if ((a_bit_array[[i]] == b_bit_array[[i]]) && a_bit_array[[i]] == 1)
      res[[i]] <- 1L
  }
  bit64::as.integer64.bitstring(paste(res, collapse = ""))
}


bitOr <- function(a, b) {
  a_bit_array <- bit64::as.integer64(a) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  b_bit_array <- bit64::as.integer64(b) |> bit64::as.bitstring() |> strsplit("") |> unlist()
  res <- vector("integer", 64L)
  for (i in seq_along(a_bit_array)) {
    if(any(a_bit_array[[i]] == 1 | b_bit_array[[i]] == 1)) res[[i]] <- 1L
  }
  bit64::as.integer64.bitstring(paste(res, collapse = ""))
}

# Opcode 0 - Divide regA by 2^operand
# Opcode 6 - Do opcode 0 with regB
# Opcode 7 - Do opcode 0 with regC
adv <- function(register_A, operand) { #nolint
  # Update: Use bitwShiftR instead of division floor(register_A / (2 ^ operand))
  bitShiftR(register_A, operand)
}

# Opcode 1 - bitwiseXor of regB and operand
bxl <- function(register_B, operand) { #nolint
  bitXor(register_B, operand)
}

# Opcode 2 - operand modulo 8
bst <- function(operand) {
  # Update: Use bitwAnd instead of modulo 8
  bitAnd(operand, 7)
}

# Opcode 3 - jump instruction pointer to operand if regA != 0
jnz <- function(register_A, operand, instructions_pointer) { #nolint
  if (register_A != 0) return(operand)
  instructions_pointer
}

# Opcode 4 - bitwiseXor of regB and regC
bxc <- function(register_B, register_C) { #nolint
  bitXor(register_B, register_C)
}

# Opcode 5 - print operator modulo 8
out <- function(operand) {
  # Update: Use bitwAnd instead of modulo 8
  as.integer(bitAnd(operand, 7))
}

get_combo_operand <- function(state, operand) {
  if (operand %in% 0:3) return(operand)
  if (operand == 4) return(state[["regA"]])
  if (operand == 5) return(state[["regB"]])
  if (operand == 6) return(state[["regC"]])
}

#' Perform an instruction
#'
#' Alter a given state by opcode operation with operand
perform_instruction <- function(state, opcode, operand) {
  regA <- state[["regA"]] #nolint
  regB <- state[["regB"]] #nolint
  regC <- state[["regC"]] #nolint
  instructions_pointer <- state[["instructions_pointer"]]
  out <- state[["out"]]
  switch(as.character(opcode),
    "0" = {
      operand <- get_combo_operand(state, operand)
      state[["regA"]] <- adv(regA, operand)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "1" = {
      state[["regB"]] <- bxl(regB, operand)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "2" = {
      operand <- get_combo_operand(state, operand)
      state[["regB"]] <- bst(operand)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "3" = {
      state[["instructions_pointer"]] <- jnz(regA, operand, instructions_pointer)
      state
    },
    "4" = {
      state[["regB"]] <- bxc(regB, regC)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "5" = {
      operand <- get_combo_operand(state, operand)
      state[["out"]] <- c(out, out(operand))
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "6" = {
      operand <- get_combo_operand(state, operand)
      state[["regB"]] <- adv(regA, operand)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    },
    "7" = {
      operand <- get_combo_operand(state, operand)
      state[["regC"]] <- adv(regA, operand)
      state[["instructions_pointer"]] <- instructions_pointer + 2
      state
    }
  )
}

#' Go through the instructions chain
perform_instructions <- function(state, instructions) {
  ptr <- state[["instructions_pointer"]]
  pptr <- -1
  while (ptr >= 0 && ptr != pptr && ptr < length(instructions)) {
    state <- perform_instruction(state, instructions[[ptr + 1]], instructions[[ptr + 2]])
    pptr <- ptr
    ptr <- state[["instructions_pointer"]]
  }
  state
}

#' Recreate instructions with register value
backtrack <- function(state, instructions) {
  get_next_digit <- function(state, instructions, current, index) {
    for (i in seq.default(0, 7)) {
      # Update the value of register A by bitshifting by 3 and adding the offset
      # => current * 8 + i
      next_a <- bitOr(bitShiftL(current, 3),  i)
      state[["regA"]] <- next_a
      # Run the instructions
      res <- perform_instructions(state, instructions)[["out"]]
      # Check, if the first digit in the solution matches the value at the
      # i-th position of the instructions
      if (res[[1]] == instructions[[index]]) {
        # If we are at the most significant digit, return the candidate
        if (index == 1) {
          return(next_a)
        }
        # Else invoke the function recursively to get the next digit
        res <- get_next_digit(state, instructions, next_a, index - 1)
        if (!is.null(res)) return(res)
      }
    }
    # If no feasible digit could be found, return NULL
    NULL
  }
  get_next_digit(state, instructions, 0, length(instructions))
}

### Tests
testthat::test_that("If register C contains 9, the program 2,6 would set register B to 1.", { #nolint
  init_state <- list(regA = 0, regB = 0, regC = 9, instructions_pointer = 0, out = NULL)
  target_state <- list(regA = 0, regB = 1, regC = 9, instructions_pointer = 2, out = NULL)
  testthat::expect_equal(perform_instruction(init_state, 2, 6), target_state)
})

testthat::test_that("If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.", { #nolint
  init_state <- list(regA = 10, regB = 0, regC = 0, instructions_pointer = 0, out = NULL)
  target_output <- "0,1,2"
  testthat::expect_equal(
    perform_instructions(init_state, instructions = c(5, 0, 5, 1, 5, 4))[["out"]] |> paste(collapse = ","),
    target_output
  )
})

testthat::test_that("If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.", { #nolint
  init_state <- list(regA = 2024, regB = 0, regC = 0, instructions_pointer = 0, out = NULL)
  target_output <- "4,2,5,6,7,7,7,7,3,1,0"
  testthat::expect_equal(
    perform_instructions(init_state, instructions = c(0, 1, 5, 4, 3, 0))[["out"]] |> paste(collapse = ","),
    target_output
  )
  target_regA <- 0
  testthat::expect_equal(
    perform_instructions(init_state, instructions = c(0, 1, 5, 4, 3, 0))[["regA"]],
    target_regA
  )
})

testthat::test_that("If register B contains 29, the program 1,7 would set register B to 26.", { #nolint
  init_state <- list(regA = 0, regB = 29, regC = 0, instructions_pointer = 0, out = NULL)
  target_regB <- 26
  testthat::expect_equal(
    perform_instructions(init_state, instructions = c(1, 7))[["regB"]],
    target_regB
  )
})

testthat::test_that("If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.", { #nolint
  init_state <- list(regA = 0, regB = 2024, regC = 43690, instructions_pointer = 0, out = NULL)
  target_regB <- 44354
  testthat::expect_equal(
    perform_instructions(init_state, instructions = c(4, 0))[["regB"]],
    target_regB
  )
})

testthat::test_that("Example Case 1", {
  init_state <- list(regA = 729, regB = 0, regC = 0, instructions_pointer = 0, out = NULL)
  instructions <- c(0, 1, 5, 4, 3, 0)
  target_output <- "4,6,3,5,6,3,5,2,1,0"
  testthat::expect_equal(
    perform_instructions(init_state, instructions)[["out"]] |> paste(collapse = ","),
    target_output
  )
})

testthat::test_that("Program Copy", {
  init_state <- list(regA = 117440, regB = 0, regC = 0, instructions_pointer = 0, out = NULL)
  instructions <- c(0, 3, 5, 4, 3, 0)
  target_output <- "0,3,5,4,3,0"
  testthat::expect_equal(
    perform_instructions(init_state, instructions)[["out"]] |> paste(collapse = ","),
    target_output
  )
})

testthat::test_that("Program Copy Backtrack", {
  init_state <- list(regA = 0, regB = 0, regC = 0, instructions_pointer = 0, out = NULL)
  instructions <- c(0, 3, 5, 4, 3, 0)
  target_output <- 117440
  testthat::expect_equal(
    backtrack(init_state, instructions),
    target_output
  )
})

### Problem Solving
source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 17, session_cookie)
input <- readLines("../input/input.txt") |> strsplit(":") |> sapply(\(i) if (length(i)) trimws(i[[2]]))

# State of the computer
computer_state <- list(
  regA = as.integer(input[[1]]),
  regB = as.integer(input[[2]]),
  regC = as.integer(input[[3]]),
  instructions_pointer = 0,
  out = NULL
)

instructions <- strsplit(input[[5]], ",")[[1]] |> as.integer()
tictoc::tic()
p1 <- perform_instructions(computer_state, instructions)[["out"]] |> paste(collapse = ",")
tictoc::toc()
print_result(2024, 17, p1)

tictoc::tic()
p2 <- backtrack(computer_state, instructions)
tictoc::toc()

# Backcheck
testthat::test_that(
  "Backcheck Result of Part 2", {
  init_state <- list(
    regA = as.integer(input[[1]]),
    regB = as.integer(input[[2]]),
    regC = as.integer(input[[3]]),
    instructions_pointer = 0,
    out = NULL
  )
  instructions <- c(2, 4, 1, 2, 7, 5, 4, 5, 0, 3, 1, 7, 5, 5, 3, 0)
  target_regA <- backtrack(init_state, instructions)
  init_state[["regA"]] <- target_regA
  testthat::expect_equal(
    perform_instructions(init_state, instructions)[["out"]] |> paste(collapse = ","),
    paste(instructions, collapse = ",")
  )
})

print_result(2024, 17, p2)

# 0.014 sec elapsed
# The result for day 17 of AOC 2024 is: 4,3,7,1,5,3,0,5,4

# 0.524 sec elapsed
# The result for day 17 of AOC 2024 is: 190384615275535

