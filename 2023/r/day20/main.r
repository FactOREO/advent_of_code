input <- readLines("./input.txt")

# FlipFlop object
FlipFlop <- R6::R6Class("FlipFlop",
  public = list(
    name = NULL,
    receivers = NULL,
    state = "off",
    low_pulse_count = 0L,
    high_pulse_count = 0L,
    initialize = function(module) {
      module <- strsplit(module, " -> ")[[1L]]
      self$name <- sub("(%|&)", "", module[[1L]])
      self$receivers <- strsplit(module[[2L]], ",")[[1L]] |> trimws()
    },
    handle_pulse = function(pulse, queue) {
      # queue: matrix [sender, receiver, pulse]
      if (pulse == "low") {
        # Switch state if low pulse
        self$state <- ifelse(self$state == "off", "on", "off")
        # Send low pulse if state is now off, else send high pulse
        pulse <- ifelse(self$state == "off", "low", "high")
        ifelse(pulse == "high", self$high_pulse_count <- self$high_pulse_count + length(self$receivers), self$low_pulse_count <- self$low_pulse_count + length(self$receivers))
        queue <- rbind(queue[-1, ], matrix(c(cbind(self$name, self$receivers, pulse)), ncol = 3))
        return(queue)
      } else {
        return(matrix(c(queue[-1, ]), ncol = 3L))
      }
    })
)

# Conjunctor Object
Conjunctor <- R6::R6Class("Conjunctor",
  public = list(
    name = NULL,
    receivers = NULL,
    historic_states = NULL,
    low_pulse_count = 0L,
    high_pulse_count = 0L,
    initialize = function(module, modules) {
      module <- strsplit(module, " -> ")[[1L]]
      self$name <- sub("(%|&)", "", module[[1L]])
      self$receivers <- strsplit(module[[2L]], ",")[[1L]] |> trimws()
      # Find all input modules and create a named vector of this length with "low"
      input_modules <- modules[which(grepl(sprintf("->.*%s", self$name), modules))]
      input_modules_names <- lapply(strsplit(input_modules, " -> "), \(x) sub("(%|&)", "", x[[1L]])) |> unlist()
      self$historic_states <- rep("low", length = length(input_modules))
      names(self$historic_states) <- input_modules_names
    },
    handle_pulse = function(sender, pulse, queue) {
      # queue: matrix [sender, receiver, pulse]
      self$historic_states[[sender]] <- pulse
      if (all(self$historic_states == "high")) {
        self$low_pulse_count <- self$low_pulse_count + length(self$receivers)
        if (nrow(queue) > 1L) {
          queue <- rbind(queue[-1, ], matrix(c(cbind(self$name, self$receivers, "low")), ncol = 3L))
        } else {
          queue <- matrix(c(cbind(self$name, self$receivers, "low")), ncol = 3L)
        }
      } else {
        self$high_pulse_count <- self$high_pulse_count + length(self$receivers)
        if (nrow(queue) > 1L) {
          queue <- rbind(queue[-1, ], matrix(c(cbind(self$name, self$receivers, "high")), ncol = 3L))
        } else {
          queue <- matrix(c(cbind(self$name, self$receivers, "high")), ncol = 3L)
        }
      }
      colnames(queue) <- c("sender", "receiver", "pulse")
      return(queue)
    }
  )
)

Broadcaster <- R6::R6Class("Broadcaster",
  public = list(
    name = "broadcaster",
    receivers = NULL,
    low_pulse_count = 0L,
    initialize = function(module) {
      module <- strsplit(module, " -> ")[[1L]]
      self$receivers <- strsplit(module[[2L]], ",")[[1L]] |> trimws()
    },
    send_pulse = function() {
      self$low_pulse_count <- self$low_pulse_count + length(self$receivers)
      return(cbind(self$name, self$receivers, "low"))
    }
  )
)

update_queue <- function(queue) {
  sender <- queue[[1, 1]]
  receiver <- queue[[1, 2]]
  pulse <- queue[[1, 3]]
  if (receiver %in% names(flip_flop_modules)) {
    queue <- flip_flop_modules[[receiver]]$handle_pulse(pulse, queue)
  } else if (receiver %in% names(conjuncture_modules)) {
    queue <- conjuncture_modules[[receiver]]$handle_pulse(sender, pulse, queue)
  } else {
    queue <- matrix(c(queue[-1, ]), ncol = 3)
  }
  queue
}

# Calculate the amount of low and high pulses
get_pulse_counts <- function(low_count, conjuncture_modules, flip_flop_modules, broadcaster) {
  high_count <- 0L
  for (m in names(c(conjuncture_modules, flip_flop_modules))) {
    if (m %in% names(conjuncture_modules)) {
      low_count <- low_count + conjuncture_modules[[m]]$low_pulse_count
      high_count <- high_count + conjuncture_modules[[m]]$high_pulse_count
    } else {
      low_count <- low_count + flip_flop_modules[[m]]$low_pulse_count
      high_count <- high_count + flip_flop_modules[[m]]$high_pulse_count
    }
  }
  low_count <- low_count + broadcaster$low_pulse_count
  c(low_count, high_count)
}

# Greatest common denominator
gcd <- function(a, b) {
  while (a != b) {
    if (a > b) a <- a - b else b <- b - a
  }
  a
}

# Calculat the LCM (see day08)
lcm <- function(a, b) {
  abs(a * b) / gcd(a, b)
}

# Initialize all the input objects dependent on the % or & sign
flip_flop_modules <- sapply(input[which(grepl("%", input))], function(m) FlipFlop$new(module = m))
names(flip_flop_modules) <- unlist(lapply(flip_flop_modules, \(f) f$name))
conjuncture_modules <- sapply(input[which(grepl("&", input))], function(m) Conjunctor$new(module = m, modules = input))
names(conjuncture_modules) <- unlist(lapply(conjuncture_modules, \(f) f$name))
broadcaster <- Broadcaster$new(input[which(grepl("broadcaster", input))])
# Initialize the queue and counters
low_count <- 0L
cycles <- 0L
# Condition for a full button cycle: identical input state
orig_state <- c(unlist(lapply(conjuncture_modules, \(m) m$historic_states)), unlist(lapply(flip_flop_modules, \(m) m$state)))

while (TRUE) {
  # Push the button
  low_count <- low_count + 1L
  # Initialize the queue
  queue <- broadcaster$send_pulse()
  # Count the cycles seen
  cycles <- cycles + 1L
  # Now go through the queue from top to bottom
  while (nrow(queue) > 0) {
    queue <- update_queue(queue)
  }
  current_state <- c(unlist(lapply(conjuncture_modules, \(m) m$historic_states)),
                     unlist(lapply(flip_flop_modules, \(m) m$state)))
  if (identical(orig_state, current_state)) break
  if (cycles == 1000L) break
}

counts <- get_pulse_counts(low_count, conjuncture_modules, flip_flop_modules, broadcaster)

# Calculate the result
print(sprintf("Low pulse count: %d|High pulse count: %d", counts[[1L]], counts[[2L]]))
print(sprintf("Part 1: %s", format(counts[[1L]] * counts[[2L]] * (1e3 / cycles)^2, scientific = FALSE)))

# PART 2: Check if there is a single low pulse reaching "rx"
# Problem: `rx` is only connected to the Conjunctor `jq`, which itself is connected to four other modules.
#          In order to receive a low pulse, we need all input modules to raise a high_pulse at the same time
#          Hence we need the cycle length for every single input module and compute the LCM from there
sub_cycles <- vector("integer", length = length(conjuncture_modules[["jq"]]$historic_states))
for (i in seq_along(conjuncture_modules[["jq"]]$historic_states)) {
  # Initialize all the input objects dependent on the % or & sign
  cycles <- 0L
  break_outer <- FALSE
  flip_flop_modules <- sapply(input[which(grepl("%", input))], function(m) FlipFlop$new(module = m))
  names(flip_flop_modules) <- unlist(lapply(flip_flop_modules, \(f) f$name))
  conjuncture_modules <- sapply(input[which(grepl("&", input))], function(m) Conjunctor$new(module = m, modules = input))
  names(conjuncture_modules) <- unlist(lapply(conjuncture_modules, \(f) f$name))
  while (TRUE) {
    # Count the cycles seen
    cycles <- cycles + 1L
    # Now go through the queue from top to bottom
    queue <- broadcaster$send_pulse()
    while (nrow(queue) > 0) {
      queue <- update_queue(queue)
      # print(sprintf("%d|%s", cycles, paste(names(conjuncture_modules[["jq"]]$historic_states), conjuncture_modules[["jq"]]$historic_states, sep = ":", collapse = ", ")))
      # break if the last seen historic state after processing all signals is high
      if (conjuncture_modules[["jq"]]$historic_states[[i]] == "high") {
        break_outer <- TRUE
        break
      }
    }
    if (break_outer) break
  }
  print(sprintf("Found high at position %d after iteration %d", i, cycles))
  sub_cycles[[i]] <- cycles
}

print(sprintf("Part 2: %s",
              format(Reduce(lcm, sub_cycles), scientific = FALSE)))

# [1] "Found high at position 1 after iteration 3911"
# [1] "Found high at position 2 after iteration 3907"
# [1] "Found high at position 3 after iteration 4003"
# [1] "Found high at position 4 after iteration 3889"
# [1] "Part 2: 237878264003759"
