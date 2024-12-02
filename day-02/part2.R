path <- "day-02/input.txt"

readr::read_lines(path) |> 
  purrr::map(~stringr::str_split(.x, " ")) |>
  purrr::map(~unlist(.x)) |> 
  purrr::map(~as.numeric(.x)) -> data

check_valid_levels <- function(x) {
  changes <- numeric()
  nth <- length(x)
  for (i in seq_along(x)) {
    ith <- x[[i]]
    if (i == nth) {
      break
    } else {
      change <- x[[i + 1]] - ith
      changes <- c(changes, change)
    }
  }

  mixed_signs <- sum(sign(changes) == -1) > 0 & sum(sign(changes) == 1) > 0

  valid_counts <-
    changes[abs(changes) == 1] |> length() +
    changes[abs(changes) == 2] |> length() +
    changes[abs(changes) == 3] |> length()

  if (valid_counts == nth - 1 && !mixed_signs) {
    return(1)
  } else {
    return(0)
  }
}

reports <- list()

# just brute force it
find_possible_reports <- function(x) {
  for (i in seq_along(x)) {
    report <- check_valid_levels(x[-i])
    if (report == 1) {
      return(x[-i])
    }
  }
}

result <- data |> 
  purrr::map(find_possible_reports) |> 
  purrr::compact() |> 
  length()

glue::glue("answer: {result}")
