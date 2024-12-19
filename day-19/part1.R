box::use(R/aoc [fetch_raw_input_data])

input <-
  fetch_raw_input_data(2024, 19) |>
  readr::read_lines()

patterns <-
  input |>
  purrr::pluck(1) |>
  stringr::str_remove_all(" ") |>
  stringr::str_split(",") |>
  purrr::flatten_chr() |>
  purrr::map(~ .x)

requests <-
  input[3:length(input)] |>
  purrr::map(~ .x)

can_construct <- function(pattern, patterns) {
  for (i in 1:(nchar(pattern) - 1)) {
    prefix <- substr(pattern, 1, i)
    suffix <- substr(pattern, i + 1, nchar(pattern))
    if (prefix %in% patterns && suffix %in% patterns) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# fitler out patterns that can't be constructed
patterns <- 
  purrr::map(
    patterns, ~{
      result <- can_construct(.x, patterns)
      list(pattern = .x, keep = result)
    }
  ) |>
  purrr::keep(~ .x$keep) |>
  purrr::map(~ .x$pattern)


do_patterns_fill_request <- function(patterns, request) {

  fill_helper <- memoise::memoise(function(remaining_request) {
    # if we get to the end of the string then we're done
    if (remaining_request == "") {
      return(TRUE)
    }

    results <- purrr::map_lgl(patterns, \(pattern) {
      if (stringr::str_starts(remaining_request, pattern)) {
        # remove the bit that matched and start again
        new_request <- stringr::str_remove(remaining_request, pattern)
        return(fill_helper(new_request))
      }
      return(FALSE)
    })
    # if any of the results are true then we're good too
    return(any(results))
  })
  return(fill_helper(request))
}

result <-
  purrr::map_lgl(requests, \(request) {
    do_patterns_fill_request(patterns, request)
  }) |>
  sum()

glue::glue("answer: {result}")
