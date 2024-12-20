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
filtered_patterns <- 
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

request_to_check <-
  purrr::map(
    requests, \(request) {
      list(
        request = request,
        result = do_patterns_fill_request(filtered_patterns, request)
      )
    }
  ) |> 
  purrr::keep(~ .x$result) |>
  purrr::map(~ .x$request)

count_combinations <- function(string_to_check, combinations) {
  # stop at the end of the string
  if (string_to_check == "") {
    return(1)
  }

  total_combinations <- 0
  for (i in 1:nchar(string_to_check)) {
    prefix <- substr(string_to_check, 1, i)
    suffix <- substr(string_to_check, i + 1, nchar(string_to_check))
    if (prefix %in% combinations) {
      total_combinations <- total_combinations + mem_count(suffix, combinations)
    }
  }

  return(total_combinations)

}

# we can speed this recursion up a bit
mem_count <- memoise::memoise(count_combinations)

result <-
  purrr::map(
    request_to_check, \(request) {
      mem_count(request, patterns)
    }
  ) |> 
  purrr::reduce(`+`)

glue::glue("answer: {result}")
