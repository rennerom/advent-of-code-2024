box::use(R/aoc [fetch_raw_input_data, print_answer])

input <-
  fetch_raw_input_data(2024, 25) |>
  stringr::str_remove("\\n$") |>
  stringr::str_split("\n\n") |>
  unlist() |>
  purrr::map(~ stringr::str_split(.x, "\n") |> unlist()) |>
  purrr::map(~ as.list(.x))

keys <-
  input |>
  purrr::map(~ if (stringr::str_detect(.x[[1]], "^#+$")) {
    purrr::map(.x, ~ stringr::str_split(.x, "")[[1]]) |>
      purrr::transpose() |>
      purrr::map(~ sum(.x == "#") - 1)
  }) |> 
  purrr::discard(~ is.null(.x))

locks <-
  input |>
  purrr::map(~ if (!(stringr::str_detect(.x[[1]], "^#+$"))) {
    purrr::map(.x, ~ stringr::str_split(.x, "")[[1]]) |>
      purrr::transpose() |>
      purrr::map(~ sum(.x == "#") - 1)
  }) |> 
  purrr::discard(~ is.null(.x))

tidyr::expand_grid(locks, keys) |>
  dplyr::mutate(
    fit = purrr::pmap_lgl(
      list(locks, keys),
      ~all(purrr::map2_lgl(.x, .y, ~ .x + .y <= 5))
    )
  ) |>
  dplyr::pull(fit) |>
  sum() |>
  print_answer()
