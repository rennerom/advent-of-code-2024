path <- "day-06/input.txt"

guard_map <- readr::read_lines(path) |>
  stringr::str_split("") |>
  lapply(unlist)

guard_map <- do.call(rbind, guard_map)

directions <- list(
  north <- list(sym = "^", dir = c(-1, 0), next_dir = c(0, 1)),
  south <- list(sym = "v", dir = c(1, 0), next_dir = c(0, -1)),
  west <- list(sym = "<", dir = c(0, -1), next_dir = c(-1, 0)),
  east <- list(sym = ">", dir = c(0, 1), next_dir = c(1, 0))
)

find_guard <- function(mat) {

  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {

      if (mat[i, j] %in% c("^", "v", "<", ">")) {
        x_pos <- i
        y_pos <- j

        init_guard_pos <- c(x_pos, y_pos)
        init_guard_sym <- mat[i, j]
        init_next_dir <- 
          purrr::keep(directions, ~ .x$sym == init_guard_sym) |> 
          purrr::list_flatten() |> 
          purrr::pluck("dir")

        init_guard <- list(
          pos = init_guard_pos,
          sym = init_guard_sym,
          next_dir = init_next_dir
        )

        return(init_guard)
      }
    }
  }
}

init_guard <- find_guard(guard_map)

is_in_bounds <- function(n, m, x, y) {
  if (x >= 1 && x <= n && y >= 1 && y <= m) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

draw_guard_path <- function(mat, guard) {

  x_guard <- guard$pos[[1]]
  y_guard <- guard$pos[[2]]
  guard_sym <- guard$sym
  dir <- guard$next_dir

  is_active <- TRUE

  while (is_active) {
    next_x_guard <- x_guard + dir[[1]]
    next_y_guard <- y_guard + dir[[2]]

    # if next position is out of bounds then break
    if (!is_in_bounds(nrow(mat), ncol(mat), next_x_guard, next_y_guard)) {
      break
    }

    # check if next position is not a wall
    if (mat[next_x_guard, next_y_guard] != "#") {
      mat[next_x_guard, next_y_guard] <- guard_sym
      x_guard <- next_x_guard
      y_guard <- next_y_guard
    } else {
      # check if next position is a wall
      if (mat[next_x_guard, next_y_guard] == "#") {

        # get new direction
        dir <- purrr::keep(directions, ~ .x$sym == guard_sym) |> 
          purrr::list_flatten() |> 
          purrr::pluck("next_dir")

        # update guard_sym based on new dir
        guard_sym <- purrr::keep(directions, ~ all(.x$dir == dir)) |> 
          purrr::list_flatten() |> 
          purrr::pluck("sym")

        # recalculate next position with new dir
        next_x_guard <- x_guard + dir[[1]]
        next_y_guard <- y_guard + dir[[2]]

        # update guard symbol and position
        if (mat[next_x_guard, next_y_guard] != guard_sym) {
          mat[next_x_guard, next_y_guard] <- guard_sym
          x_guard <- next_x_guard
          y_guard <- next_y_guard
        } else {
          is_active <- FALSE
        }
      } else {
        is_active <- FALSE
      }
    }

  }
  return(mat)
}

final_map <- draw_guard_path(guard_map, init_guard)
result <- sum(final_map != "." & final_map != "#")

glue::glue("answer: {result}")
