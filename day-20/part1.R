box::use(R/aoc [fetch_raw_input_data])

input <-
  fetch_raw_input_data(2024, 20) |>
  stringr::str_split("\n") |>
  purrr::map(~.x |> stringr::str_split("")) |>
  purrr::list_flatten() |>
  purrr::discard(~length(.x) == 0)

mrows <- length(input)
ncols <- max(purrr::map_int(input, length))

map <- 
  input |>  
  as.data.frame() |>
  as.matrix() |> unname() |> t()

turn_right <- function(dir) {
  as.vector(dir %*% matrix(c(0, 1, -1, 0), nrow = 2))
}

turn_left <- function(dir) {
  as.vector(dir %*% matrix(c(0, -1, 1, 0), nrow = 2))
}

find_start_dir <- function(pos, map) {
  dirs <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  for (dir in dirs) {
    if (map[pos[1] + dir[1], pos[2] + dir[2]] == ".") {
      return(dir)
    }
  }
}

s_pos <- which(map == "S", arr.ind = TRUE) |> as.vector()
e_pos <- which(map == "E", arr.ind = TRUE) |> as.vector()
dir <- find_start_dir(s_pos, map)
pos <- s_pos
path <- list(list(pos = s_pos, dir = dir))

while (TRUE) {
  # check if we've reached the end
  if (all(pos == e_pos)) {
    path <- append(path, list(list(pos = pos, dir = dir)))
    break
  }
  # check forward path
  if (map[pos[1] + dir[1], pos[2] + dir[2]] %in% c(".", "E")) {
    pos <- pos + dir
    path <- append(path, list(list(pos = pos, dir = dir)))
  } else {
    # check right path
    r_dir <- turn_right(dir)
    if (map[pos[1] + r_dir[1], pos[2] + r_dir[2]] %in% c(".", "E")) {
      dir <- r_dir
      pos <- pos + dir
      path <- append(path, list(list(pos = pos, dir = dir)))
    } else {
      # check left path
      l_dir <- turn_left(dir)
      if (map[pos[1] + l_dir[1], pos[2] + l_dir[2]] %in% c(".", "E")) {
        dir <- l_dir
        pos <- pos + dir
        path <- append(path, list(list(pos = pos, dir = dir)))
      } else {
        break
      }
    }
  }
}

check_for_cheat <- function(pos, dir, map, path, i) {
  cheat_pos <- list()

  in_bounds <- function(pos, dir, steps) {
    !(pos[1] + steps * dir[1] < 1 || pos[1] + steps * dir[1] > mrows || pos[2] + steps * dir[2] < 1 || pos[2] + steps * dir[2] > ncols)
  }

  visited <- function(pos, path, i) {
    visited_positions <- purrr::map(path[1:i], ~.x$pos)
    any(purrr::map_lgl(visited_positions, ~all(.x == pos)))
  }

  check_two_steps <- function(pos, dir, map, path, i) {
    if (in_bounds(pos, dir, 2)) {
      if (map[pos[1] + 2 * dir[1], pos[2] + 2 * dir[2]] %in% c(".", "E") && map[pos[1] + dir[1], pos[2] + dir[2]] == "#") {
        if (!visited(pos + dir * 2, path, i)) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  directions <- list(dir, turn_right(dir), turn_left(dir))
  cheat_pos <-
    purrr::map(
      directions,
      ~if (check_two_steps(pos, .x, map, path, i)) list(pos = pos, dir = .x) else NULL
    ) |>
    purrr::compact()

  return(cheat_pos)
}

all_cheat_pos <- list()
l <- length(path)

# can't think of a way to make this part any faster
all_cheat_pos <-
  purrr::map(seq_along(path), ~check_for_cheat(path[[.x]]$pos, path[[.x]]$dir, map, path, .x)) |>
  purrr::discard(~length(.x) == 0) |>
  purrr::flatten()

results <- list()
new_path <- list(list(pos = s_pos, dir = dir))

while (TRUE) {
  # check if we've reached the end
  if (all(pos == e_pos)) {
    break
  } 
  # check forward path
  if (map[pos[1] + dir[1], pos[2] + dir[2]] == ".") {
    pos <- pos + dir
    path <- append(path, list(list(pos = pos, dir = dir)))
  } else {
    # check right path
    r_dir <- turn_right(dir)
    if (map[pos[1] + r_dir[1], pos[2] + r_dir[2]] == ".") {
      dir <- r_dir
      pos <- pos + dir
      path <- append(path, list(list(pos = pos, dir = dir)))
    } else {
      # check left path
      l_dir <- turn_left(dir)
      if (map[pos[1] + l_dir[1], pos[2] + l_dir[2]] == ".") {
        dir <- l_dir
        pos <- pos + dir
        path <- append(path, list(list(pos = pos, dir = dir)))
      } else {
        break
      }
    }
  }
}

# why does R have to be so finnicky?
# memoise wasn't great so tried hashing instead
# seemed to be a little bit better
path_index <- hash::hash()
for (i in seq_along(path)) {
  path_index[[paste(path[[i]]$pos, collapse = ",")]] <- i
}

find_index <- function(path_index, target_pos) {
  key <- paste(target_pos, collapse = ",")
  if (hash::has.key(key, path_index)) {
    return(path_index[[key]])
  } else {
    return(integer(0))
  }
}

cheat_times <- list()
l <- length(all_cheat_pos)

for (i in seq_along(all_cheat_pos)) {
  a_cheat <- all_cheat_pos[[i]]
  cheat_pos <- a_cheat$pos
  cheat_dir <- a_cheat$dir

  exit_pos <- cheat_pos + cheat_dir * 2

  cheat_index <- find_index(path_index, cheat_pos)
  exit_index <- find_index(path_index, exit_pos)

  if (length(cheat_index) > 0 && length(exit_index) > 0) {
    pre_cheat_length <- cheat_index
    post_cheat_length <- length(path) - exit_index + 1

    new_time <- pre_cheat_length + post_cheat_length

    cheat_times <- append(cheat_times, list(new_time))
  } else {
    # debugging
    cat("Cheat position or exit position not found in path\n")
    cat("Cheat position:", cheat_pos, "\n")
    cat("Exit position:", exit_pos, "\n")
  }
}

original_time <- length(path)

result <-
  cheat_times |>
  purrr::map_dfr(~data.frame(time = .x)) |>
  dplyr::group_by(time) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(time_saved = original_time - time) |>
  dplyr::filter(time_saved >= 100) |>
  dplyr::pull(n) |>
  sum()

glue::glue("answer: {result}")
