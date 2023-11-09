#' Day 18: Boiling Boulders
#'
#' [Boiling Boulders](https://adventofcode.com/2022/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' You and the elephants finally reach fresh air. You\'ve emerged near the
#' base of a large volcano that seems to be actively erupting! Fortunately,
#' the lava seems to be flowing away from you and toward the ocean.
#'
#' Bits of lava are still being ejected toward you, so you\'re sheltering
#' in the cavern exit a little longer. Outside the cave, you can see the
#' lava landing in a pond and hear it loudly hissing as it solidifies.
#'
#' Depending on the specific compounds in the lava and speed at which it
#' cools, it might be forming
#' [obsidian](https://en.wikipedia.org/wiki/Obsidian)! The cooling rate
#' should be based on the surface area of the lava droplets, so you take a
#' quick scan of a droplet as it flies past you (your puzzle input).
#'
#' Because of how quickly the lava is moving, the scan isn\'t very good;
#' its resolution is quite low and, as a result, it approximates the shape
#' of the lava droplet with *1x1x1
#' [cubes]{title="Unfortunately, you forgot your flint and steel in another dimension."}
#' on a 3D grid*, each given as its `x,y,z` position.
#'
#' To approximate the surface area, count the number of sides of each cube
#' that are not immediately connected to another cube. So, if your scan
#' were only two adjacent cubes like `1,1,1` and `2,1,1`, each cube would
#' have a single side covered and five sides exposed, a total surface area
#' of `10` sides.
#'
#' Here\'s a larger example:
#'
#'     2,2,2
#'     1,2,2
#'     3,2,2
#'     2,1,2
#'     2,3,2
#'     2,2,1
#'     2,2,3
#'     2,2,4
#'     2,2,6
#'     1,2,5
#'     3,2,5
#'     2,1,5
#'     2,3,5
#'
#' In the above example, after counting up all the sides that aren\'t
#' connected to another cube, the total surface area is `64`.
#'
#' *What is the surface area of your scanned lava droplet?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns .... For Part Two,
#'   `f18b(x)` returns ....
#' @export
#' @examples
#' f18a(example_data_18())
#' f18b()
f18a <- function(x) {

}


#' @rdname day18
#' @export
f18b <- function(x) {

}


parse_input <- function(x) {
    x |>
        stringr::str_split(",") |>
        purrr::map(as.numeric) |>
        purrr::as_vector() |>
        matrix(byrow = TRUE, ncol = 3)

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = c(
        "2,2,2",
        "1,2,2",
        "3,2,2",
        "2,1,2",
        "2,3,2",
        "2,2,1",
        "2,2,3",
        "2,2,4",
        "2,2,6",
        "1,2,5",
        "3,2,5",
        "2,1,5",
        "2,3,5"
    )
  )
  l[[example]]
}

input <- example_data_18() |>
    parse_input()



input <- readLines("inst/input18.txt") |>
    parse_input()
#' point(2, 2, 2)
#' It is touching any points which are 1 unit away in
#' one direction only, e.g. (3, 2, 2) or (2, 3, 2) etc.

get_faces <- function(input) {

    input <- as.matrix(input)
    i <- input[, 1]
    j <- input[, 2]
    k <- input[, 3]

    neighbour_points <- matrix(
        c(i+1, i-1,   i,   i,     i, i,
          j,     j, j+1, j-1,     j, j,
          k,     k,   k,   k,   k+1, k-1),
        byrow = FALSE, ncol = 3
    )

    neighbour_points <- tibble::as_tibble(neighbour_points) |>
        rename(X = V1, Y = V2, Z = V3)
    neighbour_points
}

neighbour_points <- get_faces(input)
input <- as_tibble(input)
colnames(input) <- c("X", "Y", "Z")
intersection <- intersect(neighbour_points, input)
# setdiff(neighbour_points, intersection)
# setdiff(neighbour_points, x)
# setdiff(x, neighbour_points)
# intersection
# nrow(setdiff(neighbour_points, x))

covered_points <- intersection |>
    mutate(slot = paste0(X, ".", Y, ".", Z)) |>
    pull(slot)

neighbour_points |>
    group_by(X, Y, Z) |>
    count() |>
    mutate(slot = paste0(X, ".", Y, ".", Z)) |>
    filter(!slot %in% covered_points) |>
    ungroup() |>
    summarise(total_faces = sum(n))
#'
#' nrow(neighbour_points) - nrow(inter)
#'
#' neighbour_points[1:13, ]
#' #' 2, 2, 2
#' p <- matrix(c(
#'     3, 2, 2,
#'     1, 2, 2,
#'     2, 3, 2,
#'     2, 1, 2,
#'     2, 2, 3,
#'     2, 2, 1
#' ), byrow = TRUE, ncol = 3)
#' p <- as_tibble(p)
#'
#'
#' intersect(p, x)
#' setdiff(p, x)
#'
#' readLines("inst/input18.txt") |>
#'     parse_input() |>
#'     nrow()

# If an empty point has all 6 of its faces present in x,
# then those 6 faces that were counted as neighbours
# from the original points shouldn't be counted.

input |>
    summarise_all(list("min" = min, "max" = max))

all_points <- expand.grid(1:20, 1:20, 1:20)
colnames(all_points) <- c("X", "Y", "Z")
p <- as_tibble(all_points)
empty_points <- setdiff(p, input)

counter <- 0
empty_ids <- c()
for(i in 1:nrow(empty_points)) {
    x <- as.numeric(empty_points[i, 1])
    y <- as.numeric(empty_points[i, 2])
    z <- as.numeric(empty_points[i, 3])

    touching <- tibble(
        X = c(x-1, x+1, x, x, x, x),
        Y = c(y, y, y-1, y+1, y, y),
        Z = c(z, z, z, z, z-1, z+1)
    )

    empty_intersect <- intersect(touching, input)
    if(nrow(empty_intersect) == 6) {
        counter <- counter + 1
        empty_ids <- c(empty_ids, i)
    }
}

out <- neighbour_points |>
    group_by(X, Y, Z) |>
    count() |>
    mutate(slot = paste0(X, ".", Y, ".", Z)) |>
    filter(!slot %in% covered_points) |>
    ungroup()

empty_slot_ids <- empty_points[empty_ids, ] |>
    mutate(slot = paste0(X, ".", Y, ".", Z)) |>
    pull(slot)

out |>
    filter(!slot %in% empty_slot_ids) |>
    summarise(sum(n))

# These are the faces that are counted, but it is including some
# faces that are completely within lava.
# To be a point completely within lava, you need to have all your faces be
# lava or a face which is empty but is also covered by lava at every other
# face.
out |>
    filter(!slot %in% empty_slot_ids)

# -1 to 20 would surround the points
#' -1 -1 -1
range <- -1:20
n <- length(range)

c(rep(-1, n), range)

# Part 2
# ======
d <- dist(input[1:5, ], method = "manhattan")
d

make_id <- function(df) {
    df |>
        mutate(id = paste0(X, ".", Y, ".", Z))
}

get_adjacent <- function(df, row) {
    x <- df[[row, "X"]]
    y <- df[[row, "Y"]]
    z <- df[[row, "Z"]]
    df |>
      filter(abs(X - x) + abs(Y - y) + abs(Z - z) == 1)
}

grid <- expand_grid(X = -1:20, Y = -1:20, Z = -1:20)

grid <- grid |>
    make_id()

magma_coords <- input |>
    rename(X = X, Y = Y, Z = Z) |>
    make_id()

list_col <- grid |>
    mutate(
        magma = if_else(id %in% magma_coords$id, TRUE, FALSE),
        visited = FALSE) |>
    nest(coord = c(X, Y, Z)) |>
    select(coord)

list_col <- grid |>
    nest(coord = c(X, Y, Z)) |>
    select(coord) |>
    mutate(coord = map(coord, unlist))

grid <- grid |>
    mutate(
        magma = if_else(id %in% magma_coords$id, TRUE, FALSE),
        visited = FALSE,
        "coord" = list_col) |>
    rowid_to_column()

q <- c(1)
i <- 1
while(length(q) > 0) {
    current <- q[1]
    cat(i, ":", current, "\n")
    # find adjacent:
    grid[current, ]$visited <- TRUE

    next_pts <- get_adjacent(grid, current) |>
        filter(visited == FALSE, magma == FALSE) |>
        pull(rowid)
    # Add to queue
    q <- unique(c(q, next_pts))
    # Pop first entry
    q <- tail(q, -1)
    i <- i + 1
}

input |>
    get_faces() |>
    group_by(X, Y, Z) |>
    count() |>
    make_id() |>
    left_join(select(grid, id, visited, magma)) |>
    filter(magma == FALSE) |>
    ungroup() |>
    summarise(sum(n))

input |>
    get_faces() |>
    group_by(X, Y, Z) |>
    count() |>
    make_id() |>
    left_join(select(grid, id, visited, magma)) |>
    filter(visited == TRUE) |>
    ungroup() |>
    summarise(sum(n))


