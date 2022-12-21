library(tidyverse)

test_input <- c(
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
)
process_input <- function(input) {
    x <- input |>
        stringr::str_split(pattern = "[=,:]") |>
        purrr::map(.f = ~ stringr::str_subset(string = .x, pattern = "[:digit:]")) |>
        purrr::map(as.numeric) |>
        unlist() |>
        matrix(byrow = TRUE, ncol = 4) |>
        `colnames<-`(c("sx", "sy", "bx", "by"))

    d <- apply(x, 1, man_dist)
    x <- cbind(x, d)

    return(x)
}   

man_dist <- function(x) {
    x1 <- x[1]
    y1 <- x[2]
    x2 <- x[3]
    y2 <- x[4]
    abs(x1 - x2) + abs(y1 - y2)
}


find_marked <- function(x, target = 10, sort = FALSE, remove = TRUE) {
    marked <- c()
    d <- apply(x, 1, man_dist)
    x <- cbind(x, d)
    for(i in seq_len(nrow(x))) {
        coords <- x[i, ]
        #' target row is row 10
        #' given by sy 
        #' check row 10 is within d of s
        is_within_target <- abs(coords["sy"] - target) < coords["d"]
        # is_within_target <- TRUE
        #' so at least, [sx, target] will be marked (no beacon)
        if(is_within_target) {
            to_be_marked <- coords["sx"]
            #' based on the distance, we +1/-1 to this to make a range 
            #' get distance of row to s;
            r <- coords["d"] - abs(coords["sy"] - target)
            marked <- c(marked, (to_be_marked - r):(to_be_marked + r))
            marked <- unique(marked)
            if(remove) {
                # remove beacons and sensors:
                beacons <- unique(x[, c("bx", "by")])
                beacons_on_target_row <- beacons[beacons[, "by"] == target, ]
                # check if x coord of any of these has been marked
                beacons_on_target_row
                ind <- marked %in% beacons_on_target_row["bx"]
                
                marked <- marked[!ind]

                # sensors:
                sensors <- unique(x[, c("sx", "sy")])
                sensors_on_target_row <- sensors[sensors[, "sy"] == target, ]
                # check if x coord of any of these has been marked
                sensors_on_target_row
                marked <- marked[!marked %in% sensors_on_target_row["sx"]]
            }
        }
    }

    if(sort) marked <- sort.int(marked)
    
    return(marked)
}

solve <- function(n = 2000000) {
    readLines("inst/input15.txt") |>
        process_input() |>
        find_marked(target = n) |>
        length()
}

# solve()



find_unmarked <- function(x, target, max = 4000000) {
    marked <- 0:max
    for(i in seq_len(nrow(x))) {
        coords <- x[i, ]
        #' target row is row 10
        #' given by sy 
        #' check row 10 is within d of s
        is_within_target <- abs(coords["sy"] - target) < coords["d"]
        #' so at least, [sx, target] will be marked (no beacon)
        if(is_within_target) {
            to_be_marked <- coords["sx"]
            #' based on the distance, we +1/-1 to this to make a range 
            #' get distance of row to s;
            r <- coords["d"] - abs(coords["sy"] - target)
            # marked <- c(marked, (to_be_marked - r):(to_be_marked + r))
            # marked <- unique(marked)

            # marked <- c(marked[marked < to_be_marked - r], marked[marked > to_be_marked + r])
            marked <- marked[(marked < to_be_marked - r) | (marked > to_be_marked + r)]

            if(length(marked) == 0) return(NULL)
        }
    }
    
    return(marked)
}

# microbenchmark::microbenchmark(
#     combo = { c(marked[marked < to_be_marked - r], marked[marked > to_be_marked + r]) },
#     just_indexes = { marked[(marked < to_be_marked - r) | (marked > to_be_marked + r)]}
# )

# microbenchmark::microbenchmark(
#     x = {
#         #to_be_marked <- coords["sx"]
#             #' based on the distance, we +1/-1 to this to make a range 
#             #' get distance of row to s;
#         r <- abs(coords["sy"] - target) - coords["d"]
#         tempmarked <- c(marked, (to_be_marked - r):(to_be_marked + r))
#         tempmarked <- unique(tempmarked)
#     }
# )
# cmarked <- 0:4000000
# microbenchmark::microbenchmark(
#     x = {
#         #to_be_marked <- coords["sx"]
#             #' based on the distance, we +1/-1 to this to make a range 
#             #' get distance of row to s;
#         r <- abs(coords["sy"] - target) - coords["d"]
#         tempmarked <- cmarked[cmarked < to_be_marked - r]
#         tempmarked <- tempmarked[tempmarked > to_be_marked + r]
#         marked <- unique(marked)
#     }
# )

# microbenchmark::microbenchmark(find = {find_unmarked(x, 2000000)})



x <- test_input |>
    process_input()

# row_coord <- 0
# col_coord <- find_unmarked(x, row_coord, max = 20)
# while(is.null(col_coord)) {
#     row_coord <- row_coord + 1
#     col_coord <- find_unmarked(x, row_coord, max = 20)
# }

# 4000000 * col_coord + row_coord

# x <- readLines("inst/input15.txt") |>
#     process_input()

# row_coord <- 0
# col_coord <- find_unmarked(x, row_coord)
# while(is.null(col_coord)) {
#     row_coord <- row_coord + 1
#     col_coord <- find_unmarked(x, row_coord)
#     print(row_coord)
# }
# 4000000 * col_coord + row_coord

# search <- sample(1000001:4000000, 3000000)
# ii <- 1
# row_coord <- search[ii]
# col_coord <- find_unmarked(x, row_coord)
# while(is.null(col_coord)) {
#     ii <- ii + 1
#     row_coord <- search[ii]
#     col_coord <- find_unmarked(x, row_coord)
#     print(row_coord)
# }
# 4000000 * col_coord + row_coord

# ==================================================
# STEP 1
# =======
x <- test_input |>
    process_input()

x <- readLines("inst/input15.txt") |>
     process_input()
# ==================================================

find_boundary <- function(coord, MAX = 4000000) {
    # points <- tibble(sx, sy)
    delta <- 0
    x_base <- coord["sx"]
    top_y <- coord["sy"] - coord["d"] - 1
    bottom_y <- coord["sy"] + coord["d"] + 1
    points <- tibble("sx" = x_base, "sy" = top_y)
    #' Loop for top y to bottom y,
    #' start at i = 0 and x is equal to base_x +- i
    y <- top_y
    if(y < 0) y <- 0
    max_y_loop <- min(bottom_y, MAX)
    while(y < coord["sy"]) {
        y <- y + 1
        delta <- delta + 1
        x_left <- x_base - delta
        x_right <- x_base + delta
        # points <- rbind(points, c(x_left, y))
        # points <- rbind(points, c(x_right, y))
        points <- bind_rows(points, c(x_left, y), c(x_right, y))

        # y = 2757325
        # ymax = 2935757 
        # initial delta 1
        # N = coord["sy"] - y
        # range <- 1:N
        # x <- c(x_base - range, x_base + range)
        # points 

    }
    while(y < max_y_loop) {
        y <- y + 1
        delta <- delta - 1
        x_left <- x_base - delta
        x_right <- x_base + delta
        # points <- rbind(points, c(x_left, y))
        # points <- rbind(points, c(x_right, y))
        points <- bind_rows(points, c(x_left, y), c(x_right, y))
    }
    # points <- distinct(points)
    # points <- tibble::as_tibble(points)
    points %>%
        filter(if_all(.fns = ~ .x >= 0 & .x <= MAX)) %>%
        distinct()
}

# ==================================================
# STEP 2
boundaries <- apply(x, 1, FUN = find_boundary, MAX = 4000000)

microbenchmark::microbenchmark(time = { find_boundary(x[1,], MAX = 4000000)}, times = 1L)
# ==================================================

d1 <- boundaries[[1]]
d2 <- boundaries[[2]]

check_overlap <- function(d1, d2) {
    df <- intersect(d1, d2)

    if(nrow(df) == 0) {
        df <- dplyr::bind_rows(d1, d2)
    }

    return(df)
}

out <- purrr::accumulate(boundaries, .f = check_overlap)


purrr::reduce(boundaries, .f = check_overlap)


boundaries[[1]]
boundaries[[2]]
em <- intersect(boundaries[[1]], boundaries[[2]])
bind_rows(boundaries[[1]], boundaries[[2]])
nrow(em)
out[[1]]
out[[2]]
out
boundaries[[9]]
boundaries[[10]]
boundaries[[11]]
boundaries[[12]]

loop_bound <- function(boundaries) {
    i <- 1
    j <- 2
    num_entries <- length(boundaries)
    found_overlap <- FALSE
    inter <- intersect(boundaries[[i]], boundaries[[j]])
    if(nrow(inter) > 0) found_overlap <- TRUE
    j <- j + 1
    while(!found_overlap) {
        while(j <= num_entries) {
            inter <- intersect(boundaries[[i]], boundaries[[j]])
            if(nrow(inter) > 0) {
                found_overlap <- TRUE
                return(c(i, j))
            }
            j <- j + 1
        }
        i <- i + 1
        j <- i + 1
    }
    return(NULL)
}

# ==================================================
# STEP 3
all_points <- purrr::reduce(boundaries, bind_rows)
check_rows <- unique(all_points$sy)
# ==================================================

search <- function(input_coords = x, vector_of_rows = 0) {
    for(i in vector_of_rows) {
        out <- find_unmarked(x = input_coords, target = i, max = 20)
        if(!is.null(out)) return(out)
    }
    return(NULL)
}

# ==================================================
# STEP 4
search(x, check_rows)
# ==================================================


microbenchmark::microbenchmark(time = {
    find_boundary(x[1, ], MAX = 20)
}, times = 1L)


points <- tibble("sx" = x_base, "sy" = top_y)  
microbenchmark::microbenchmark(
    time = {
        i = 1
        while(i < 10000) {
                y <- y + 1
                delta <- delta + 1
                x_left <- x_base - delta
                x_right <- x_base + delta
                # points <- rbind(points, c(x_left, y))
                # points <- rbind(points, c(x_right, y))
                points <- bind_rows(points, c(x_left, y), c(x_right, y))
                i <- i + 1
            }
    }, times = 1L
)
