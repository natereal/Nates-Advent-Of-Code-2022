# init 
S <- c(14, 17)
B <- c(10, 16)

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

x <- test_input |>
    stringr::str_split(pattern = "[=,:]") |>
    purrr::map(.f = ~ stringr::str_subset(string = .x, pattern = "[:digit:]")) |>
    purrr::map(as.numeric) |>
    unlist() |>
    matrix(byrow = TRUE, ncol = 4) |>
    `colnames<-`(c("sx", "sy", "bx", "by"))
    
man_dist(x[1,])

grid <- matrix(".", 25, 25)

print_grid <- function(grid) {
    apply(grid, MARGIN = 1, FUN = function(x) cat(paste0(paste0(x, collapse = ""), "\n")))
}

grid[S[2], S[1]] <- "S"
grid[B[2], B[1]] <- "B"

print_grid(grid)

man_dist <- function(x) {
    x1 <- x[1]
    y1 <- x[2]
    x2 <- x[3]
    y2 <- x[4]
    abs(x1 - x2) + abs(y1 - y2)
}

man_dist <- function(x1, y1, x2, y2) {
    abs(x1 - x2) + abs(y1 - y2)
}

mark_points <- function(S, B, print = FALSE) {
    d <- man_dist(S, B)
    initX <- S[2] - d
    maxX  <- S[2] + d
    initY <- S[1] 

    X <- initX
    Y <- initY

    while(X < S[2]) {
        if(grid[X, Y] == ".") grid[X, Y] <- "#" 
        X <- X + 1  
        Y <- (head(Y, 1) - 1):(tail(Y, 1) + 1) 
        if(PRINT) print_grid(grid)
    }
    while(X <= maxX) {
        if(grid[X, Y] == ".") grid[X, Y] <- "#" 
        X <- X + 1
        Y <- (head(Y, 1) + 1):(tail(Y, 1) - 1)
        if(PRINT) print_grid(grid)
    }
}
print_grid(grid)

count_hash <- function(grid, row) {
    sum(grid[row, ] == "#")
}

x
d <- apply(x, 1, man_dist)
x <- cbind(x, d)

down <- x[, "sy"] + x[, "d"]
up   <- x[, "sy"] - x[, "d"]

purrr::map2(.x = down, .y = up, .f = ~ .x:.y)
