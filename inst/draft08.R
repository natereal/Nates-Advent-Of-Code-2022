test <- c(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
)

parse_input <- function(input) {
    nrow <- length(input)
    forest <- input |>
        stringr::str_split(pattern = "") |>
        unlist() |>
        as.numeric() |>
        matrix(nrow = nrow, byrow = TRUE) 

        return(forest)   
}

forest <- parse_input(test)


count_visible <- function(forest) {
    N <- nrow(forest)
    num_edge <- (N * 4) - 4
    num_visible <- 0
    for(i in 2:(N-1)) {
        for(j in 2:(N-1)) {
            tree <- forest[i,j]
        
            left  <- j-1
            right <- j+1
            up   <- i-1
            down <- i+1
            
            visible_left  <- all(forest[i,right:N] < tree)
            visible_right <- all(forest[i,1:left] < tree)
            visible_up    <- all(forest[1:up, j] < tree)
            visible_down  <- all(forest[down:N, j] < tree)

            is_visible <- any(visible_down, visible_left, visible_right, visible_up)
            if(is_visible) num_visible <- num_visible + 1
        }
    }
    return(num_edge + num_visible)
}

solve <- function(input) {
    input |>
        parse_input() |>
        count_visible()
}

test |>
 parse_input() |>
 count_visible()

solve(test)

readLines("inst/input08.txt") |>
    solve()

x <- readLines("inst/input08.txt")
length(x)
nchar(x[1])

count_visible <- function(forest, score = TRUE) {
    N <- nrow(forest)
    num_edge <- (N * 4) - 4
    num_visible <- 0
    running_total <- 0
    for(i in 2:(N-1)) {
        for(j in 2:(N-1)) {
            tree <- forest[i,j]
        
            left  <- j-1
            right <- j+1
            up   <- i-1
            down <- i+1
            
            visible_left  <- all(forest[i,right:N] < tree)
            visible_right <- all(forest[i,1:left] < tree)
            visible_up    <- all(forest[1:up, j] < tree)
            visible_down  <- all(forest[down:N, j] < tree)

            score_r <- find_score(forest[i,right:N] < tree)
            score_l <- find_score(rev(forest[i,1:left] < tree))
            score_u <- find_score(rev(forest[1:up, j] < tree))
            score_d <- find_score(forest[down:N, j] < tree)
            total <- score_d * score_l * score_r  * score_u
            
            if(total > running_total) running_total <- total

            is_visible <- any(visible_down, visible_left, visible_right, visible_up)
            if(is_visible) num_visible <- num_visible + 1
        }
    }
    output <- num_edge  + num_visible
    if(score == TRUE) output <- running_total

    return(output)
}

y <- forest[i,4:5] < tree
cumsum(y)
y
min(which(y == FALSE))

find_first_false <- function(vector) {
    id <- min(which(vector == FALSE), na.rm = TRUE)
    if(all(vector == TRUE)) id <- -1
    return(id)
}

find_score <- function(vector) {
    id <- find_first_false(vector)
    sum <- cumsum(vector)
    if(length(sum) > 1) {
        if(id != -1) {
            score <- sum[id] + 1
        } else{
            score <- tail(sum, 1)
        }
    } else{
        score <- 1
    }
    return(score)
}

which(y == FALSE)
cumsum(y)
id <- find_first_false(y)
cumsum(y)[id] + 1
id
cumsum(y)[-1]

test |>
 parse_input() |>
 count_visible()

y
find_score(y)
forest[2, 1:1]

readLines("inst/input08.txt") |>
 parse_input() |>
 count_visible()
