test <- c(
    "        ...#    ",
    "        .#..    ",
    "        #...    ",
    "        ....    ",
    "...#.......#    ",
    "........#...    ",
    "..#....#....    ",
    "..........#.    ",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5"
)
ch <- function(string, n) {
    stringr::str_sub(string, n, n)
}

parse_input <- function(input) {
    LENGTH <- input |>
        stringr::str_subset("[:digit:]", negate = TRUE) |>
        nchar() |>
        max()

    input |>
        stringr::str_subset("[:digit:]", negate = TRUE) |>
        purrr::map(.f = ~ {
                     if(.x == "") return(.x)
                     missing_chars <- LENGTH - nchar(.x)
                     if(missing_chars > 0) return(paste0(.x, paste0(rep(" ", missing_chars), collapse = "")) )
                     return(.x)
                 }) |>
        stringr::str_split("") |>
        unlist() |>
        matrix(byrow = TRUE, ncol = nchar(input[1]))
}

parse_directions <- function(input){
    moves <- input |>
        stringr::str_subset("[:digit:]") |>
        stringr::str_split("R|L") |>
        unlist() |>
        as.numeric()

    turns <- input |>
        stringr::str_subset("[:digit:]") |>
        stringr::str_split("[:digit:]") |>
        unlist() |>
        stringr::str_subset("R|L") |>
        append("X") # This is so the lengths of the instructions are consistent

    return(list(
        "moves" = moves, "turns" = turns
    ))
}

for(i in 1:L) {
    ch(test[1], i)
}

test |>
    stringr::str_subset("[:digit:]", negate = TRUE) |>
    stringr::str_split("") |>
    unlist() |>
    matrix(byrow = TRUE, ncol = 16)

grid <- parse_input(test)

# x <- readLines("inst/input22.txt")
input <- readLines("inst/input22.txt")
grid <- parse_input(readLines("inst/input22.txt"))

LENGTH <- nchar(grid[1])


# grid <- parse_input(test)

instructions <- parse_directions(test)
instructions <- parse_directions(input)


rmod <- function(n, mod) {
    # Takes modulo indexes starting from 1
    # So mod N returns
    # 1,2,3,...,N as opposed to 0,1,2,...,N-1
    return(((n-1) %% mod) + 1)
}

dir_symbols <- c(">" = 0, "v" = 1, "<" = 2, "^" = 3)
purrr::map(instructions, .f = ~ head(.x))
# Position is (i, j)
# Initialise

solve <- function(input, dim = 50, print = FALSE) {

    grid <- parse_input(input)
    instructions <- parse_directions(input)

    LENGTH <- nchar(grid[1])

    i <- 1
    j <- min(which(grid[1, ] == "."))
    direction <- 0

    max_k <- length(instructions$moves)
    k <- 1

    while(k <= max_k) {
        #' Direction
        #' mod 4
        #' 0 : Right : +x : >
        #' 1 : Down  : +y : v
        #' 2 : Left  : -x : <
        #' 3 : Up    : -y : ^

        rotation <- instructions$turns[k]
        # Number of moves
        moves <- instructions$moves[k]

        # R uses column major
        nonexistant <- which(grid == " ")
        convert_1dim <- function(x, y) {
            x + (y-1)*nrow(grid)
        }
        TELEPORT <- FALSE
        if(direction == 0){
            singular <- convert_1dim(i, j+moves)
            if(j + moves > 3*dim | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- dim - rmod(j, dim)

            }
        } else if(direction == 1) {
            singular <- convert_1dim(i+moves, j)
            if(i + moves > 200 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- dim - rmod(i, dim)

            }
        } else if(direction == 2){
            singular <- convert_1dim(i, j-moves)
            if(j - moves < 1 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- rmod(j, dim) - 1


            }
        } else if(direction == 3) {
            singular <- convert_1dim(i-moves, j)
            if(i - moves < 1 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- rmod(i, dim) - 1


            }
        }

        if(TELEPORT) {
            # allowed_move <- allowed_move - 1

            BLOCKED <- FALSE
            # check if path ahead has any '#'
            if(direction == 0) {
                if(any(grid[i, j:(j+allowed_move)] == "#")) BLOCKED <- TRUE
            } else if(direction == 1) {
                if(any(grid[i:(i+allowed_move), j] == "#")) BLOCKED <- TRUE
            } else if(direction == 2) {
                if(any(grid[i, j:(j-allowed_move)] == "#")) BLOCKED <- TRUE
            } else if(direction == 3) {
                if(any(grid[i:(i-allowed_move), j] == "#")) BLOCKED <- TRUE
            }

            if(BLOCKED == FALSE) {
                remaining_move <- moves - allowed_move
                moves <- allowed_move
            } else {
                TELEPORT <- FALSE
            }

            # determine amount we can move without teleport
        }


        if(direction %in% c(0,2)) {
            true_path_coords <- which(grid[i,] != " ")
            path <- grid[i, ][true_path_coords]
            true_values <- grid[i, ]
            coord <- j
        }else {
            true_path_coords <- which(grid[,j] != " ")
            path <- grid[, j][true_path_coords]
            true_values <- grid[ ,j]
            coord <- i
        }

        mod <-length(path)

        if(direction %in% c(0,1)) {
            operation <- `+`
        }else {
            operation <- `-`
        }

        # j <- 51
        # move 10
        # abs - 51:61
        # relative - 1:10

        # offset <- min(which(grid[i, ] != " ")) - 1
        offset <- min(which(true_values != " ")) - 1
        coord_rel <- coord - offset
        rel_coords <- rmod(coord_rel:operation(coord_rel, moves), mod = mod)
        abs_coords <- true_path_coords[rel_coords]
        # grid[i, abs_coords]
        wall_check <- which(path[rel_coords] == "#")
        if(length(wall_check) > 0) {
            wall_rel_to_move <- min(wall_check)
            if(length(wall_rel_to_move) == 1) {
                if(wall_rel_to_move < max(abs_coords)) {
                    abs_coords <- abs_coords[1:(wall_rel_to_move-1)]
                }
            }
        }


        true_values[abs_coords] <- names(dir_symbols)[dir_symbols == direction]

        if(direction %in% c(0,2)) {
            grid[i, ] <- true_values
            # i <- i
            j <- tail(abs_coords, 1)
        }else {
            grid[, j] <- true_values
            i <- tail(abs_coords, 1)
            # j <- j
        }

        SKIP_ROTATION <- FALSE
        if(TELEPORT) {
            # set i and j manually
            # rotate manually
            # append new instructions
            # skip rotation step

            # 1. need to know which face we are at
            face <- NULL
            if(i == 1 & direction == 3) {
                if(j < 101) {
                    face <- "one_blue"
                } else {
                    face <- "two_purple"
                }
            } else if(i == 50 & direction == 1) {
                face <- "two_orange"
            } else if(j == 150 & direction == 0) {
                face <- "two_yellow"
            } else if(j == 100 & direction == 0) {
                if(i < 101) {
                    face <- "three_orange"
                } else {
                    face <- "five_yellow"
                }
            } else if(i == 150 & direction == 1) {
                face <- "five_red"
            } else if(j == 50 & direction == 0) {
                face <- "six_red"
            } else if(i == 200 & direction == 1) {
                face <- "six_purple"
            } else if(j == 1 & direction == 2) {
                if(i < 151) {
                    face <- "four_pink"
                } else {
                    face <- "six_blue"
                }
            } else if(j == 51 & direction == 2) {
                if(i < 51) {
                    face <- "one_pink"
                } else {
                    face <- "three_green"
                }
            } else if(i == 101 & direction == 3) {
                face <- "four_green"
            }

            local_i <- rmod(i, dim)
            local_j <- rmod(j, dim)
            if(print) {
                cat(face, ":face\n")
                cat(i, ":i\n")
                cat(j, ":j\n")
                cat(direction, ":dir\n")
                cat(moves, ":moves\n")
                cat(k, ":k\n")
                print_grid(grid)
            }
            # new_coords_old <- case_when(
            #     face == "one_blue"     ~ c(local_j + 150, 1),
            #     face == "one_pink"     ~ c((150:101)[local_i], 1),
            #     face == "two_purple"   ~ c(200, local_j),
            #     face == "two_orange"   ~ c(local_j + 50, 100),
            #     face == "two_yellow"   ~ c((150:101)[local_i], 100),
            #     face == "three_orange" ~ c(50, local_i + 100),
            #     face == "three_green"  ~ c(101, local_i),
            #     face == "four_pink"    ~ c((50:1)[local_i], 51),
            #     face == "four_green"   ~ c(local_j + 50, 51),
            #     face == "five_yellow"  ~ c((50:1)[local_i], 150),
            #     face == "five_red"     ~ c(local_j + 150, 50),
            #     face == "six_red"      ~ c(150, local_i + 50),
            #     face == "six_blue"     ~ c(1, local_j + 50),
            #     face == "six_purple"   ~ c(1, local_j + 100)
            # )
            new_coords <- case_when(
                face == "one_blue"     ~ c( (151:200)[local_j],  1 ),
                face == "one_pink"     ~ c( (150:101)[local_i],  1 ),
                face == "two_purple"   ~ c( 200   , (1:50)[local_j]),
                face == "two_orange"   ~ c( (51:100)[local_j], 100),
                face == "two_yellow"   ~ c( (150:101)[local_i], 100),
                face == "three_orange" ~ c( 50, (101:150)[local_i]),
                face == "three_green"  ~ c( 101, local_i ),
                face == "four_pink"    ~ c( (50:1)[local_i], 51),
                face == "four_green"   ~ c( (51:100)[local_j], 51),
                face == "five_yellow"  ~ c( (50:1)[local_i], 150),
                face == "five_red"     ~ c( (151:200)[local_j], 50),
                face == "six_red"      ~ c( 150, (51:100)[local_i]),
                face == "six_blue"     ~ c( 1, (51:100)[local_i]),
                face == "six_purple"   ~ c( 1, (101:150)[local_j]),
            )
            # if(any(new_coords_old != new_coords)) {
            #     print(face)
            #     print(i)
            #     print(j)
            #     print(new_coords_old)
            #     print(new_coords)
            # }

            PATH_FREE <- TRUE
            if(grid[new_coords[1], new_coords[2]] == "#") {
                PATH_FREE <- FALSE
            }

            SKIP_ROTATION <- FALSE

            if(PATH_FREE) {
                i <- new_coords[1]
                j <- new_coords[2]

                direction <- case_when(
                    face == "one_blue"     ~ 0,
                    face == "one_pink"     ~ 0,
                    face == "four_green"   ~ 0,
                    face == "four_pink"    ~ 0,
                    face == "six_blue"     ~ 1,
                    face == "six_purple"   ~ 1,
                    face == "three_green"  ~ 1,
                    face == "two_orange"   ~ 2,
                    face == "two_yellow"   ~ 2,
                    face == "five_yellow"  ~ 2,
                    face == "five_red"     ~ 2,
                    face == "three_orange" ~ 3,
                    face == "two_purple"   ~ 3,
                    face == "six_red"      ~ 3
                )

                # The teleport counts as 1 move
                instructions$moves[k] <- remaining_move - 1
                SKIP_ROTATION <- TRUE
            }
        }
        if(!SKIP_ROTATION) {
            if(rotation != "X") {
                direction <- (direction + if_else(rotation == "R", 1, -1)) %% 4
            }
            k <- k + 1
        }

    }
    print(k)
    print(i)
    print(j)
    print(direction)
    print(TELEPORT)
    print(instructions$turns |> tail() )
    print(1000*i + 4*j + direction)
    return(1000*i + 4*j + direction)
}

solution <- solve(input, print = FALSE)

print_grid <- function(fncgrid = grid) {
    for(i in 1:nrow(fncgrid)) {
        for(j in 1:ncol(fncgrid)) {
            cat(fncgrid[i,j])
        }
        cat("\n")
    }
}

print_grid(grid)





if(FALSE) {
    # =========================================================================
    grid <- parse_input(input)
    instructions <- parse_directions(input)

    LENGTH <- nchar(grid[1])

    i <- 1
    j <- min(which(grid[1, ] == "."))
    direction <- 0

    max_k <- length(instructions$moves)
    k <- 1

    while(k < 111) {
        #' Direction
        #' mod 4
        #' 0 : Right : +x : >
        #' 1 : Down  : +y : v
        #' 2 : Left  : -x : <
        #' 3 : Up    : -y : ^
        rotation <- instructions$turns[k]
        # Number of moves
        moves <- instructions$moves[k]

        # R uses column major
        nonexistant <- which(grid == " ")
        convert_1dim <- function(x, y) {
            x + (y-1)*nrow(grid)
        }
        TELEPORT <- FALSE
        if(direction == 0){
            singular <- convert_1dim(i, j+moves)
            if(j + moves > 3*dim | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- dim - rmod(j, dim)

            }
        } else if(direction == 1) {
            singular <- convert_1dim(i+moves, j)
            if(i + moves > 200 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- dim - rmod(i, dim)

            }
        } else if(direction == 2){
            singular <- convert_1dim(i, j-moves)
            if(j - moves < 1 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- rmod(j, dim) - 1


            }
        } else if(direction == 3) {
            singular <- convert_1dim(i-moves, j)
            if(i - moves < 1 | singular %in% nonexistant) {
                TELEPORT <- TRUE
                allowed_move <- rmod(i, dim) - 1


            }
        }

        if(TELEPORT) {
            # allowed_move <- allowed_move - 1

            BLOCKED <- FALSE
            # check if path ahead has any '#'
            if(direction == 0) {
                if(any(grid[i, j:(j+allowed_move)] == "#")) BLOCKED <- TRUE
            } else if(direction == 1) {
                if(any(grid[i:(i+allowed_move), j] == "#")) BLOCKED <- TRUE
            } else if(direction == 2) {
                if(any(grid[i, j:(j-allowed_move)] == "#")) BLOCKED <- TRUE
            } else if(direction == 3) {
                if(any(grid[i:(i-allowed_move), j] == "#")) BLOCKED <- TRUE
            }

            if(BLOCKED == FALSE) {
                remaining_move <- moves - allowed_move
                moves <- allowed_move
            } else {
                TELEPORT <- FALSE
            }

            # determine amount we can move without teleport
        }


        if(direction %in% c(0,2)) {
            true_path_coords <- which(grid[i,] != " ")
            path <- grid[i, ][true_path_coords]
            true_values <- grid[i, ]
            coord <- j
        }else {
            true_path_coords <- which(grid[,j] != " ")
            path <- grid[, j][true_path_coords]
            true_values <- grid[ ,j]
            coord <- i
        }

        mod <-length(path)

        if(direction %in% c(0,1)) {
            operation <- `+`
        }else {
            operation <- `-`
        }

        # j <- 51
        # move 10
        # abs - 51:61
        # relative - 1:10

        # offset <- min(which(grid[i, ] != " ")) - 1
        offset <- min(which(true_values != " ")) - 1
        coord_rel <- coord - offset
        rel_coords <- rmod(coord_rel:operation(coord_rel, moves), mod = mod)
        abs_coords <- true_path_coords[rel_coords]
        # grid[i, abs_coords]
        wall_check <- which(path[rel_coords] == "#")
        if(length(wall_check) > 0) {
            wall_rel_to_move <- min(wall_check)
            if(length(wall_rel_to_move) == 1) {
                if(wall_rel_to_move < max(abs_coords)) {
                    abs_coords <- abs_coords[1:(wall_rel_to_move-1)]
                }
            }
        }


        true_values[abs_coords] <- names(dir_symbols)[dir_symbols == direction]

        if(direction %in% c(0,2)) {
            grid[i, ] <- true_values
            # i <- i
            j <- tail(abs_coords, 1)
        }else {
            grid[, j] <- true_values
            i <- tail(abs_coords, 1)
            # j <- j
        }

        SKIP_ROTATION <- FALSE
        if(TELEPORT) {
            # set i and j manually
            # rotate manually
            # append new instructions
            # skip rotation step

            # 1. need to know which face we are at
            face <- NULL
            if(i == 1 & direction == 3) {
                if(j < 101) {
                    face <- "one_blue"
                } else {
                    face <- "two_purple"
                }
            } else if(i == 50 & direction == 1) {
                face <- "two_orange"
            } else if(j == 150 & direction == 0) {
                face <- "two_yellow"
            } else if(j == 100 & direction == 0) {
                if(i < 101) {
                    face <- "three_orange"
                } else {
                    face <- "five_yellow"
                }
            } else if(i == 150 & direction == 1) {
                face <- "five_red"
            } else if(j == 50 & direction == 0) {
                face <- "six_red"
            } else if(i == 200 & direction == 1) {
                face <- "six_purple"
            } else if(j == 1 & direction == 2) {
                if(i < 151) {
                    face <- "four_pink"
                } else {
                    face <- "six_blue"
                }
            } else if(j == 51 & direction == 2) {
                if(i < 51) {
                    face <- "one_pink"
                } else {
                    face <- "three_green"
                }
            } else if(i == 101 & direction == 3) {
                face <- "four_green"
            }

            local_i <- rmod(i, dim)
            local_j <- rmod(j, dim)
            if(print) {
                cat(face, ":face\n")
                cat(i, ":i\n")
                cat(j, ":j\n")
                cat(direction, ":dir\n")
                cat(moves, ":moves\n")
                cat(k, ":k\n")
                print_grid(grid)
            }
            # new_coords_old <- case_when(
            #     face == "one_blue"     ~ c(local_j + 150, 1),
            #     face == "one_pink"     ~ c((150:101)[local_i], 1),
            #     face == "two_purple"   ~ c(200, local_j),
            #     face == "two_orange"   ~ c(local_j + 50, 100),
            #     face == "two_yellow"   ~ c((150:101)[local_i], 100),
            #     face == "three_orange" ~ c(50, local_i + 100),
            #     face == "three_green"  ~ c(101, local_i),
            #     face == "four_pink"    ~ c((50:1)[local_i], 51),
            #     face == "four_green"   ~ c(local_j + 50, 51),
            #     face == "five_yellow"  ~ c((50:1)[local_i], 150),
            #     face == "five_red"     ~ c(local_j + 150, 50),
            #     face == "six_red"      ~ c(150, local_i + 50),
            #     face == "six_blue"     ~ c(1, local_j + 50),
            #     face == "six_purple"   ~ c(1, local_j + 100)
            # )
            new_coords <- case_when(
                face == "one_blue"     ~ c( (151:200)[local_j],  1 ),
                face == "one_pink"     ~ c( (150:101)[local_i],  1 ),
                face == "two_purple"   ~ c( 200   , (1:50)[local_j]),
                face == "two_orange"   ~ c( (51:100)[local_j], 100),
                face == "two_yellow"   ~ c( (150:101)[local_i], 100),
                face == "three_orange" ~ c( 50, (101:150)[local_i]),
                face == "three_green"  ~ c( 101, local_i ),
                face == "four_pink"    ~ c( (50:1)[local_i], 51),
                face == "four_green"   ~ c( (51:100)[local_j], 51),
                face == "five_yellow"  ~ c( (50:1)[local_i], 150),
                face == "five_red"     ~ c( (151:200)[local_j], 50),
                face == "six_red"      ~ c( 150, (51:100)[local_i]),
                face == "six_blue"     ~ c( 1, (51:100)[local_i]),
                face == "six_purple"   ~ c( 1, (101:150)[local_j]),
            )
            # if(any(new_coords_old != new_coords)) {
            #     print(face)
            #     print(i)
            #     print(j)
            #     print(new_coords_old)
            #     print(new_coords)
            # }

            PATH_FREE <- TRUE
            if(grid[new_coords[1], new_coords[2]] == "#") {
                PATH_FREE <- FALSE
            }

            SKIP_ROTATION <- FALSE

            if(PATH_FREE) {
                i <- new_coords[1]
                j <- new_coords[2]

                direction <- case_when(
                    face == "one_blue"     ~ 0,
                    face == "one_pink"     ~ 0,
                    face == "four_green"   ~ 0,
                    face == "four_pink"    ~ 0,
                    face == "six_blue"     ~ 1,
                    face == "six_purple"   ~ 1,
                    face == "three_green"  ~ 1,
                    face == "two_orange"   ~ 2,
                    face == "two_yellow"   ~ 2,
                    face == "five_yellow"  ~ 2,
                    face == "five_red"     ~ 2,
                    face == "three_orange" ~ 3,
                    face == "two_purple"   ~ 3,
                    face == "six_red"      ~ 3
                )

                # The teleport counts as 1 move
                instructions$moves[k] <- remaining_move - 1
                SKIP_ROTATION <- TRUE
            }
        }
        if(!SKIP_ROTATION) {
            if(rotation != "X") {
                direction <- (direction + if_else(rotation == "R", 1, -1)) %% 4
            }
            k <- k + 1
        }
    }

print_grid()

    # =========================================================================
}




