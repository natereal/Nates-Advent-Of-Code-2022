solve <- function(input, dim = 4, print = FALSE) {

    grid <- parse_input(test)
    instructions <- parse_directions(test)

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
        print(instructions)
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
            if(i + moves > 4*dim | singular %in% nonexistant) {
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

        # TODO:
        #' The issue is when the number of moves would put you
        #' off the grid, but there is a wall blocking the path
        #' in another square. It only checks for blocked paths
        #' within the current square.
        #' So the "allowed moves' is incorrect;
        #' the allowed moves should take into account the fact
        #' you are allowed move into another square if it is an
        #' adjacent one.
        #'
        #' ughhhhh

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
                original_moves <- moves
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

        ROTATE <- TRUE
        if(TELEPORT) {
            # set i and j manually
            # rotate manually
            # append new instructions
            # skip rotation step

            # 1. need to know which face we are at

            local_i <- rmod(i, dim)
            local_j <- rmod(j, dim)

            face <- NULL
            face <- case_when(
                direction == 3 ~ case_when(
                    j %in%  1:4  ~ "one_green",
                    j %in%  5:8  ~ "two_purple",
                    j %in%  9:12 ~ "zero_green",
                    j %in% 13:16 ~ "five_orange"
                ),
                direction == 0 ~ case_when(
                    i %in%  1:4  ~ "zero_red",
                    i %in%  5:8  ~ "three_orange",
                    i %in%  9:12 ~ "five_red"
                ),
                direction == 1 ~ case_when(
                    j %in%  1:4  ~ "one_blue",
                    j %in%  5:8  ~ "two_pink",
                    j %in%  9:12 ~ "four_blue",
                    j %in% 13:16 ~ "five_yellow"
                ),
                direction == 2 ~ case_when(
                    i %in%  1:4  ~ "zero_purple",
                    i %in%  5:8  ~ "one_yellow",
                    i %in%  9:12 ~ "four_pink"
                )
            )

            # if(print) {
            #     cat(face, ":face\n")
            #     cat(i, ":i\n")
            #     cat(j, ":j\n")
            #     cat(direction, ":dir\n")
            #     cat(moves, ":moves\n")
            #     cat(k, ":k\n")
            #     print_grid(grid)
            # }
            new_coords <- case_when(
                face == "one_green"   ~ c(1, local_j + 8),
                face == "two_purple"  ~ c(local_j, 9),
                face == "zero_green"  ~ c(5, local_j),
                face == "five_orange" ~ c((8:5)[local_j], 12),
                face == "zero_red" ~ c((12:9)[local_i], 16),
                face == "three_orange" ~ c(9,  (16:13)[local_i]),
                face == "five_red" ~ c( local_i, 12),
                face == "one_blue" ~ c( 12, (12:9)[local_j] ),
                face == "two_pink" ~ c( (12:9)[local_j], 8),
                face == "four_blue" ~ c( 8, (4:1)[local_j]),
                face == "five_yellow" ~ c( (8:5)[local_j], 1),
                face == "zero_purple" ~ c( 5, local_j + 4),
                face == "one_yellow" ~ c( 12, (16:13)[local_i]),
                face == "four_pink" ~ c( 8, (8:5)[local_i])
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

            if(PATH_FREE) {
                i <- new_coords[1]
                j <- new_coords[2]

                direction <- case_when(
                    face == "one_green"   ~ 1,
                    face == "two_purple"  ~ 0,
                    face == "zero_green"  ~ 1,
                    face == "five_orange" ~ 2,
                    face == "zero_red" ~ 2,
                    face == "three_orange" ~ 1,
                    face == "five_red" ~ 2,
                    face == "one_blue" ~ 3,
                    face == "two_pink" ~ 0,
                    face == "four_blue" ~ 3,
                    face == "five_yellow" ~ 0,
                    face == "zero_purple" ~ 1,
                    face == "one_yellow" ~ 3,
                    face == "four_pink" ~ 3
                )

                # The teleport counts as 1 move
                instructions$moves[k] <- remaining_move - 1
                ROTATE <- FALSE
            }
        }
        if(ROTATE) {
            if(rotation != "X") {
                direction <- (direction + if_else(rotation == "R", 1, -1)) %% 4
            }
            k <- k + 1
        }
        print(instructions)
        print_grid()

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

solve(test, print = TRUE)
