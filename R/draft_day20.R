test <- c("1", "2", "-3", "3", "-2", "0", "4")
test <- as.numeric(test)

# Mod is length - 1
m <- function(n, mod = 6) {
    ((n-2) %% mod) + 2
}

insert <- function(vec, value, pos) {
    MAX_LENGTH <- length(vec)
    stopifnot(pos < MAX_LENGTH+2)
    out <- c(head(vec, pos-1), value)
    if(pos < MAX_LENGTH+1) {
        out <- c(out, vec[pos:MAX_LENGTH])
    }
    return(out)
}

pop <- function(vec) {
    return(tail(vec, -1))
}

x <- test
x <- readLines("inst/input20.txt") |> as.numeric()
I <- length(x)
df <- tibble::tibble(values = x, move_order = 1:I)

for(i in 1:I) {
    # i <- min(which(df$to_be_checked))
    # backwards_adjustment <- ifelse(df$values[i] < 0, -1, 0)
    # target_index <- m(i + df$values[i] + backwards_adjustment) # move forward number of positions

    val   <- df$values[df$move_order == i]
    current_index <- which(df$move_order == i)
    backwards_adjustment <- ifelse(val < 0, -1, 0)
    target_index <- m(current_index + val + backwards_adjustment, mod = I-1)

    df$values <- insert(df$values[-current_index], val, target_index)
    df$move_order <- insert(df$move_order[-current_index], 0, target_index)
    # print(df)
}
df

adjust <- function(n, mod) {
    ((n-1) %% mod) + 1
}

zero <- which(df$values == 0)
c1 <- adjust(zero + 1000, I)
c2 <- adjust(zero + 2000, I)
c3 <- adjust(zero + 3000, I)

sum(df$values[c(c1, c2, c3)])

#
v <- df$values

i = 1
v
val <- 100
v <- insert(v, val, m(i + val))
if(v[i] == val) {
    v <- v[-i]
}

v
# When i = 1, then pos is i + moves_forward
insert(v[-i], val, 1)
insert(v[-i], val, 2)
insert(v[-i], val, 3)
insert(v[-i], val, 4)
insert(v[-i], val, 5)
insert(v[-i], val, 6)
insert(v[-i], val, 7)

# When i = 1, then pos is i + moves_forward
i = 2
v
insert(v[-i], val, 1)
insert(v[-i], val, 2)
insert(v[-i], val, 3)
insert(v[-i], val, 4)
insert(v[-i], val, 5)
insert(v[-i], val, 6)
insert(v[-i], val, 7)

i = 6
v
insert(v[-i], 100, 1) # +1, and shift
insert(v[-i], 100, 2) # +2
insert(v[-i], 100, 3) # +3
insert(v[-i], 100, 4) # Somehow this is move +4
insert(v[-i], 100, 5) # +5
insert(v[-i], 100, 6) # Move 0 or Move +6?
insert(v[-i], 100, 7) # Move +1

#' 1 - Exclude
#' 2 - 2
#' 3 - 3
#' 4 - 4
#' 5 - 5
#' 6 - 0/6
#' 7 - 1/7
m(1)
m(2)
m(3)
m(4)
m(5)
m(6)
m(7)
m(8)
