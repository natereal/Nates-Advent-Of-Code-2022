# root = pppw + sjmn
# dbpl = 5
# cczh = sllz + lgvd
# zczc = 2
# ptdq = humn - dvpt
# dvpt = 3
# lfqf = 4
# humn = 5
# ljgn = 2
# sjmn = drzm * dbpl
# sllz = 4
# pppw = cczh / lfqf
# lgvd = ljgn * ptdq
# drzm = hmdt - zczc
# hmdt = 32
# root
#
#
# ####
#
# root <- function(){ pppw() + sjmn() }
# dbpl <- function(){ 5}
# cczh <- function(){ sllz() + lgvd() }
# zczc <- function(){ 2}
# ptdq <- function(){ humn()  - dvpt()}
# dvpt <- function(){ 3}
# lfqf <- function(){ 4}
# humn <- function(){ 5}
# ljgn <- function(){ 2}
# sjmn <- function(){ drzm() * dbpl()}
# sllz <- function(){ 4}
# pppw <- function(){ cczh() / lfqf()}
# lgvd <- function(){ ljgn() * ptdq()}
# drzm <- function(){ hmdt() - zczc()}
# hmdt <- function(){ 32}
#
# root()

###

test <- c(
    "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32"
)

eval("2 + 2")

input <- test
input <- readLines("inst/input21.txt")

commands <- input |>
    stringr::str_split(":| ") |>
    purrr::map(.f = ~ {
        if(length(.x) == 3) {
            return(paste0(.x[1], " <- function() {", .x[3], "}"))
        } else {
            return(paste0(.x[1], " <- function() {", .x[3], "() ", .x[4], " ", .x[5], "()}"))
        }
    })

for(i in 1:length(commands)) {
    eval(parse(text = commands[[i]]))
}

options(scipen = 30)
root()

# root: rvrh + hzgl
root <- function() {rvrh() == hzgl()}
root()

humn <- function(n=1) {n}
rvrh()
hzgl()
# hzgl is constant

humn <- function(n=1) {n}
rvrh()
humn <- function(n=2) {n}
rvrh()
humn <- function(n=3) {n}
rvrh()
# n: +1 -> rvrh: -3

humn <- function(n=1) {n}
var <- rvrh()
const <- hzgl()
target <- (var - const) / 3

target <- 3453735819199
humn <- function(n=target) {n}
rvrh()
hzgl()
target <- target + ((rvrh() - hzgl()) / 3)
root()
target
rvrh() == hzgl()
