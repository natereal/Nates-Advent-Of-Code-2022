#' Day 12: Hill Climbing Algorithm
#'
#' [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' You try contacting the Elves using your [handheld
#' device]{title="When you look up the specs for your handheld device, every field just says \"plot\"."},
#' but the river you\'re following must be too low to get a decent signal.
#'
#' You ask the device for a heightmap of the surrounding area (your puzzle
#' input). The heightmap shows the local area from above broken into a
#' grid; the elevation of each square of the grid is given by a single
#' lowercase letter, where `a` is the lowest elevation, `b` is the
#' next-lowest, and so on up to the highest elevation, `z`.
#'
#' Also included on the heightmap are marks for your current position (`S`)
#' and the location that should get the best signal (`E`). Your current
#' position (`S`) has elevation `a`, and the location that should get the
#' best signal (`E`) has elevation `z`.
#'
#' You\'d like to reach `E`, but to save energy, you should do it in *as
#' few steps as possible*. During each step, you can move exactly one
#' square up, down, left, or right. To avoid needing to get out your
#' climbing gear, the elevation of the destination square can be *at most
#' one higher* than the elevation of your current square; that is, if your
#' current elevation is `m`, you could step to elevation `n`, but not to
#' elevation `o`. (This also means that the elevation of the destination
#' square can be much lower than the elevation of your current square.)
#'
#' For example:
#'
#'     Sabqponm
#'     abcryxxl
#'     accszExk
#'     acctuvwj
#'     abdefghi
#'
#' Here, you start in the top-left corner; your goal is near the middle.
#' You could start by moving down or right, but eventually you\'ll need to
#' head toward the `e` at the bottom. From there, you can spiral around to
#' the goal:
#'
#'     v..v<<<<
#'     >v.vv<<^
#'     .>vv>E^^
#'     ..v>>>^^
#'     ..>>>>>^
#'
#' In the above diagram, the symbols indicate whether the path exits each
#' square moving up (`^`), down (`v`), left (`<`), or right (`>`). The
#' location that should get the best signal is still `E`, and `.` marks
#' unvisited squares.
#'
#' This path reaches the goal in `31` steps, the fewest possible.
#'
#' *What is the fewest steps required to move from your current position to
#' the location that should get the best signal?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12())
#' f12b()
f12a <- function(x) {

}


#' @rdname day12
#' @export
f12b <- function(x) {

}


f12_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a = c(
        "Sabqponm",
        "abcryxxl",
        "accszExk",
        "acctuvwj",
        "abdefghi"
    )
  )
  l[[example]]
}
