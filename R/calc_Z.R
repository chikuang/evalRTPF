#' Calculator of the test statistics of delta test
#'
#' calc_Z() returns TBA
#'
#' @param df A tibble contains 2 sets of probability and respective time grid
#' @param pA TBA
#' @param pB TBA
#' @param Y TBA
#' @param grid TBA
#' @param nsamp TBA
#' @param ngame TBA
#' @param L Logical of whether to centre the data or not
#'
#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export


calc_Z <- function(df, pA, pB, Y, grid, nsamp, ngame, L = function(x,y) (x-y)^2){
  df %>% group_by(grid) %>%
    summarise(delta_n = mean(L(!!sym(pA), !!sym(Y)) - L(!!sym(pB), !!sym(Y)))) %>%
    {sum((.)$delta_n ^ 2) / nsamp * ngame}
}
