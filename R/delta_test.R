#' A statistical test based on the square norm
#'
#' delta_test() returns the test statistics between two real-time probabilistic forecasts under square norm
#'
#' @param a First set of probabilistic forecasts
#' @param b Vector of simulated/estimated capture rates.
#' @param D Scalar of how many eigenvalues to use
#' @param N_MC Scalar of how many Monte Carlo simulation to do
#' @param center Logical of whether to center the data or not
#'
#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export

delta_test <- function(a, b, D = 10, N_MC = 5000, center = TRUE){
  a = tibble(a = a); b = tibble(b =b )
  tmp_df <- a %>% inner_join(b, by = "grid")

  my_df %>% group_by(grid) %>% mutate(p_bar_12 = mean(phat_logit - IP),
                                      diff_A = phat_logit - IP,
                                      diff_B = phat_logit - IP - p_bar_12) %>% ungroup()
}
