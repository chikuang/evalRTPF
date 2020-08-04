#' The difference in probability
#'
#' cal_prob_diff() returns the difference in probabilitistic forecasts of each instance at each time points
#'
#' @param df A tibble contains 2 sets of probability and respective time grid
#' @param center Logical of whether to center the data or not
#'
#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export


cal_prob_diff <- function(df, center = FALSE){
  group_by(df, grid) %>% mutate(p_bar_12 = mean(phat_1, phat_2)) %>% ungroup() %>%
    mutate(diff_prob = ifelse(center, phat_1 - phat_2 - p_bar_12, phat_1 - phat_2))
}
