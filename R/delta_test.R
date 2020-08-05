#' A statistical test based on the square norm
#'
#' delta_test() returns the test statistics between two real-time probabilistic forecasts under square norm
#'
#' @param df First set of probabilistic forecasts
#' @param D Scalar of how many eigenvalues to use
#' @param N_MC Scalar of how many Monte Carlo simulation to do
#' @param center Logical of whether to center the data or not
#' @param grid Logical of whether to center the data or not
#' @param pA Logical of whether to center the data or not
#' @param pB Logical of whether to center the data or not
#' @param L Logical of whether to center the data or not
#'
#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export

delta_test <- function(df, D = 10, N_MC = 5000, center = FALSE,
                       grid = "grid", pA = "phat_A", pB = "phat_B", L = function(x, y){ (x-y)^2}){

  Y <- as.symbol("Y")
  game_num <- as.symbol("game_num")
  ngame <- group_by(df, grid) %>% summarise(n = n()) %>% slice_head(1) %>% pull(n)
  nsamp <- group_by(df, game_num) %>% summarise(n = n()) %>% slice_head(1) %>% pull(n)
  Z <- group_by(df, grid) %>%
    summarise(delta_n = mean( L(pA, Y) - L(pB, Y))) %>%
    {sum((.)$delta_n^2)/nsamp * ngame}

  temp <- group_by(df, grid) %>% group_split()

  ## This might not work, check it later
  diff <- ifelse(center, "diff_B", "diff_A")

  eigV <- lapply(1:nsamp, function(i){
    sapply(1:nsamp, function(j){
      (temp[[i]] %>% pull(!!sym(diff))) %*% (temp[[j]] %>% pull(!!sym(diff))) /ngame %>% as.numeric()
    })
  }) %>% rlist::list.rbind() %>% {RSpectra::eigs_sym(A = (.), k = D, which = "LM",
                                                     opts = list(retvec = FALSE))$values} %>%
    {(.)/nsamp}

  set.seed(520)
  MC <- sapply(1:N_MC, function(x){
    crossprod(eigV, stats::rchisq(D, df = 1))
  })
  p_val <- 1 - stats::ecdf(MC)(Z)

  return(c(Z, p_val))
}
