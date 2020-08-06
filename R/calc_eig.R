#' TBA
#'
#' calc_eig() returns TBA
#'
#' @param df TBA
#' @param n_eig TBA
#' @param nsamp TBA
#' @param ngame TBA
#' @param cent A tibble contains 2 sets of probability and respective time grid

#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export


calc_eig <- function(df, n_eig = 10, ngame, nsamp, cent = FALSE){
  diff <- ifelse(cent, "diff_cent", "diff_non_cent")

  eigV <- lapply(1:nsamp, function(i){
    sapply(1:nsamp, function(j){
      (df[[i]] %>% pull(!!sym(diff))) %*% (df[[j]] %>% pull(!!sym(diff))) /ngame %>% as.numeric()
    })
  }) %>% rlist::list.rbind() %>% {RSpectra::eigs_sym(A = (.), k = n_eig, which = "LM",
                                                     opts = list(retvec = FALSE))$values} %>%
    {(.)/nsamp}

  return(eigV)
}
