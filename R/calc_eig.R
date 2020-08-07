#' Calculator for eigenvalues
#'
#' calc_eig() returns TBA
#'
#' @param df TBA
#' @param n_eig TBA
#' @param nsamp TBA
#' @param ngame TBA
#' @param grid TBA
#' @param cent A tibble contains 2 sets of probability and respective time grid

#' @return A value of the test statistics and the associated p-value between two sets of real-time probabilistic forecasts
#'
#' @export

calc_eig <- function(df, n_eig = 10, ngame, nsamp, grid = "grid", cent = FALSE){
  diff <- ifelse(cent, "diff_cent", "diff_non_cent")

  df_list <- df %>% dplyr::select(!!sym(grid), !!sym(diff)) %>% group_split(!!sym(grid), .keep = FALSE)
  df_vec <- lapply(seq_along(df_list), function(x){
    df_list[[x]] %>% unlist() %>% as.vector()
  })
  rm(df_list)
  eigV <- lapply(1:nsamp, function(i){
    sapply(1:nsamp, function(j){
      as.numeric( df_vec[[i]] %*% df_vec[[j]] /ngame)
    })
  }) %>% rlist::list.rbind() %>% {RSpectra::eigs_sym(A = (.), k = n_eig, which = "LM",
                                                     opts = list(retvec = FALSE))$values} %>%
    {(.)/nsamp}

  return(eigV)
}
