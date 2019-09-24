#' @export
simulate_pc <- function(
  pars,
  n_0,
  age,
  n_test = 1e4
) {
  score <- 0
  for (seed in 1:n_test) {
    set.seed(seed)
    brts <- mbd::mbd_sim(pars = pars, n_0 = n_0, cond = 0, age = age, seed = seed)$brts
    score <- score + 1 * (length(brts) != 0)
  }
  score / n_test
}
