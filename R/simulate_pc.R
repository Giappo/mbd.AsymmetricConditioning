#' @export
simulate_pc <- function(
  pars,
  age,
  n_sims = 1e4
) {
  n_0 <- 2
  score <- 0
  pb <- txtProgressBar(min = 1, max = n_sims, style = 3)
  for (seed in 1:n_sims) {
    setTxtProgressBar(pb, seed)
    sim <- mbd::mbd_sim(
      pars = pars,
      n_0 = n_0,
      cond = 0,
      age = age,
      seed = seed
    )

    l_matrix <- sim$l_matrix
    alive <- l_matrix[l_matrix[, 4] == -1, ]
    alive <- matrix(alive, ncol = 4)
    crown_species_dead <- (length(unique(sign(alive[, 3]))) != n_0)
    crown_survival <- !crown_species_dead

    score <- score + 1 * crown_survival
  }
  close(pb)
  score / n_sims
}
