#' @export
calculate_all_pcs <- function(
  pars,
  age = 10,
  lx = 120
) {
  n_0 <- 2
  cond <- 1

  # this brts are useless: we only need the crown age
  brts <- c(age, age / 2)

  time_1 <- system.time(
    pc_1 <- mbd::cond_prob(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
  )[[3]]
  time_2 <- system.time(
    pc_2 <- cond_prob_2(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
  )[[3]]
  time_3 <- system.time(
    pc_3 <- cond_prob_3(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
  )[[3]]
  list(
    pc_1 = pc_1,
    pc_2 = pc_2,
    pc_3 = pc_3,
    time_1 = time_1,
    time_2 = time_2,
    time_3 = time_3
  )
}
