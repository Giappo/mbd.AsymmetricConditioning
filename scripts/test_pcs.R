seed <- 1; set.seed(seed)
lambda <- 0.25
mu <- 0.15
nu <- 1.3
q <- 0.15
pars <- c(lambda, mu, nu, q)
n_0 <- 2
age <- 10
lx <- 140
n_sims <- 1e4

pcs_all <- calculate_all_pcs(
  pars = pars,
  n_0 = n_0,
  age = age,
  lx = lx
)

pc_sim <- simulate_pc(
  pars,
  n_0,
  age,
  n_sims = n_sims
)
