seed <- 1; set.seed(seed)
lambda <- 0.25
mu <- 0.15
nu <- 1.3
q <- 0.15
pars <- c(lambda, mu, nu, q)
n_0 <- 2
age <- 10

pcs_all <- all_pcs(
  pars = pars,
  n_0 = n_0,
  age = age,
  lx = 120,
  seed = 1
)

pc_sim <- simulate_pc(
  pars,
  n_0,
  age,
  n_test = 3e3
)
