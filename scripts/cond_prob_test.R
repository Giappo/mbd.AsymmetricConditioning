# simulate data
seed <- 1
set.seed(seed)
lambda <- runif(n = 1, min = 0.2, max = 0.5)
mu <- 0 # with this we know the theoretical value Pc = 1
nu <- runif(n = 1, min = 0.9, max = 2.2)
q <- runif(n = 1, min = 0.08, max = 0.21)
pars <- c(lambda, mu, nu, q)
n_0 <- 2
cond <- 1
age <- 10
brts <- mbd_sim(pars = pars, n_0 = n_0, cond = cond, age = age, seed = seed)$brts

# low precision - low memory - hish speed
lx <- 30
time_30 <- system.time(
  prob_30 <- cond_prob(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
)[[3]]
cat("It took ", time_30, " seconds. \n", sep = "")
cat("Result is ", prob_30, ". It should be 1.\n", sep = "")

# higher precision - higher memory - slow speed
lx <- 100
time_100 <- system.time(
  prob_100 <- cond_prob(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
)[[3]]
cat("It took ", time_100, " seconds. \n", sep = "")
cat("Result is ", prob_100, ". It should be 1.\n", sep = "")

# # Can we achieve high precision - low memory - high speed ?
# # Something like this:
# cond_prob2 <- function(pars, brts, cond, n_0, lx) {
#   # awesome stuff!
# }
# lx <- 400
# time_400 <- system.time(
#   prob_400 <- cond_prob2(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
# )[[3]]
# time_likelihood <- system.time( # unconditioned likelihood time for the same branching times
#   mbd_loglik(pars = pars, brts = brts, n_0 = n_0, cond = 0, lx = lx)
# )
# # we want the conditioning time to be smaller than the time
# #  for the full likelihood computation.
# testit::assert(time_400 < time_likelihood)
# # we want the result to be accurate enough to have an error
# #  smaller than 1% (or any fixed %).
# testit::assert((1 - prob_400) <= 0.01)
