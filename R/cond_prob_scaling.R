if (1 == 2) { # run this if needed
  # Install package
  devtools::install_github("Giappo/mbd@debug")

  # The functions we need are in the file "mbd_conditioning.R". They are:
  # "cond_prob", "cond_prob_rhs1", "cond_prob_rhs2", "cond_prob_matrices".
  utils::browseURL(
    "https://github.com/Giappo/mbd/blob/debug/R/mbd_conditioning.R"
  )
}

# A few things here:
# 1. For some values you might still get errors due to sums between very
#  small and very big numbers.
# 2. Parameter lx is the max number of species considered. Since here left and
#  right crown species are considered differently, the q vector will have
#  lx^2 entries. This makes computation times rise very fast as lx increase.
# 3. Cutting lx to some value will create an error on the estimation of the
#  conditional probability. In this example mu = 0, so Pc = 1 (in theory). We
#  can then evaluate how much of this probability we can get for a given lx.

# The computation might be slow. Check the attached figure for the
#  results if you don't want to wait

max_seed <- 5
min_lx <- 10
max_lx <- 36
lxs <- seq(from = min_lx, to =  max_lx)
probs <- times <- rep(0, max_lx)
for (seed in 1:max_seed) {
  for (lx in lxs) {
    set.seed(seed)
    lambda <- runif(n = 1, min = 0.2, max = 0.5)
    # mu <- runif(n = 1, min = 0.2, max = lambda)
    mu <- 0 # with this we know the theoretical value Pc = 1
    nu <- runif(n = 1, min = 0.9, max = 2.2)
    q <- runif(n = 1, min = 0.08, max = 0.21)
    pars <- c(lambda, mu, nu, q)
    n_0 <- 2
    cond <- 1
    age <- 10
    brts <- mbd_sim(pars = pars, n_0 = n_0, cond = cond, age = age, seed = seed)$brts
    times[lx] <- system.time(
      prob <- cond_prob(pars = pars, brts = brts, cond = cond, n_0 = n_0, lx = lx)
    )[[3]] + times[lx]
    testit::assert(prob >= 0 && prob <= 1)
    if (seed == 1) {probs[lx] <- prob}
  }
}
times <- times / max_seed # average over simulations

# Plot times
par(mfrow = c(2, 1))
plot(times, ylab = "time", xlab = "lx")

# How much lx is required? Prob is supposed to be 1!
plot(probs, ylab = "total probability", xlab = "lx")
