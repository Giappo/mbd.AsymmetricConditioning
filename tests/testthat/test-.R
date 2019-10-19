context("cond_prob_2")

test_that("mu = 0", {
  pars <- c(0.2, 0, 1, 0.1)
  brts <- c(3)
  cond <- 1
  n_0 <- 2
  lx <- 30
  test <- cond_prob_2(
    pars = pars,
    brts = brts,
    cond = cond,
    n_0 = n_0,
    lx = lx
  )
  testthat::expect_equal(
    test$p_sum,
    1,
    tolerance = 1e-3
  )
  testthat::expect_equal(
    test$pc,
    1
  )
})

test_that("mu > 0", {
  pars <- c(0.2, 0.15, 1.5, 0.15)
  brts <- c(3)
  cond <- 1
  n_0 <- 2
  lx <- 30
  test <- cond_prob_2(
    pars = pars,
    brts = brts,
    cond = cond,
    n_0 = n_0,
    lx = lx
  )
  testthat::expect_equal(
    test$p_sum,
    1,
    tolerance = 1e-3
  )
})

test_that("bd", {
  pars <- c(0.2, 0.15, 0, 0)
  brts <- c(1)
  cond <- 1
  n_0 <- 2
  lx <- 40
  mu_vec <- seq(from = 0.05, to = 0.4, by = 0.05)
  # test0 <- test2 <- rep(0, length(mu_vec))
  for (m in seq_along(mu_vec)) {
    pars[2] <- mu_vec[m]
    test0 <- exp(
      DDD::bd_loglik(
        pars1 = c(pars[1], pars[2], 0, 0),
        pars2 = c(0, 0, 0, 0, n_0),
        brts = brts,
        missnumspec = 0
      ) -
        DDD::bd_loglik(
          pars1 = c(pars[1], pars[2], 0, 0),
          pars2 = c(0, 1, 0, 0, n_0),
          brts = brts,
          missnumspec = 0
        )
    )
    test1 <- cond_prob_2(
      pars = pars,
      brts = brts,
      cond = cond,
      n_0 = n_0,
      lx = lx
    )
    testthat::expect_equal(
      test1$p_sum,
      1,
      tolerance = 1e-3
    )
    testthat::expect_equal(
      test0,
      test1$pc,
      tolerance = max(1 - test1$p_sum, 10 * .Machine$double.eps)
    )
  }
})

test_that("p vs q", {
  pars <- c(0.2, 0.15, 1.5, 0.12)
  brts <- c(2)
  cond <- 1
  n_0 <- 2
  lx <- 40
  mu_vec <- seq(from = 0.05, to = pars[1], by = 0.05)
  for (m in seq_along(mu_vec)) {
    pars[2] <- mu_vec[m]
    test0 <- mbd::cond_prob(
      pars = pars,
      brts = brts,
      cond = cond,
      n_0 = n_0,
      lx = lx
    )
    test1 <- cond_prob_2(
      pars = pars,
      brts = brts,
      cond = cond,
      n_0 = n_0,
      lx = lx
    )
    testthat::expect_equal(
      test1$p_sum,
      1,
      tolerance = 1e-3
    )
    testthat::expect_equal(
      test0,
      test1$pc,
      tolerance = max(1 - test1$p_sum, 10 * .Machine$double.eps)
    )
  }
})

test_that("p vs sims", {
  pars <- c(0.2, 0.15, 1.5, 0.15)
  brts <- c(3)
  cond <- 1
  n_0 <- 2
  lx <- 50
  test_p <- cond_prob_2(
    pars = pars,
    brts = brts,
    cond = cond,
    n_0 = n_0,
    lx = lx
  )
  testthat::expect_equal(
    test_p$p_sum,
    1,
    tolerance = 1e-3
  )
  n_sims <- 1e5
  test_sim <- simulate_pc(
    pars = pars,
    age = max(brts),
    n_sims = n_sims,
    saveit = TRUE
  )
  testthat::expect_equal(
    test_sim,
    test_p$pc,
    tolerance = 1 / sqrt(n_sims)
  )
})
