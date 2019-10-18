##### pc2

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_matrices_2 <- function(
  q,
  lx
) {

  pq <- q #bart's translation
  nu_matrix <- matrix(0, nrow = lx, ncol = lx)
  for (n1 in 0:(lx - 1)) {
    for (m1 in n1:(lx - 1)) {
      aux <- lchoose(n1, max(0, m1 - n1))
      aux <- exp(aux)
      aux <- aux * pq ^ (m1 - n1) * (1 - pq) ^ (2* n1 - m1)
      nu_matrix[m1 + 1, n1 + 1] <- aux
    }
  }
  rownames(nu_matrix) <- paste0("m1=", 0:(lx - 1))
  colnames(nu_matrix) <- paste0("n1=", 0:(lx - 1))

  empty_pp <- matrix(0, nrow = (lx + 2), ncol = (lx + 2))
  m1 <- col(nu_matrix) - 1
  m2 <- row(nu_matrix) - 1
  list(
    nu_matrix = nu_matrix,
    empty_pp = empty_pp,
    m1 = m1,
    m2 = m2
  )
}

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_rhs1_2 <- function(
  pvec,
  lambda,
  mu,
  nu,
  nu_matrix,
  m1,
  m2,
  empty_pp,
  t
) {
  lx2 <- length(pvec)
  lx <- sqrt(lx2)

  mm <- 2:(lx + 1)
  mm_plus_one <- mm + 1
  mm_minus_one <- mm - 1

  pp <- matrix(pvec, nrow = lx, ncol = lx)
  rownames(m2) <- rownames(m1) <- rownames(pp) <-
    paste0("n2=", 0:(lx - 1))
  colnames(m2) <- colnames(m1) <- colnames(pp) <-
    paste0("n1=", 0:(lx - 1))
  pp2 <- empty_pp
  rownames(pp2) <- paste0("n2=", -1:lx)
  colnames(pp2) <- paste0("n1=", -1:lx)
  pp2[mm, mm] <- pp

  dp1 <- (m1 - 1) * pp2[mm, mm_minus_one] +
    (m2 - 1) * pp2[mm_minus_one, mm] -
    (m1 + m2) * pp # ok

  dp2 <- (m1 + 1) * pp2[mm, mm_plus_one] +
    (m2 + 1) * pp2[mm_plus_one, mm] -
    (m1 + m2) * pp # ok

  dp3 <- nu_matrix %*% pp %*% t(nu_matrix) - pp # first cc is m1, t(cc) is m2
  # dp3 <- t(nu_matrix) %*% pp %*% nu_matrix - pp # first cc is m1, t(cc) is m2

  dp <- lambda * dp1 + mu * dp2 + nu * dp3

  dp <- matrix(dp, nrow = lx2, ncol = 1)
  dp
}

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_rhs2_2 <- function(t, x, parms) {
  list(cond_prob_rhs1_2(
    pvec = x,
    lambda = parms$lambda,
    mu = parms$mu,
    nu = parms$nu,
    nu_matrix = parms$nu_matrix,
    m1 = parms$m1,
    m2 = parms$m2,
    empty_pp = parms$empty_pp,
    t = t
  ))
}

#' Called by \link{mbd_loglik} if there is a conditioning != 0
#' @return the conditional probability
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_2 <- function(
  pars,
  brts,
  cond,
  n_0 = 2,
  lx = 30,
  debug_mode = FALSE
) {
  if (n_0 != 2) {
    stop("This works only for n_0 == 2.")
  }
  if (cond == 0) {
    return(1)
  }
  lambda <- pars[1]
  mu <- pars[2]
  nu <- pars[3]
  q <- pars[4]
  tt <- max(abs(brts)) # time between crown age and present
  times <- c(0, tt)

  # construct auxiliary matrix
  matrices <- cond_prob_matrices_2(q = q, lx = lx)

  # integrate equations
  parms <- list()
  parms$lambda <- lambda
  parms$mu <- mu
  parms$nu <- nu
  parms$nu_matrix <- matrices$nu_matrix
  parms$m1 <- matrices$m1
  parms$m2 <- matrices$m2
  parms$empty_pp <- matrices$empty_pp
  p_0 <- matrix(0, nrow = lx, ncol = lx)
  p_0[2, 2] <- 1
  p_0 <- matrix(p_0, nrow = lx ^ 2, ncol = 1)

  ode_out <- deSolve::ode(
    y = p_0,
    times = times,
    func = cond_prob_rhs2_2,
    parms = parms,
    method = "lsoda",
    atol = 1e-100,
    rtol = 1e-6,
    tcrit = tt
  )[2, -1]
  p_m1_m2 <- matrix(ode_out, nrow = lx, ncol = lx)
  somma <- sum(p_m1_m2)

  # compute conditioning probability
  pc <- 1 + p_m1_m2[1, 1] - sum(p_m1_m2[, 1]) - sum(p_m1_m2[1, ])

  if (!(pc >= 0 && pc <= 1)) {
    if (debug_mode != TRUE) {
      stop("problems: pc is wrong!")
    }
  } # debug

  list(pc = pc, p_sum = somma)
}
