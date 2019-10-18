##### pc2

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_matrices_2 <- function(
  q,
  lq
) {

  pq <- q #bart's translation
  nu_matrix <- matrix(0, nrow = lq, ncol = lq)
  for (n1 in 0:(lq - 1)) {
    for (m1 in n1:(lq - 1)) {
      aux <- lchoose(n1, max(0, m1 - n1))
      aux <- exp(aux)
      aux <- aux * pq ^ (m1 - n1) * (1 - pq) ^ (2* n1 - m1)
      nu_matrix[m1 + 1, n1 + 1] <- aux
    }
  }
  rownames(nu_matrix) <- paste0("m1=", 0:(lq - 1))
  colnames(nu_matrix) <- paste0("n1=", 0:(lq - 1))

  empty_pp <- matrix(0, nrow = (lq + 2), ncol = (lq + 2))
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
  lq2 <- length(pvec)
  lq <- sqrt(lq2)

  mm <- 2:(lq + 1)
  mm_plus_one <- mm + 1
  mm_minus_one <- mm - 1

  pp <- matrix(pvec, nrow = lq, ncol = lq)
  pp2 <- empty_pp
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

  dp <- matrix(dp, nrow = lq2, ncol = 1)
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

  lq <- lx # maximal number of missing species for both m1 and m2

  # construct auxiliary matrix
  matrices <- cond_prob_matrices_2(q = q, lq = lq)

  # integrate equations
  parms <- list()
  parms$lambda <- lambda
  parms$mu <- mu
  parms$nu <- nu
  parms$nu_matrix <- matrices$nu_matrix
  parms$m1 <- matrices$m1
  parms$m2 <- matrices$m2
  parms$empty_pp <- matrices$empty_pp
  p_0 <- matrix(0, nrow = lq, ncol = lq)
  p_0[2, 2] <- 1
  p_0 <- matrix(p_0, nrow = lq ^ 2, ncol = 1)

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
  p_m1_m2 <- matrix(ode_out, nrow = lq, ncol = lq)
  print(paste0("sum is ", sum(p_m1_m2)))

  # compute conditioning probability
  pc <- 1 - p_m1_m2[1, 1] - p_m1_m2[2, 1] - p_m1_m2[1, 2]

  if (!(pc >= 0 && pc <= 1)) {
    if (debug_mode != TRUE) {
      stop("problems: pc is wrong!")
    }
  } # debug

  pc
}

##### pc3

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_matrices_3 <- function(
  q,
  lq
) {

  pq <- q #bart's translation
  nu_matrix <- matrix(0, nrow = lq, ncol = lq)
  for (m1 in 0:(lq - 1)) {
    for (a1 in 0:floor((m1 + 1) / 2)) { # nolint lintrbot is math's enemy
      aux <- log(m1 + 1) +
        lgamma(m1 - a1 + 1) -
        lgamma(m1 - 2 * a1 + 2) -
        lgamma(a1 + 1)
      aux <- exp(aux)
      aux <- aux * pq ^ a1 * (1 - pq) ^ (m1 + 1 - 2 * a1)
      nu_matrix[m1 + 1, m1 - a1 + 1] <- aux
    }
  }

  empty_qq <- matrix(0, nrow = (lq + 2), ncol = (lq + 2))
  m1 <- col(nu_matrix) - 1
  m2 <- row(nu_matrix) - 1
  list(
    nu_matrix = nu_matrix,
    empty_qq = empty_qq,
    m1 = m1,
    m2 = m2
  )
}

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_rhs1_3 <- function(
  qvec,
  lambda,
  mu,
  nu,
  k,
  nu_matrix,
  m1,
  m2,
  empty_qq,
  t
) {
  lq2 <- length(qvec)
  lq <- sqrt(lq2)

  mm <- 2:(lq + 1)
  mm_plus_one <- mm + 1
  mm_minus_one <- mm - 1

  qq <- matrix(qvec, nrow = lq, ncol = lq)
  qq2 <- empty_qq
  qq2[mm, mm] <- qq

  dq1 <- (2 * k + m1 - 1) * qq2[mm_minus_one, mm] +
    (2 * k + m2 - 1) * qq2[mm, mm_minus_one] -
    (2 * k + m1 + m2) * qq # ok

  dq2 <- (m1 + 1) * qq2[mm_plus_one, mm] +
    (m2 + 1) * qq2[mm, mm_plus_one] -
    (2 * k + m1 + m2) * qq # ok

  dq3 <- nu_matrix %*% qq %*% t(nu_matrix) - qq # first cc is m1, t(cc) is m2

  dq <- lambda * dq1 + mu * dq2 + nu * dq3

  dq <- matrix(dq, nrow = lq2, ncol = 1)
  dq
}

#' Auxilary function for cond_prob
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_rhs2_3 <- function(t, x, parms) {
  list(cond_prob_rhs1_3(
    qvec = x,
    lambda = parms$lambda,
    mu = parms$mu,
    nu = parms$nu,
    nu_matrix = parms$nu_matrix,
    k = parms$k,
    m1 = parms$m1,
    m2 = parms$m2,
    empty_qq = parms$empty_qq,
    t = t
  ))
}

#' Called by \link{mbd_loglik} if there is a conditioning != 0
#' @return the conditional probability
#' @author Giovanni Laudanno, Bart Haegeman
#' @export
cond_prob_3 <- function(
  pars,
  brts,
  cond,
  n_0 = 2,
  tips_interval = c(n_0 * (cond > 0), Inf),
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

  lq <- lx # maximal number of missing species for both m1 and m2

  # construct auxiliary matrix
  matrices <- cond_prob_matrices_3(q = q, lq = lq)

  # integrate equations
  parms <- list()
  parms$lambda <- lambda
  parms$mu <- mu
  parms$nu <- nu
  parms$kk <- 1
  parms$nu_matrix <- matrices$nu_matrix
  parms$m1 <- matrices$m1
  parms$m2 <- matrices$m2
  parms$empty_qq <- matrices$empty_qq
  q_0 <- c(y = c(1, rep(0, lq ^ 2 - 1)))

  ode_out <- deSolve::ode(
    y = q_0,
    times = times,
    func = cond_prob_rhs2_3,
    parms = parms,
    method = "lsoda",
    atol = 1e-100,
    rtol = 1e-6,
    tcrit = tt
  )[2, -1]
  q_m1_m2 <- matrix(ode_out, nrow = lq, ncol = lq)

  # compute conditioning probability
  m1 <- col(q_m1_m2) - 1
  m2 <- row(q_m1_m2) - 1
  p_m1_m2 <- q_m1_m2 / ((m1 + 1) * (m2 + 1)) # nolint lintr has issues with math
  pc <- sum(p_m1_m2)

  if (!(pc >= 0 && pc <= 1)) {
    if (debug_mode == TRUE) {
    } else {
      stop("problems: pc is wrong!")
    }
  } # debug

  pc
}
