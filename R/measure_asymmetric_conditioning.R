#' Measure Asymmetric conditioning
#' @return a dataframe
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
measure_asymmetric_conditioning <- function(
  pars,
  age = 10,
  lx_min = 20,
  lx_max = 50,
  saveit = TRUE
) {
  n_0 <- 2

  # pass Rcheck
  measure <- NULL; rm(measure)

  full_filename <- get_full_filename(
    pars = pars,
    age = age
  )

  lx_seq0 <- seq(from = lx_min, to = lx_max, by = 10)
  if (file.exists(full_filename)) {
    load(full_filename)

    # pc
    pc_sim <- unique(measure$pc_sim)
    testit::assert(length(pc_sim) == 1)

    # lx seq
    lx_prev <- unique(measure$lx)
    lx_seq <- lx_seq0[!lx_seq0 %in% lx_prev]
    if (length(lx_seq) == 0) {
      stop("This data is already available")
    }
  } else {
    # pc
    n_sims <- 1e5
    pc_sim <- simulate_pc(
      pars = pars,
      age = age,
      n_sims = n_sims
    )

    # lx seq
    lx_seq <- lx_seq0
  }

  time_1s <- time_2s <- time_3s <-
    pc_1s <- pc_2s <- pc_3s <- rep(0, length(lx_seq))
  i <- 1
  for (lx in lx_seq) {
    print(lx)
    pc_all <- calculate_all_pcs(
      pars = pars,
      age = age,
      lx = lx
    )
    pc_1s[i] <- pc_all$pc_1
    pc_2s[i] <- pc_all$pc_2
    pc_3s[i] <- pc_all$pc_3
    time_1s[i] <- pc_all$time_1
    time_2s[i] <- pc_all$time_2
    time_3s[i] <- pc_all$time_3
    i <- i + 1
  }
  measure <- data.frame(
    lx = lx_seq,
    pc_1 = pc_1s,
    pc_2 = pc_2s,
    pc_3 = pc_3s,
    t_1 = time_1s,
    t_2 = time_2s,
    t_3 = time_3s,
    pc_sim = pc_sim
  )
  if (saveit == TRUE) {
    save(measure, file = full_filename)
  }
  measure
}
