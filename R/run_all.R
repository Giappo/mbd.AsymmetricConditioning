#' Run all
#' @inheritParams default_params_doc
#' @return List with meaningful measures for each parameter setting
#' @author Giovanni Laudanno
#' @export
run_all <- function(
  pars = c(0.2, 0.1, 1.5, 0.15),
  age = 10,
  lx_min = 20,
  lx_max = 50,
  saveit = TRUE
) {
  measure_asymmetric_conditioning(
    pars = pars,
    age = age,
    lx_min = lx_min,
    lx_max = lx_max,
    saveit = saveit
  )
  plot_asymmetric_conditioning(
    pars = pars,
    age = age,
    saveit = saveit
  )
}
