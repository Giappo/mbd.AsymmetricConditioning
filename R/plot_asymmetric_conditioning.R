#' Measure Asymmetric conditioning
#' @return a dataframe
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
plot_asymmetric_conditioning <- function(
  pars,
  age = 10,
  saveit = TRUE
) {
  n_0 <- 2

  # pass Rcheck
  measure <- NULL; rm(measure)
  median <- NULL; rm(median)
  quantile <- NULL; rm(quantile)
  x <- NULL; rm(x)

  full_filename <- get_full_filename(
    pars = pars,
    age = age
  )
  file <- full_filename
  testit::assert(file.exists(full_filename))
  load(full_filename)

  df <- measure
  df_pc <- df[, !grepl(pattern = "t_", x = names(df))]

  meltdf <- reshape2::melt(df_pc, id = "lx")
  plot <- ggplot2::ggplot(
    data = meltdf,
    ggplot2::aes(
      x = lx,
      y = value,
      colour = variable,
      group = variable
    )
  ) +
    ggplot2::geom_line()
  plot_filename <- paste0(
    "pc_plots",
    ".png"
  )
  if (saveit == TRUE) {
    ggplot2::ggsave(
      filename = file.path(dirname(full_filename), plot_filename),
      plot = plot,
      width = 10,
      height = 10,
    )
  }
  plot
}
