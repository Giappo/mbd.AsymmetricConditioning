.onLoad <- function(libname, pkgname) { # nolint .onLoad cannot be snake_case

  suppressPackageStartupMessages(
    lapply(
      c(
        "mbd"
      ),
      library,
      character.only = TRUE,
      warn.conflicts = FALSE
    )
  )
  invisible()
}
