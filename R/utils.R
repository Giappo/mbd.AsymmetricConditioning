#' @noRd
get_pkg_name <- function() {
  pkg_name <- "mbd.AsymmetricConditioning"
  pkg_name
}

#' @noRd
get_pkg_path <- function() {
  list.files(getwd())
}

#' @noRd
to_string2 <- function(
  var
) {
  gsub(x = toString(var), pattern = " ", replacement = "")
}

#' @noRd
get_full_filename <- function(
  pars,
  age
) {
  # folder structure
  x <- unlist(strsplit(getwd(), .Platform$file.sep))
  if (get_pkg_name() %in% x) {
    y <- which(x == get_pkg_name())
    project_folder <- paste0(x[1:y], collapse = .Platform$file.sep)
  } else {
    home_folder <- paste0(x, collapse = .Platform$file.sep)
    project_folder <- file.path(home_folder, get_pkg_name())
  }
  rm(x)
  if (!dir.exists(project_folder)) {
    dir.create(project_folder)
  }

  data_folder <- file.path(project_folder, "inst", "extdata")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
  }

  parsetting <- paste0(
    "pars=", to_string2(pars),
    "-",
    "age=", to_string2(age)
  )
  parsetting <- gsub(parsetting, pattern = " ", replacement = "")
  parsetting_folder <- file.path(data_folder, parsetting)
  if (!dir.exists(parsetting_folder)) {
    dir.create(parsetting_folder)
  }
  filename <- paste0(
    "result",
    ".Rdata"
  )
  full_filename <- file.path(parsetting_folder, filename)
  full_filename
}
