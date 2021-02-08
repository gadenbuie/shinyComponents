read_utf8 <- function(path) {
  if (inherits(path, "connection"))
    con <- path
  else {
    con <- base::file(path, encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

extract_yaml <- function(path) {
  x <- read_utf8(path)
  if (!sum(grepl("^---", x)) >= 2) return()
  yaml_between <- grep("^---\\s*", x)[1:2] # assume first two ---
  yaml::yaml.load(x[(yaml_between[1] + 1):(yaml_between[2] - 1)])
}

html_dependency <- function(..., all_files = FALSE) {
  # default to all_files = FALSE
  htmltools::htmlDependency(..., all_files = all_files)
}

prep_html_dependency <- function(dep, envir = new.env()) {
  if (rlang::is_string(dep)) {
    res <- eval(parse(text = dep), envir = envir)
    if (rlang::is_function(res)) res() else res
  } else if (rlang::is_list(dep)) {
    do.call(html_dependency, dep)
  } else {
    stop("Unsupported dependency type: ", dep)
  }
}

prep_html_dependencies <- function(deps = NULL, envir = new.env()) {
  if (is.null(deps)) return()

  has_name <- vapply(deps, FUN.VALUE = logical(1), function(x) {
    rlang::is_string(x) || "name" %in% names(x)
  })
  if (!all(has_name)) {
    stop("All dependencies must have a name item")
  }

  dep_names <- vapply(deps, FUN.VALUE = character(1), function(x) {
    if (rlang::is_string(x)) x else x[["name"]]
  })
  names(deps) <- dep_names
  lapply(deps, prep_html_dependency, envir = envir)
}
