tag_css_style <- function(x = NULL, ...) {
  if (is.null(x) || !any(nzchar(x)) || length(x) == 0) {
    return(NULL)
  }
  htmltools::tags$style(
    htmltools::HTML(
      paste(x, collapse = "\n")
    ),
    ...
  )
}

tag_js_script <- function(x = NULL, ...) {
  if (is.null(x) || !any(nzchar(x)) || length(x) == 0) {
    return(NULL)
  }
  htmltools::tags$script(
    htmltools::HTML(
      paste(x, collapse = "\n")
    ),
    ...
  )
}

shallow_update_list <- function(x, y) {
  if (is.null(y) || !length(y)) return(x)
  if (is.null(x)) x <- list()
  for (v in names(y)) {
    if (!is.null(y[[v]])) {
      x[[v]] <- y[[v]]
    } else {
      x[v] <- list(NULL)
    }
  }
  x
}
