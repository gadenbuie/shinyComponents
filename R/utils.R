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
