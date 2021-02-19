#' Knit Component as a Shiny R Markdown Document
#'
#' @keywords internal
knit_preview <- function(input, ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("`rmarkdown` is required: install.packages('rmarkdown')")
  }

  input <- normalizePath(input)
  input_dir <- dirname(input)

  contents <- c(
    "---",
    "runtime: shiny",
    "---",
    "",
    '```{r include=FALSE}',
    "knitr::opts_chunk$set(echo = FALSE)",
    sprintf(
      'library(shinyComponents)\nx <- ShinyComponent$new("%s")',
      input
    ),
    "```",
    "",
    "```{r}",
    'if (is.list(x$ui)) x$ui$ui() else x$ui()',
    "```",
    "",
    "```{r}",
    'if (is.list(x$server)) x$server$server else x$server()',
    "```"
  )

  tmprmd <- tempfile("shiny-component-preview", fileext = ".Rmd")
  on.exit(unlink(tmprmd), add = TRUE)
  writeLines(contents, tmprmd)
  rmarkdown::run(tmprmd, shiny_args = list(launch.browser = TRUE))
}
