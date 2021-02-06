read_knitr_chunks <- function(file) {
  chunks <- list()

  old_opts_knit <- knitr::opts_knit$get("unnamed.chunk.label")
  knitr::opts_knit$set(unnamed.chunk.label = "__unnamed__")

  r_engine <- knitr::knit_engines$get("r")
  knitr::knit_engines$set(ui = identity, server = identity)

  hook_get_chunk <- function(before, options, envir) {
    if (!isTRUE(before)) {
      label <- options$label
      chunk <- knitr::knit_code$get(label)
      chunks[[label]]$chunk <<- c(chunk)
      chunks[[label]]$chunk_opts <<- attributes(chunk)$chunk_opts
      chunks[[label]]$options <<- options
      chunks[[label]]$engine <<- options$engine
    }
  }

  hook_chunk_disable <- function(options) {
    options$eval <- FALSE
    options
  }

  # cache original hooks
  old_chunk <- knitr::knit_hooks$get("chunk")
  old_eval <- knitr::opts_hooks$get("eval")

  # overwrite hooks
  knitr::knit_hooks$set(chunk = hook_get_chunk)
  knitr::opts_hooks$set(eval = hook_chunk_disable)

  # render component Rmd to extract chunk information
  tmpfile <- tempfile(fileext = "md")
  outfile <- knitr::knit(file, output = tmpfile, quiet = TRUE)

  # clean up temp files and restore hooks
  unlink(outfile)
  restore_knitr_hooks(old_chunk, old_eval)
  knitr::knit_engines$delete(c("ui", "server"))
  knitr::opts_knit$set(unnamed.chunk.label = old_opts_knit)

  # return chunk details
  relabel_chunks(chunks)
}

restore_knitr_hooks <- function(chunk = NULL, eval = NULL) {
  if (is.null(chunk)) {
    knitr::knit_hooks$restore("chunk")
  } else {
    knitr::knit_hooks$set(chunk = chunk)
  }
  if (is.null(eval)) {
    knitr::opts_hooks$delete("eval")
  } else {
    knitr::opts_hooks$set(eval = eval)
  }
}

relabel_chunks <- function(chunks) {
  chunk_engines <- tolower(vapply(chunks, `[[`, character(1), "engine"))
  chunk_engines_uniq <- unique(chunk_engines)
  is_unnamed_chunk <- grepl("__unnamed__", names(chunks), fixed = TRUE)

  labels_new <- names(chunks)

  r_server_ui_chunks <- chunk_engines == "r" & labels_new %in% c("ui", "server")
  if (any(r_server_ui_chunks)) {
    named_server_ui_chunks <- labels_new[r_server_ui_chunks]
    stop(
      "Found r chunks named ", knitr::combine_words(named_server_ui_chunks),
      ". Please use ",
      knitr::combine_words(paste0("{", named_server_ui_chunks, "}"), and = " or "),
      " chunks instead."
    )
  }

  labels_new[is_unnamed_chunk] <- chunk_engines[is_unnamed_chunk]
  labels_new <- make.unique(c(chunk_engines_uniq, labels_new), sep = "")
  labels_new <- labels_new[-seq_along(chunk_engines_uniq)]
  labels_new <- sub("^(ui|server)1$", "\\1", labels_new)

  names(chunks) <- labels_new

  # rewrite label in $options as well
  for (i in seq_along(chunks)) {
    chunks[[i]]$options$label <- names(chunks)[i]
  }

  chunks
}
