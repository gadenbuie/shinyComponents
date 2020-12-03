#' Create A Shiny Component from an R Markdown Document
#'
#' @export
ShinyComponent <- R6::R6Class(
  "ShinyComponent",
  public = list(
    chunks = "<chunks from rmd>",
    global = new.env(),
    initialize = function(file) {
      self$chunks <- private$get_chunks(file)
      names(self$chunks) <- tolower(names(self$chunks))
      private$check_chunks()

      r_chunks <- private$get_chunks_by_engine("r")
      r_chunks <- r_chunks[setdiff(names(r_chunks), c("ui", "server"))]
      if (length(r_chunks)) {
        for (r_chunk in r_chunks) {
          if (!is.null(r_chunk$chunk_opts$eval) && !isTRUE(r_chunk$chunk_opts$eval)) {
            next
          }
          chunk_code <- paste(r_chunk$chunk, collapse = "\n")
          if (!grepl("[^\\s]", chunk_code)) next
          exprs <- rlang::parse_exprs(chunk_code)
          if (!length(exprs)) next
          for (expr in exprs) {
            rlang::eval_bare(expr, self$global)
          }
        }
      }
    },
    ui = function(..., id = NULL) {
      ui_elements <- private$parse_text_exprs(self$chunks$ui$chunk)
      ui_parent <- rlang::call2(htmltools::tagList, !!!ui_elements)
      call_env <- rlang::env_clone(self$global, parent = parent.frame())
      call_env[["ns"]] <- shiny::NS(id)
      args <- rlang::list2(...)
      if (rlang::has_length(args)) {
        if (is.null(names(args)) || !all(nzchar(names(args)))) {
          stop("All ... arguments to ShinyComponent ui() method must be named")
        }
        mapply(names(args), args, FUN = function(name, val) {
          call_env[[name]] <- val
        })
      }
      rlang::eval_bare(ui_parent, env = call_env)
    },
    server = function(..., id = NULL) {
      call_env <- rlang::env_clone(self$global, parent = parent.frame())
      args <- rlang::list2(...)

      # if `id` was provided, the server chunk becomes a module,
      # otherwise it gets evaluated as a regular server chunk
      if (!is.null(id)) {
        module_fn <- rlang::new_function(
          rlang::pairlist2(input=, output=, session=),
          private$parse_text_body(self$chunks$server$chunk),
          call_env
        )
        shiny::callModule(module = module_fn, id = id, ...)
      } else {
        if (
          rlang::has_length(args) &&
          (is.null(names(args)) || !all(nzchar(names(args))))
        ) {
          stop("All ... arguments to ShinyComponent server() method must be named")
        }
        mapply(names(args), args, FUN = function(name, val) {
          call_env[[name]] <- val
        })
        eval(parse(text = self$chunks$server$chunk), envir = call_env)
      }
    },
    assets = function() {
      css <- private$get_code_from_chunks_by_engine("css")
      js <- private$get_code_from_chunks_by_engine("js")
      shiny::tagList(
        tag_css_style(css),
        tag_js_script(js)
      )
    },
    app = function(...) {
      shiny::shinyApp(
        ui = shiny::fluidPage(
          self$ui(),
          self$assets()
        ),
        server = function(input, output, server) {
          self$server()
        },
        ...
      )
    }
  ),
  private = list(
    get_chunks = function(file, chunks) {
      chunks <- list()

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
      private$restore_knitr_hooks(old_chunk, old_eval)

      # return chunk details
      chunks
    },
    check_chunks = function() {
      if (!"ui" %in% names(self$chunks)) {
        stop("Needs `ui` chunk")
      }
      if (!"server" %in% names(self$chunks)) {
        stop("Needs `server` chunk")
      }
      for (component in c("ui", "server")) {
        if (sum(names(self$chunks) == component) > 1) {
          stop("Only one case-insensitive `", component, "` chunk can be present")
        }
        if (tolower(self$chunks[[component]]$engine) != "r") {
          stop("The `", component, "` chunk must be R code")
        }
      }
    },
    restore_knitr_hooks = function(chunk = NULL, eval = NULL) {
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
    },
    get_code_from_chunks_by_engine = function(engine = "css") {
      Reduce(x = self$chunks, function(acc, item) {
        if (identical(tolower(item$engine), engine)) {
          acc <- c(acc, item$chunk)
        }
        acc
      }, init = c())
    },
    get_chunks_by_engine = function(engine = "r") {
      is_engine <- vapply(self$chunks, FUN.VALUE = logical(1), function(x) {
        identical(tolower(x$engine), tolower(engine))
      })
      self$chunks[is_engine]
    },
    parse_text_exprs = function(code) {
      rlang::parse_exprs(paste(code, collapse = "\n"))
    },
    parse_text_body = function(code) {
      rlang::parse_expr(paste0("{", paste(code, collapse = "\n"), "}"))
    }
  )
)
