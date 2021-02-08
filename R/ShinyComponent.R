#' Create A Shiny Component from an R Markdown Document
#'
#' @import shiny
#' @export
ShinyComponent <- R6::R6Class(
  "ShinyComponent",
  public = list(
    ui = "<list>",
    server = "<list>",
    dependencies = "<list>",
    initialize = function(file) {
      knit_result <- read_knitr_chunks(file, new.env(parent = baseenv()))

      private$global <- knit_result$envir
      private$chunks <- knit_result$chunks
      private$check_chunks()

      # prepare dependencies, if any
      self$dependencies <- prep_html_dependencies(
        extract_yaml(file)$dependencies,
        envir = private$global
      )

      # prepare ui and server elements
      ui_chunks <- private$get_chunk_names_by_engine("ui")
      self$ui <-
        if (identical(ui_chunks, c(ui = "ui"))) {
          # a single ui chunk creates $ui() method
          private$ui_factory("ui")
        } else {
          lapply(ui_chunks, private$ui_factory)
        }

      server_chunks <- private$get_chunk_names_by_engine("server")
      self$server <-
        if (identical(server_chunks, c(server = "server"))) {
          private$server_factory("server")
        } else {
          lapply(server_chunks, private$server_factory)
        }
    },
    assets = function() {
      css <- private$get_code_from_chunks_by_engine("css")
      js <- private$get_code_from_chunks_by_engine("js")
      sass <- private$prepare_sass()
      shiny::tagList(
        tag_css_style(sass),
        tag_css_style(css),
        tag_js_script(js)
      )
    },
    app = function(..., id = NULL, .ui = list(), .server = list()) {
      ...demo <- TRUE
      stopifnot(
        "Requires a ui chunk named 'ui'" =
          "ui" %in% private$get_chunk_names_by_engine("ui"),
        "Requires a server chunk named 'server'" =
          "server" %in% private$get_chunk_names_by_engine("server")
      )
      ui_fn <- if (rlang::is_function(self$ui)) self$ui else self$ui$ui
      server_fn <- if (rlang::is_function(self$server)) self$server else self$server$server

      shiny::shinyApp(
        ui = shiny::fluidPage(
          eval(rlang::call2(ui_fn, id = id, !!!.ui))
        ),
        server = function(input, output, session) {
          eval(rlang::call2(server_fn, id = id, !!!.server))
        },
        ...
      )
    }
  ),
  private = list(
    chunks = "<chunks from rmd>",
    global = "<envir from knitting rmd>",
    check_chunks = function() {
      if (length(private$get_chunks_by_engine("ui")) < 1) {
        stop("Needs at least one `ui` chunk")
      }
      if (length(private$get_chunks_by_engine("server")) < 1) {
        stop("Needs at least one `server` chunk")
      }
      reserved_method_names <- c("initialize", "clone", "app", "assets")
      ui_chunk_names <- names(private$get_chunks_by_engine("ui"))
      ui_bad <- intersect(ui_chunk_names, reserved_method_names)
      if (length(ui_bad)) {
        stop("ui chunks cannot be named ", paste0("'", ui_bad, "'", collapse = ", "))
      }

      server_chunk_names <- names(private$get_chunks_by_engine("server"))
      server_bad <- intersect(server_chunk_names, reserved_method_names)
      if (length(server_bad)) {
        stop("server chunks cannot be named ", paste0("'", server_bad, "'", collapse = ", "))
      }

      invisible(TRUE)
    },
    ui_factory = function(component) {
      stopifnot(component %in% names(private$chunks))
      chunk <- private$chunks[[component]]

      fn_args <- if (
        "..." %in% names(chunk$chunk_opts)
      ) {
        chunk$chunk_opts[["..."]]
      }

      fn <- function(..., id = NULL) {
        # prepare environment
        call_env <- rlang::env_clone(private$global, parent = parent.frame())
        call_env[["ns"]] <- shiny::NS(id)
        call_env[["self"]] <- self

        # prepare arguments in current call
        # start with args defined in chunk, ensuring id is present
        fn_args <- shallow_update_list(
          eval(fn_args, rlang::env_clone(private$global)),
          list(id = NULL)
        )
        # then merge with args in the current call
        args <- shallow_update_list(fn_args, as.list(match.call())[-1])
        # and take out dots, which are handled separately (TODO: remove?)
        args <- args[setdiff(names(args), c("..."))]
        dots <- rlang::list2(...)
        if (length(dots)) {
          if (is.null(names(dots)) || !all(nzchar(names(dots)))) {
            stop("All ... arguments to ShinyComponent ui() method must be named")
          }
        }
        args <- c(args, dots)
        if (length(args)) {
          mapply(names(args), args, FUN = function(name, val) {
            call_env[[name]] <- val
          })
        }

        .tagList <- chunk$chunk_opts$.tagList %||% "FALSE"
        ui_as_taglist <- eval(parse(text = .tagList), call_env)
        ui_elements <- if (isTRUE(ui_as_taglist)) {
          rlang::call2(htmltools::tagList, !!!private$parse_text_exprs(chunk$chunk))
        } else {
          parse(text = paste(chunk$chunk, collapse = "\n"))
        }
        ui_out <- eval(ui_elements, envir = call_env)
        if (component == "ui") {
          htmltools::tagList(ui_out, self$assets(), self$dependencies)
        } else {
          ui_out
        }
      }

      if (is.null(fn_args)) return(fn)
      rlang::fn_fmls(fn) <- rlang::pairlist2(
        !!!eval(fn_args, rlang::env_clone(private$global)),
        ... =,
        id = NULL
      )
      fn
    },
    server_factory = function(component = "server") {
      stopifnot(
        "not an available component" = component %in% names(private$chunks),
        "not a server component" = private$chunks[[component]]$engine == "server"
      )

      chunk <- private$chunks[[component]]
      chunk_code <- chunk$chunk

      fn_args <- if (
        "..." %in% names(chunk$chunk_opts)
      ) {
        chunk$chunk_opts[["..."]]
      }

      fn <- function(..., id = NULL) {
        # prepare arguments
        # start with args defined in chunk, ensuring id is present
        fn_args <- shallow_update_list(
          eval(fn_args, rlang::env_clone(private$global)),
          list(id = NULL)
        )
        # then merge with args in the current call
        args <- shallow_update_list(fn_args, as.list(match.call())[-1])
        # and take out dots, which are handled separately (TODO: remove?)
        args <- args[setdiff(names(args), c("..."))]
        dots <- rlang::list2(...)
        if (length(dots)) {
          if (is.null(names(dots)) || !all(nzchar(names(dots)))) {
            stop("All ... arguments to ShinyComponent server() method must be named")
          }
        }
        args <- c(args, dots)

        call_env <- rlang::env_clone(private$global, parent = parent.frame())
        call_env[["self"]] <- self

        # if `id` was provided, the server chunk becomes a module,
        # otherwise it gets evaluated as a regular server chunk
        if (!is.null(id)) {
          module_fn <- rlang::new_function(
            rlang::pairlist2(input=, output=, session=, !!!args),
            private$parse_text_body(chunk_code),
            call_env
          )
          callMod <- rlang::call2(shiny::callModule, module_fn, id = id, !!!args)
          eval(callMod, envir = call_env)
        } else {
          mapply(names(args), args, FUN = function(name, val) {
            call_env[[name]] <- val
          })
          eval(parse(text = chunk_code), envir = call_env)
        }
      }

      if (is.null(fn_args)) return(fn)
      rlang::fn_fmls(fn) <- rlang::pairlist2(
        !!!eval(fn_args, rlang::env_clone(private$global)),
        ... =,
        id = NULL
      )
      fn
    },
    get_code_from_chunks_by_engine = function(engine = "css") {
      Reduce(x = private$chunks, function(acc, item) {
        if (identical(tolower(item$engine), engine)) {
          acc <- c(acc, item$chunk)
        }
        acc
      }, init = c())
    },
    get_chunks_by_engine = function(engine = "r") {
      is_engine <- vapply(private$chunks, FUN.VALUE = logical(1), function(x) {
        identical(tolower(x$engine), tolower(engine))
      })
      private$chunks[is_engine]
    },
    get_chunk_names_by_engine = function(engine = "r") {
      chunks <- private$get_chunks_by_engine(engine)
      names(chunks) <- chunks <- names(chunks)
      chunks
    },
    parse_text_exprs = function(code) {
      rlang::parse_exprs(paste(code, collapse = "\n"))
    },
    parse_text_body = function(code) {
      rlang::parse_expr(paste0("{", paste(code, collapse = "\n"), "}"))
    },
    prepare_sass = function(...) {
      sass_code <- private$get_code_from_chunks_by_engine("sass")
      if (is.null(sass_code) || !length(sass_code)) return()
      sass::sass(sass_code, ...)
    }
  )
)
