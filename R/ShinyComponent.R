#' Create A Shiny Component from an R Markdown Document
#'
#' @export
ShinyComponent <- R6::R6Class(
  "ShinyComponent",
  public = list(
    ui = "<list>",
    server = "<list>",
    initialize = function(file) {
      private$chunks <- read_knitr_chunks(file)
      names(private$chunks) <- tolower(names(private$chunks))
      private$check_chunks()

      # evaluate standard R chunks into the global environment
      r_chunks <- private$get_chunks_by_engine("r")
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
            rlang::eval_bare(expr, private$global)
          }
        }
      }

      # prepare ui and server elements
      self$ui <- lapply(
        private$get_chunk_names_by_engine("ui"),
        private$ui_factory
      )

      self$server <- lapply(
        private$get_chunk_names_by_engine("server"),
        private$server_factory
      )
    },
    assets = function() {
      css <- private$get_code_from_chunks_by_engine("css")
      js <- private$get_code_from_chunks_by_engine("js")
      shiny::tagList(
        if (length(css)) htmltools::tags$style(paste(css, collapse = "\n")),
        if (length(js)) htmltools::tags$script(paste(js, collapse = "\n"))
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
    chunks = "<chunks from rmd>",
    global = new.env(),
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
      ui_elements <- private$parse_text_exprs(chunk$chunk)


      function(..., id = NULL) {
        # prepare environment
        call_env <- rlang::env_clone(private$global, parent = parent.frame())
        call_env[["ns"]] <- shiny::NS(id)

        # prepare arguments
        args <- rlang::list2(...)
        if (length(args)) {
          if (is.null(names(args)) || !all(nzchar(names(args)))) {
            stop("All ... arguments to ShinyComponent ui() method must be named")
          }
          mapply(names(args), args, FUN = function(name, val) {
            call_env[[name]] <- val
          })
        }

        .tagList <- chunk$chunk_opts$.tagList %||% "FALSE"
        ui_as_taglist <- eval(parse(text = .tagList), call_env)
        if (isTRUE(ui_as_taglist)) {
          ui_elements <- rlang::call2(htmltools::tagList, !!!ui_elements)
        }
        rlang::eval_bare(ui_elements, env = call_env)
      }
    },
    server_factory = function(component = "server") {
      stopifnot(
        "not an available component" = component %in% names(private$chunks),
        "not a server component" = private$chunks[[component]]$engine == "server"
      )

      chunk <- private$chunks[[component]]$chunk

      function(..., id = NULL) {
        args <- rlang::list2(...)
        call_env <- rlang::env_clone(private$global, parent = parent.frame())

        # if `id` was provided, the server chunk becomes a module,
        # otherwise it gets evaluated as a regular server chunk
        if (!is.null(id)) {
          module_fn <- rlang::new_function(
            rlang::pairlist2(input=, output=, session=),
            private$parse_text_body(chunk),
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
          eval(parse(text = chunk), envir = call_env)
        }
      }
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
    }
  )
)
