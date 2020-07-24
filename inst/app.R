library(shiny)
library(shinyComponents)

ex <- ShinyComponent$new("../inst/example-component.Rmd")

ui <- fluidPage(
  fluidRow(
    column(4, ex$ui(id = "one", message = "This is cool")),
    column(4, ex$ui(id = "two", message = "I like it")),
    column(4, ex$ui(message = "I wonder about this")),
  ),
  ex$assets()
)

server <- function(input, output, session) {
  ex$server(id = "one")
  two <- ex$server(id = "two")

  # not a module, still works (can't have more than one, though.)
  three <- ex$server()

  observe({
    message(str(list(
      one = input[["one-number"]],
      two = two(),
      three = three()
    )))
  })
}

shinyApp(ui, server)
