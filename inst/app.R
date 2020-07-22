library(shiny)
library(shinyComponents)

ex <- ShinyComponent$new("../inst/example-component.Rmd")

ui <- fluidPage(
  ex$ui("one", message = "This is cool"),
  ex$ui("two", message = "I like it"),
  ex$assets()
)

server <- function(input, output, session) {
  ex$server("one")
  two <- ex$server("two")

  observe({
    message("The value of 'two' is ", two())
  })
}

shinyApp(ui, server)
