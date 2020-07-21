
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyComponents

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of shinyComponents is to turn R Markdown documents in
self-contained Shiny components. Like [web
components](https://developer.mozilla.org/en-US/docs/Web/Web_Components)
meets [Shiny modules](https://shiny.rstudio.com/articles/modules.html)
(without that *ShadowDOM* business).

## Installation

You can install the released version of shinyComponents from Github.

``` r
# install.packages("remotes")
remotes::install_github("gadenbuie/shinyComponents")
```

## Example

Writing a Shiny component often requires wiring together a number of
different languages: R for the Shiny UI and Server pieces, CSS for
styling, JavaScript for interactivity, and more R for data and other
things.

Imagine you’ve written out all of those pieces in a R Markdown file,
like the one in the details section below (click to expand).

<details>

<summary>Example R Markdown:
<code>example-component.Rmd</code></summary>

```` markdown
---
title: "Example Component"
---

```{r}
library(shiny)
initial_header <- "Just a simple demo"
```

```{r ui}
h3(initial_header)
sliderInput("number", "Pick a number", min = 0, max = 10, value = 1)
verbatimTextOutput("debug")
```

```{r server}
output$debug <- renderPrint(input$number)
```

```{css}
h3 { color: #aaa; }
```

```{js}
setTimeout(function() {
  let h3 = document.querySelector('h3')
  h3.textContent = 'Just an awesome demo'
  h3.style.color = 'red'
}, 3000)
```
````

</details>

To read the component from the R Markdown, load `shinyComponents` and
create a new `ShinyComponent`.

``` r
library(shinyComponents)

ex_rmd <- system.file("example-component.Rmd", package = "shinyComponents")

ex <- ShinyComponent$new(ex_rmd)
```

The chunk named `ui` becomes available in the `ui()` method, making a
chunk like this

```` markdown
```{r ui}
h3(initial_header)
sliderInput("number", "Pick a number", min = 0, max = 10, value = 1)
verbatimTextOutput("debug")
```
````

available in the `$ui()` method.

``` r
ex$ui()
```

    #> <h3>Just a simple demo</h3>
    #> <div class="form-group shiny-input-container">
    #>   <label class="control-label" for="number">Pick a number</label>
    #>   <input class="js-range-slider" id="number" data-min="0" data-max="10" data-from="1" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
    #> </div>
    #> <pre id="debug" class="shiny-text-output noplaceholder"></pre>

The `server` chunk is available in the server method `$server()`, to be
called anywhere in your server function. Any CSS or JavaScript chunks
are concatenated and included in appropriate `<style>` and `<script>`
tags when you call the `$assets()` method.

Here’s a bare bone Shiny app incorporating all of the pieces.

``` r
library(shiny)

ex <- ShinyComponent$new(ex_rmd)

ui <- fluidPage(
  ex$ui(),
  ex$assets()
)

server <- function(input, output, session) {
  ex$server()
}

shinyApp(ui, server)
```

Or you can run a demo app that pulls the components together into an app
just like the one above.

``` r
ex$app()
```
