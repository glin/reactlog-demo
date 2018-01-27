library(shiny)
options(shiny.reactlog = TRUE)

ui <- fluidPage(
  column(4, wellPanel(
    sliderInput("A", "A", 0, 100, 0),
    sliderInput("B", "B", 0, 100, 0),
    actionButton("resetSlidersBtn", "Reset sliders"),
    actionButton("incrementBtn", "Increment")
  )),

  column(8,
         uiOutput("data"),
         uiOutput("debugInfo")
  )
)

server <- function(input, output, session) {
  counter <- reactiveVal(0)
  valA <- reactiveVal(0)
  valB <- reactiveVal(0)

  sumAB <- reactive({ valA() + valB() })

  observe({ valA(input$A) })
  observe({ valB(input$B) })

  observeEvent(input$incrementBtn, {
    counter(isolate(counter()) + 1)
  })

  observeEvent(input$resetSlidersBtn, {
    updateSliderInput(session, "A", value = 0)
    updateSliderInput(session, "B", value = 0)
  })

  data <- reactive({
    paste0(
      "sumAB: ", sumAB(), "\n",
      "counter: ", counter()
    )
  })

  output$data <- renderUI({
    div(
      h4("output$data"),
      tags$pre(data())
    )
  })

  output$debugInfo <- renderUI({
    data()
    stacktrace <- reactlog::traceInvalidation()
    dependencies <- reactlog::listDependencies()

    div(
      h4("output$debugInfo"),

      code("reactlog::traceInvalidation()"),
      pre(ansiToHtml(format(stacktrace))),

      code("reactlog::listDependencies()"),
      pre(ansiToHtml(format(dependencies)))
    )
  })
}

ansiToHtml <- function(str) {
  HTML(ansistrings::ansi_to_html(str, fullpage = FALSE))
}

shinyApp(ui, server)
