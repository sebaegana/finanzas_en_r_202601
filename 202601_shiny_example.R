library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .calc-btn { width: 100%; height: 60px; font-size: 20px; }
      .display  { font-size: 28px; text-align: right; padding: 10px; }
      .btn-row  { margin-bottom: 10px; }
    "))
  ),
  
  titlePanel("Calculadora (Shiny)"),
  
  fluidRow(
    column(
      4,
      div(class = "display well", textOutput("display"))
    )
  ),
  
  fluidRow(
    column(4,
           div(class="btn-row",
               column(3, actionButton("b7", "7", class="calc-btn")),
               column(3, actionButton("b8", "8", class="calc-btn")),
               column(3, actionButton("b9", "9", class="calc-btn")),
               column(3, actionButton("bdiv", "/", class="calc-btn"))
           ),
           div(class="btn-row",
               column(3, actionButton("b4", "4", class="calc-btn")),
               column(3, actionButton("b5", "5", class="calc-btn")),
               column(3, actionButton("b6", "6", class="calc-btn")),
               column(3, actionButton("bmul", "*", class="calc-btn"))
           ),
           div(class="btn-row",
               column(3, actionButton("b1", "1", class="calc-btn")),
               column(3, actionButton("b2", "2", class="calc-btn")),
               column(3, actionButton("b3", "3", class="calc-btn")),
               column(3, actionButton("bsub", "-", class="calc-btn"))
           ),
           div(class="btn-row",
               column(3, actionButton("b0", "0", class="calc-btn")),
               column(3, actionButton("bdot", ".", class="calc-btn")),
               column(3, actionButton("beq", "=", class="calc-btn")),
               column(3, actionButton("badd", "+", class="calc-btn"))
           ),
           div(class="btn-row",
               column(6, actionButton("bC", "C", class="calc-btn")),
               column(6, actionButton("bdel", "DEL", class="calc-btn"))
           )
    )
  )
)

server <- function(input, output, session) {
  
  expr <- reactiveVal("")
  
  append_token <- function(tok) {
    expr(paste0(expr(), tok))
  }
  
  # botones numéricos
  observeEvent(input$b0,  append_token("0"))
  observeEvent(input$b1,  append_token("1"))
  observeEvent(input$b2,  append_token("2"))
  observeEvent(input$b3,  append_token("3"))
  observeEvent(input$b4,  append_token("4"))
  observeEvent(input$b5,  append_token("5"))
  observeEvent(input$b6,  append_token("6"))
  observeEvent(input$b7,  append_token("7"))
  observeEvent(input$b8,  append_token("8"))
  observeEvent(input$b9,  append_token("9"))
  observeEvent(input$bdot, append_token("."))
  
  # operadores
  observeEvent(input$badd, append_token("+"))
  observeEvent(input$bsub, append_token("-"))
  observeEvent(input$bmul, append_token("*"))
  observeEvent(input$bdiv, append_token("/"))
  
  # limpiar
  observeEvent(input$bC, {
    expr("")
  })
  
  # borrar último caracter
  observeEvent(input$bdel, {
    x <- expr()
    if (nchar(x) > 0) expr(substr(x, 1, nchar(x) - 1))
  })
  
  # calcular
  observeEvent(input$beq, {
    x <- expr()
    if (nchar(x) == 0) return()
    
    # Evaluación con manejo de errores
    res <- tryCatch({
      val <- eval(parse(text = x))
      if (is.infinite(val) || is.nan(val)) "Error" else as.character(val)
    }, error = function(e) "Error")
    
    expr(res)
  })
  
  output$display <- renderText({
    x <- expr()
    if (x == "") "0" else x
  })
}

shinyApp(ui, server)
