library(shiny)
library(bslib)

extremity <- c("🦵", "💪", "🧠")
side <- c("left", "right")
color <- c("black" = 1, "violet" = 1, "green" = 1, "blue" = 1, "grey" = 1, 
           "yellow" = 1,"pink" = 1, "orange" = 1, "white" = 1, "any color" = 0.5)

ui <- page_fillable(
  layout_columns(
    card( 
      actionButton(inputId = "button_roll", label = "Roll"),
      actionButton(inputId = "button_reset", label = "Reset roll")
    ), 
    card( 
      p("Next move"),
      htmlOutput(outputId = "render_next_move"),
      p("Previous move"),
      htmlOutput(outputId = "render_prev_move"),
      textOutput(outputId = "render_num_rolls")
    ),
    card(
      card_header("Legend"),
      p("🦵, 💪: leg and arm, respectively"),
      p("🧠: player chooses limb"),
      input_dark_mode(id = "mode")
    )
  ) 
)

server <- function(input, output) {
  db <- reactiveValues(roll_streak = 0, next_move = "", prev_move = "")
  
  observeEvent(eventExpr = input$button_roll, {
    db$roll_streak <- db$roll_streak + 1
    
    sp_extremity <- sample(x = extremity, size = 1)
    sp_color <- sample(x = names(color), size = 1, prob = color)
    sp_side <- sample(x = side, size = 1)
    
    db$prev_move <- db$next_move
    db$next_move <- paste(sp_side, sp_extremity, sp_color)
  })
  
  observeEvent(eventExpr = input$button_reset, {
    db$roll_streak = 0
    db$next_move = ""
    db$prev_move = ""
  })
  
  output$render_num_rolls <- renderText(expr = {
    paste("Number of rolls in a row:", db$roll_streak)
  })
  
  output$render_prev_move <- renderText(expr = {
    HTML(paste("<h6>", db$prev_move, "</h6>"))
  })
  
  output$render_next_move <- renderText(expr = {
    HTML(paste("<h1>", db$next_move, "</h1>"))
  })
}

shinyApp(ui = ui, server = server)