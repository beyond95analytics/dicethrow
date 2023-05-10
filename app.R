

rm(list = ls()) # clears environment 
cat("\014") # clears console

library(shiny)

ui <- fluidPage(
  
  titlePanel(
    
    h2( "Try to roll the same numbers" , align = "center"),
    
  ),
  
  h1(span(textOutput("message1"), style="color:transparent") ),
  # h1(span(textOutput("message2"), style="color:transparent") ),
  
  fluidRow(
    column(4,     ),
    column(2,
           h1(actionButton("dotheaction", "Roll the dice!"))
    ),
    column(1,  h1(textOutput("dice1") ) ),
    column(1,  h1(textOutput("dice2") ) ),
    column(4, )
  ),
  fluidRow(
    column(6,     ),
    column(3, htmlOutput("message") ),
    column(3,     )
  )
)

server <- function(input, output, session) {
  
  dicethrow <- eventReactive( input$dotheaction , {
    sample(1:6, 2, replace=TRUE)
  })
  
  output$dice1 <- renderText({   dicethrow()[1]    })
  output$dice2 <- renderText({   dicethrow()[2]    })
  
  output$message <- renderText({   
    
    # if ( (dicethrow()[1]==dicethrow()[2]) ) {
    #   return( paste0("Bingo, You Win!") )
    # } else {
    #   return(  paste0("Unlucky, try Again") )
    # }
    
    if ( (dicethrow()[1]==dicethrow()[2]) ) {
      return( paste0("<span style=\"color:red;font-size:15px;font-weight:bold\">Bingo - You Win!</span>") )
    } else {
      return(  paste0("Unlucky, try Again") )
    }
    
  })
  
  output$message1 <- renderText({"coloured text made transparent for row filler"})
  # output$message2 <- renderText({"coloured text made transparent for row filler"})
  
}

shinyApp(ui, server)





