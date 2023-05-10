

# RollDiceTester

rm(list = ls()) # clears environment 
cat("\014") # clears console

library(shiny)

ui <- fluidPage(
  
  h1( "Try to roll the same numbers" , align = "left"),
  
  # h1(actionButton("dotheaction",h4("Roll the dice!"))),
  # h1(textOutput("dice1") ) ,
  # h1(textOutput("dice2") ) ,
  
  fluidRow(
    column(1,     ),
    column(2,
           h1(actionButton("dotheaction",h4("Roll the dice!")))
    ),
    column(1,  h1(textOutput("dice1") ) ),
    column(1,  h1(textOutput("dice2") ) ),
    column(7, )
  ),
  
  # h3(textOutput("value") ),
  # h3(textOutput("valuelist") ),
  # h3(textOutput("lenlist") ),
  
  # h3(textOutput("message") ),
  
  h3(htmlOutput("message1") ), 
  
  h5(span(textOutput("message2"), style="color:transparent") ),
  
  h3(textOutput("message3") )
  
  
)

server <- function(input, output, session) {
  dicethrow <- eventReactive( input$dotheaction , {
    sample(1:6, 2, replace=TRUE)
  })
  output$dice1 <- renderText({   dicethrow()[1]    })
  output$dice2 <- renderText({   dicethrow()[2]    })
  
  vals <- reactiveValues(count=0)
  observeEvent( input$dotheaction, 
                if ( dicethrow()[1]!=dicethrow()[2] ) {
                  vals$count <- vals$count + 1  } else {
                    vals$count <- 0 
                  }
  )
  output$value <- renderText( vals$count  )
  
  observeEvent( input$dotheaction,
                vals$container <- c( vals$container, vals$count)
  )
  
  output$valuelist <- renderText( vals$container  )
  
  output$lenlist <- renderText( length(vals$container)  )
  
  # output$message <- renderText({   
  #   if ( (dicethrow()[1]==dicethrow()[2]) ) {
  #     return( paste0("Bingo - You Win!") )
  #   } else {
  #     return(  paste0("Unlucky, try again") )
  #   }
  # })
  
  output$message1 <- renderText(   
    if ( (dicethrow()[1]==dicethrow()[2]) ) {
      return( paste0("<span style=\"color:red;font-size:30px;font-weight:bold\">Bingo - You Win!</span>") )
    } else {
      return(  paste0("Unlucky, try again") )
    }
  )
  
  output$message2 <- renderText({"coloured text made transparent for row filler"})
  
  output$message3 <- renderText(  
    
    if ( length(vals$container)==1) {
      if (dicethrow()[1]==dicethrow()[2] ) {
        return( paste0("It took 1 roll to win"  ) )
      } else {
        return(  paste0("You are on roll ", vals$count  ))  
      }
    } else if (length(vals$container)>1 ) {
      if ( vals$container[(length(vals$container)-1)]==0 & vals$container[length(vals$container)]==0 ) {
        return( paste0("It took 1 roll to win"  ) )
      } else if ( dicethrow()[1]==dicethrow()[2] ) {
        return( paste0("It took " ,
                       vals$container[(length(vals$container)-1)]+1,
                       " rolls to win") )
      } else {
        return(  paste0("You are on roll " ,
                        vals$count
        )   )
      }
    }
  )
}

shinyApp(ui, server)

