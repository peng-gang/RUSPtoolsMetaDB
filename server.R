library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- rnorm(1000)
    
    # draw the histogram with the specified number of bins
    hist(x, col = 'darkgray', border = 'white')
    
  })
  
})
