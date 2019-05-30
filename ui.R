library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("RUSPtools",
             title = h1("RUSPtools: Newborn Metabolic Screening Database")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Show", fluid = TRUE),
        tabPanel("Compare", fluid = TRUE),
        tabPanel("About", fluid = TRUE)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))