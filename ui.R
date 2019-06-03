library(shiny)

load("data/RUSP.SN.RData")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/style.css"),
  
  # Application title
  titlePanel("RUSPtools",
             title = h1("RUSPtools: Newborn Metabolic Screening Database")),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Show", fluid = TRUE, value = "show",
          tabsetPanel(
            type = "tabs",
            id = "tabShow",
            tabPanel(
              "Single",
              value = "single",
              tags$div(
                title = "Select an anallyte to show",
                selectInput(
                  "analytes",
                  label = h4("Analytes"),
                  choices = meta.name,
                  selected = "C3"
                )
              )
            ),
            tabPanel(
              "Ratio",
              value = "ratio",
              tags$div(
                title = "Select a numerator and a denominator",
                selectInput(
                  "numerator",
                  label = h4("Numerator"),
                  choices = meta.name,
                  multiple = TRUE,
                  selected = "C3"
                ),
                selectInput(
                  "denominator",
                  label = h4("Denominator"),
                  choices = meta.name,
                  multiple = TRUE,
                  selected = "C2"
                )
              )
            )
          ),
          hr(),
          
          tags$div(
            title = "Specify the race/ethnicity to show",
            selectInput(
              "race",
              label = h4("Race/Ethnicity"),
              choices = list(
                "All" = 1,
                "Asian" = 2,
                "Black" = 3,
                "Hispanic" = 4,
                "White" = 5
              ),
              selected = 1
            )
          ),
          
          hr(),
          tags$div(
            title = "Specify the sex to show",
            selectInput(
              "sex",
              label = h4("Sex"),
              choices = list(
                "Both" = 1,
                "Male" = 2,
                "Female" = 3
              ),
              selected = 1
            )
          ),
          
          hr(),
          tags$div(
            title = "Select a range of age at blood collection to show",
            selectInput(
              "aac",
              label = h4("Age at blood collection"),
              choices = list(
                "All" = 1,
                "12-24 hours" = 2,
                "24-48 hours" = 3,
                "48-168 hours" = 4
              ),
              selected = 3
            ),
            
            hr(),
            tags$div(
              title = "Select TPN option",
              selectInput(
                "tpn",
                label = h4("Total Parenteral Nutrition"),
                choices = list(
                  "All" = 1,
                  "No TPN" = 2,
                  "TPN" = 3
                ),
                selected = 2,
              )
            )
          )),
        
        tabPanel(
          "Compare", fluid = TRUE, 
          value = "compare",
          tabsetPanel(
            id = "tabCompare",
            tabPanel(
              "Single",
              value = "singleC",
              tags$div(
                title = "Select an anallyte to show",
                selectInput(
                  "analytesC",
                  label = h4("Analytes"),
                  choices = meta.name,
                  selected = "C3"
                )
              )
            ),
            tabPanel(
              "Ratio",
              value = "ratioC",
              tags$div(
                title = "Select a numerator and a denominator",
                selectInput(
                  "numeratorC",
                  label = h4("Numerator"),
                  choices = meta.name,
                  multiple = TRUE,
                  selected = "C3"
                ),
                selectInput(
                  "denominatorC",
                  label = h4("Denominator"),
                  choices = meta.name,
                  multiple = TRUE,
                  selected = "C2"
                )
              )
            )),
          
          hr(),
          
          fluidPage(
            column(
              6,
              h3("Group A"),
              
              tags$div(
                title = "Specify the race/ethnicity to show",
                selectInput(
                  "raceA",
                  label = h4("Race/Ethnicity"),
                  choices = list(
                    "All" = 1,
                    "Asian" = 2,
                    "Black" = 3,
                    "Hispanic" = 4,
                    "White" = 5
                  ),
                  selected = 4
                )
              ),
              
              tags$div(
                title = "Specify the sex to show",
                selectInput(
                  "sexA",
                  label = h4("Sex"),
                  choices = list(
                    "Both" = 1,
                    "Male" = 2,
                    "Female" = 3
                  ),
                  selected = 1
                )
              ),
              
              tags$div(
                title = "Select a range of age at blood collection to show",
                selectInput(
                  "aacA",
                  label = h4("Age at blood collection"),
                  choices = list(
                    "All" = 1,
                    "12-24 hours" = 2,
                    "24-48 hours" = 3,
                    "48-168 hours" = 4
                  ),
                  selected = 3)
                ),
                
                tags$div(
                  title = "Select TPN option",
                  selectInput(
                    "tpnA",
                    label = h4("Total Parenteral Nutrition"),
                    choices = list(
                      "All" = 1,
                      "No TPN" = 2,
                      "TPN" = 3
                    ),
                    selected = 2,
                  )
                )
            ),
            column(
              6,
              h3("Group B"),
              
              tags$div(
                title = "Specify the race/ethnicity to show",
                selectInput(
                  "raceB",
                  label = h4("Race/Ethnicity"),
                  choices = list(
                    "All" = 1,
                    "Asian" = 2,
                    "Black" = 3,
                    "Hispanic" = 4,
                    "White" = 5
                  ),
                  selected = 3
                )
              ),
              
              tags$div(
                title = "Specify the sex to show",
                selectInput(
                  "sexB",
                  label = h4("Sex"),
                  choices = list(
                    "Both" = 1,
                    "Male" = 2,
                    "Female" = 3
                  ),
                  selected = 1
                )
              ),
              
              tags$div(
                title = "Select a range of age at blood collection to show",
                selectInput(
                  "aacB",
                  label = h4("Age at blood collection"),
                  choices = list(
                    "All" = 1,
                    "12-24 hours" = 2,
                    "24-48 hours" = 3,
                    "48-168 hours" = 4
                  ),
                  selected = 3)
              ),
              
              tags$div(
                title = "Select TPN option",
                selectInput(
                  "tpnB",
                  label = h4("Total Parenteral Nutrition"),
                  choices = list(
                    "All" = 1,
                    "No TPN" = 2,
                    "TPN" = 3
                  ),
                  selected = 2,
                )
              )
            )
          )),
        
        
        tabPanel(
          "About", fluid = TRUE,
          value = "about")
      ), width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
))