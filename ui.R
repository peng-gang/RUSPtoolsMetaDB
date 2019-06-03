library(shiny)

load("data/RUSP.SN.RData")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/style.css"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Application title
  titlePanel("RUSPtools",
             title = h1("RUSPtools: Newborn Metabolic Screening Database")),
  
  sidebarLayout(
    sidebarPanel(
      # width = 3,
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
                  selected = 2
                )
              )
            )
          )),
        
        
        tabPanel(
          "About", fluid = TRUE,
          value = "about",
          h3("About RUSPtools"),
          p(
            "This web application shows the dynamic changes in newborn metabolism in relation to birth weight, 
            gestational age, sex, and race/ethnicity, and estimates the physiological variability in screening 
            markers for inborn metabolic disorders."
          ),
          h3("Instructions"),
          p(
            "Click ", code("show"), " to view the differences of analyte markers in relation to gestational age and birth weight. 
            The heatmap color code indicates the mean analyte level for each of 25 groups of newborns. 
            Values in each tile show the mean analyte value and the group size in parenthesis. 
            The line charts show the marginal trends of the selected analyte with respect to gestational age (top) 
            and increasing birth weight (right). Click drop-down menu for Analytes to select from 43 analytes, 
            or click Ratio to select a ratio of two markers. Click drop-down menus for Race/Ethnicity to view marker 
            levels associated with four race/ethnicity groups; for Sex to view marker levels associated with female and male infants; 
            and for Age at collection to view changes in marker levels related to the age of blood collection after birth. "
          ),
          p(
            "Click ", code("compare"), " for a pairwise comparison of analytes and analyte ratios for two sample groups. 
            Heatmaps for sample group A and B are in the same format as in the individual analyte browser. 
            The third heatmap shows the difference between the two sample groups in the format of cohen’s D 
            (sample size of group A, sample size of group B), where cohen’s D values indicate the significance 
            of the difference between the two groups."
          ),
          h3("Data"),
          p("Data from 100,000 screen-negative singleton infants born between 2013 and 15 were selected at random by 
            the ",
            a("California NBS program ",
              href = "https://www.cdph.ca.gov/Programs/CFH/DGDS/Pages/cbp/default.aspx",
              target = "_blank"),
            "that included metabolic analytes measured by MS/MS as well as birth weight, 
            gestational age, sex, race/ethnicity, and age at blood collection. The California Department of Public Health 
            is not responsible for the results or conclusions drawn by the authors of this publication."),
          h3("Code"),
          p(
            "Built with ",
            a("R",
              href = "https://www.r-project.org", target = "_blank"),
            "and the ",
            a("Shiny framework.",
              href = "http://shiny.rstudio.com", target = "_blank")
          )
      )
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  ),
  p("Copyright 2019 by ",
    a("Gang Peng ", 
      href = "https://publichealth.yale.edu/people/gang_peng-1.profile",
      target = "_blank"),
    "and ",
    a("Curt Scharfe.",
      href = "https://medicine.yale.edu/genetics/people/curt_scharfe-2.profile",
      target = "_blank")
  ),
  p("Report issues to the",
    a("developers.",
      href = "mailto:gang.peng@yale.edu")
  )
)
)