library(shiny)
library(DT)

ui <- fluidPage(
  theme = "bootstrap.css",
  hr(),
  titlePanel(title=div("DILIc: An App to classify Drug Induced Liver Injury Literature",h6("Developed & Maintained by Sanjay Rathee (Namshik Han Lab)")), windowTitle = ""),  
  hr(),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Upload Validation Cohort"),
        hr(),
        fileInput("file1", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        #fileInput("file2", "Upload Metadata CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        hr(),
        div(style="display: inline-block;vertical-align:top; width: 94%;",uiOutput("selectedcolumn")),
        div(style="display: inline-block;vertical-align:top; width: 94%;",actionButton("buttonValidate", "Validate Cohort")),
      ),
        
    ),
  
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Probability Table and Predictions",
                           # column(8, wellPanel(
                           h5("DataTable showing probabilities for a abstract to be DILI positive and negative."),
                           hr(),
                           DT::dataTableOutput('predictionprobs', width = "99%", height = "20em") %>% withSpinner(color="#0dc5c1"),
                           hr(),
                           #DT::dataTableOutput('predictionaccuracy', width = "99%", height = "10em") %>% withSpinner(color="#0dc5c1"),
                  )
      )
      
    ),
    
  )

)