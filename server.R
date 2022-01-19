
server <- function(input, output, session) {
  
  ## Define max file size
  options(shiny.maxRequestSize=530*1024^2)
  #options(DT.options = list(pageLength = 15))
  options(DT.options = list(scrollX = TRUE))
  source('global.R', local = TRUE)
  
  ##  get dose information for DRUG 2
  validation_data <- reactive({
    shiny::validate(
      need(is.null(input$file1$datapath) != TRUE, "Please upload csv data for validation cohort")
    )
    expression_data <- read_tsv(file = input$file1$datapath)
    return(expression_data)
  })
  
  ## Make dynamic box in Ui to select only one record by ID
  output$selectedcolumn <- renderUI({
    pp2 <- validation_data()
    pp2 <- colnames(pp2)
    selectInput("selectedcolumng", "Select Column having Abstracts", choices = c("Select",pp2))
  })
  
  ## Find records for a drug name 1
  validate <- eventReactive(input$buttonValidate, ignoreNULL = TRUE, ignoreInit = FALSE, {
    if(input$selectedcolumng=="Select"){
      shiny::validate(
        need(input$selectedcolumng != "Select", "Please select balanced or full cohort to validate")
      )
    }else{
      selectedcol  <-  input$selectedcolumng
    }
    
    dili_validation_data <- validation_data()
    rm(result_validation)
    load("functions.RData")
    cl<-makeCluster(6)
    registerDoParallel(cl)
    result_validation <- foreach(i = 1:dim(dili_validation_data)[1], .combine = rbind) %dopar% {
      test_predictor_optimized(dili_validation_data[i,],c(2), positive_only_df, negative_only_df)
    }
    parallel::stopCluster(cl)
    print(table(result_validation$response))
    result_validation$Positive_Probability <- result_validation$pos_score/(result_validation$pos_score+result_validation$neg_score)
    result_validation$Negative_Probability <- result_validation$neg_score/(result_validation$pos_score+result_validation$neg_score)
    row.names(result_validation) <- c(1:dim(result_validation)[1])
    result_validation <- result_validation[, c((dim(dili_validation_data)[2]+1),(dim(dili_validation_data)[2]+4):dim(result_validation)[2])]
    return(list(result_validation))
  })

  # output$predictionprobs = DT::renderDataTable(DT::datatable(gse87211_validate()[[4]][,c(1:4)], escape=FALSE,
  #                                                  options = list(pageLength = 5, autoWidth = TRUE,
  #                                                   columnDefs = list(list( targets = c(4), width = '200px')),
  #                                                   scrollX = TRUE)))
  output$predictionprobs = DT::renderDataTable(DT::datatable({validate()[[1]][,]}
                                                             , extensions = 'Buttons'
                                                             , options = list( 
                                                               dom = "Blfrtip"
                                                               , buttons = 
                                                                 list("copy", list(
                                                                   extend = "collection"
                                                                   , buttons = c("csv", "excel", "pdf")
                                                                   , text = "Download"
                                                                 ) ) # end of buttons customization
                                                               
                                                               # customize the length menu
                                                               , lengthMenu = list( c(5,10, 20, -1) # declare values
                                                                                    , c(5,10, 20, "All") # declare titles
                                                               ) # end of lengthMenu customization
                                                               , pageLength = 10
                                                               
                                                             ) # end of options
                                                             
  ), server = FALSE
  )
  output$predictionaccuracy = DT::renderDataTable(DT::datatable(gse87211_validate()[[3]], escape=FALSE,
                                                             options = list(pageLength = 5, autoWidth = TRUE,
                                                                            columnDefs = list(list( targets = c(6), width = '200px')),
                                                                            scrollX = TRUE)))

  output$probPlot <- renderPlot({
        return(gse87211_validate()[[1]])
      })
  output$rocPlot <- renderPlot({
    return(gse87211_validate()[[2]])
  })
  

  

}