#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
   
    updateSelectInput(session, "variable", choices = names(df),selected="")
    
    df
  })
  
  
  summary <- reactive({
    req(data())
    req(input$variable)
  
    description<-get_summary_stats(data()[,input$variable])
  
    
    description

  })
  
  
  output$contents <- renderDataTable({
   req(data())
    
    data()
  })
  
  
  output$summary <- renderDataTable({
    req(summary())
    
    summary()
  })
  
  output$boxplot <- renderPlotly({
    req(data())
    req(input$variable)
    column_data <- data()[, input$variable]
    if (is.numeric(column_data)) {
      fig <- plot_ly(y = column_data, type = "box", name = input$variable)
      fig
    }
  })
  
  output$histogramme <- renderPlotly({
    req(data())
    req(input$variable)
    column_data <- data()[, input$variable]
    if (is.numeric(column_data)) {
      fig <- plot_ly(y = column_data, type = "histogram", name = input$variable)
      fig
    }
  })
  
  
  
  output$heatmap <- renderPlot({
    req(data())
    numeric_columns <- sapply(data(), is.numeric)
    numeric_data <- data()[, numeric_columns]
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    corrplot(cor_matrix, method = "color")
  })

  output$report <- downloadHandler(
    filename = function() { paste("report-", Sys.Date(), ".docx", sep="") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = data(), variable = input$variable)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}



