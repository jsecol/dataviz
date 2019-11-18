## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Histogram dashboard"),
  dashboardSidebar(sliderInput("slider2017bins", "Number of bins 2017:", 1, 100, 30),
                   sliderInput("slider2018bins", "Number of bins 2018:", 1, 100, 30),
                   div(style="padding: 10pt;", downloadButton("download2017Data", "Download 2017 data", class = "butt")),
                   div(style="padding: 10pt;", downloadButton("download2018Data", "Download 2018 data", class = "butt")),
                   tags$head(tags$style(".butt{background-color:darkblue;}"))),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 8,
             box(
               title = "Histogram of 2017 credit balance.", width = NULL, 
               solidHeader = TRUE, status = "primary",
               plotOutput("plot1", height = 225)
             ),
             box(
               title = "Histogram of 2018 credit balance.", width = NULL, solidHeader = TRUE, status = "primary",
               plotOutput("plot2", height = 225)
             )
      ),
      
      column(width = 4,
             box(
               title = "Controls 2017", status = "primary", width = NULL,
               selectInput("housing2017", "Housing:", 
                           c("No" = "no", "Yes" = "yes")),
               uiOutput("secondSelection2017"),
               uiOutput("thirdSelection2017"),
               uiOutput("min_x_2017"),
               uiOutput("max_x_2017")
             ),
             box(
               title = "Controls 2018", status = "primary", width = NULL,
               selectInput("housing2018", "Housing:", 
                           c("No" = "no", "Yes" = "yes")),
               uiOutput("secondSelection2018"),
               uiOutput("thirdSelection2018"),
               uiOutput("min_x_2018"),
               uiOutput("max_x_2018")
             )
      )
      )
    )
  )    

server <- function(input, output) {

  #see for data source options: https://shiny.rstudio.com/articles/persistent-data-storage.html
 
  bankdata2017 <- read.csv("data/bank2017.csv")
  bankdata2018 <- read.csv("data/bank2018.csv")  


  lookup_2017 <- bankdata2017 %>% group_by(housing, education, job) %>%
    summarise(count = n()) %>% ungroup() %>% as.data.frame()
  
  lookup_2018 <- bankdata2018 %>% group_by(housing, education, job) %>%
    summarise(count = n()) %>% ungroup() %>% as.data.frame()
   
  data1 <- reactive({
    if(input$education2017=="") {
      data.frame(bankdata2017[bankdata2017$housing == input$housing2017,], stringsAsFactors = FALSE)
    } else 
      if(input$education2017!="") {
        data.frame(bankdata2017[bankdata2017$housing == input$housing2017 &
                                  bankdata2017$education == input$education2017 &
                                  bankdata2017$job == input$job2017,], stringsAsFactors = FALSE)  
      }
  })
  
  data2 <- reactive({
    if(input$education2018=="") {
      data.frame(bankdata2018[bankdata2018$housing == input$housing2018,])
    } else 
      if(input$education2018!="") {
        data.frame(bankdata2018[bankdata2018$housing == input$housing2018 &
                              bankdata2018$education == input$education2018 &
                              bankdata2018$job == input$job2018,])  
    }
  })
  
  
  output$plot1 <- renderPlot({
    gg_hist1 <- ggplot(data1(), aes(x=balance)) + 
      geom_histogram(bins = input$slider2017bins, color = "blue")
    
    if(!is.na(input$min_x_2017c) & !is.na(input$max_x_2017c)) {
    gg_hist1 + coord_cartesian(xlim = c(input$min_x_2017c, input$max_x_2017c)) }
      else {
        gg_hist1
      }
  })
  
  output$plot2 <- renderPlot({
    gg_hist2 <- ggplot(data2(), aes(x=balance)) + geom_histogram(bins = input$slider2018bins,
                                                                 color = "red")

    if(!is.na(input$min_x_2018c) & !is.na(input$max_x_2018c)) {
    gg_hist2 + coord_cartesian(xlim = c(input$min_x_2018c, input$max_x_2018c)) }
      else {
        gg_hist2
    }
  })
  
  output$secondSelection2017 <- renderUI({
    selectInput("education2017", "Education:", 
                choices = lookup_2017[lookup_2017$housing==input$housing2017,"education"])
  })
  
  output$thirdSelection2017 <- renderUI({
    selectInput("job2017", "Job:", 
                choices = lookup_2017[lookup_2017$housing==input$housing2017 & 
                                        lookup_2017$education==input$education2017,"job"])
  })
  
  output$min_x_2017 <- renderUI({
    numericInput("min_x_2017c", "Minimum 2017 axis:", 
                 min(bankdata2017[bankdata2017$housing==input$housing2017 &
                                    bankdata2017$education==input$education2017 &
                                    bankdata2017$job==input$job2017,]$balance))
  })
  
  output$max_x_2017 <- renderUI({
    numericInput("max_x_2017c", "Maximum 2017 axis:", 
                 max(bankdata2017[bankdata2017$housing==input$housing2017 &
                                    bankdata2017$education==input$education2017 &
                                    bankdata2017$job==input$job2017,]$balance))
  })
  
  output$secondSelection2018 <- renderUI({
    selectInput("education2018", "Education:", 
                choices = lookup_2018[lookup_2018$housing==input$housing2018,"education"])
  })
  
  output$thirdSelection2018 <- renderUI({
    selectInput("job2018", "Job:", 
                choices = lookup_2018[lookup_2018$housing==input$housing2018 & 
                                        lookup_2018$education==input$education2018,"job"])
  })
  
  output$min_x_2018 <- renderUI({
    numericInput("min_x_2018c", "Minimum 2018 axis:", 
                 min(bankdata2018[bankdata2018$housing==input$housing2018 &
                                    bankdata2018$education==input$education2018 &
                                    bankdata2018$job==input$job2018,]$balance))
  })
  output$max_x_2018 <- renderUI({
    numericInput("max_x_2018c", "Maximum 2018 axis:", 
                 max(bankdata2018[bankdata2018$housing==input$housing2018 &
                                    bankdata2018$education==input$education2018 &
                                    bankdata2018$job==input$job2018,]$balance))
  })

  
  output$download2017Data <- downloadHandler(
    filename = function() {
      paste("data2017", "csv", sep = ".")
    },
    content = function(file) {
    write.table(data1(), file, sep = ",", row.names = FALSE)
    }
  
  )
}

shinyApp(ui, server)