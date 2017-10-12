#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(xts)
library(reshape2)
library(quantmod)
library(lubridate)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Inwestycja o rolowanych horyzoncie czasowym"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Inwestycja o horyzoncie miesięcznym wynoszącym ( miesięcy ):",
                     min = 1,
                     max = 15,
                     value = 12),
         selectInput(inputId = "dataset",
                     label = "Choose a dataset:",
                     choices = c("Stoxx50","SP500"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   stoxx50 = "stoxx50.csv"
   sp500 = "sp500.csv"
  
   datasetInput <- reactive({
    switch(input$dataset,
           "Stoxx50" = stoxx50,
           "SP500" = sp500)
   })
  
   
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
     stoxx50 <-read.csv(datasetInput())[c(1,5)]
     stoxx50.xts <- xts(stoxx50[,2],as.Date(stoxx50[,1]))
     stoxx50.lagged12 <- diff(stoxx50.xts,lag=input$bins,differences = 1)/lag(stoxx50.xts,input$bins)
     
     
     stoxx50_bymonth <- list()
     
     for ( i in 1:12 ) {
       
       stoxx50_month <-stoxx50.lagged12[month(stoxx50.lagged12) == i]
       stoxx50_bymonth[[i]] <- stoxx50_month[!is.na(index(stoxx50_month))]
       
     }
     
     #lapply(stoxx50_bymonth, mean,na.rm=TRUE)
     
     df <- melt(stoxx50_bymonth)
     qplot(factor(L1), value, data = df, geom = "boxplot")

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

