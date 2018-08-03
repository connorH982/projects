library(shiny)
library(ggplot2)
library(data.table)
library(scales)
library(magrittr)
#setwd("C:/Users/Owner/Desktop/Portfolio/Retail_App")
#sales <- read.csv("sales data-set.csv") %>% data.table(key = "Store")
sales <- read.csv("https://raw.githubusercontent.com/connorH982/projects/master/sales%20data-set.csv") %>% data.table(key = "Store")

ui <- fluidPage(
  selectInput(inputId = "selection", 
              label = "Select a Store's Sales to visualize", choices = c(1:length(unique(sales$Store)),"All")),
  plotOutput("sales")

  )

server <- function(input, output, session) {
  output$sales <- renderPlot({
    sales$Date <- sales$Date %>% as.Date("%d/%m/%Y")
    if(input$selection=="All"){
      ggplot(data = sales,aes(x=sales$Date,y=sales$Weekly_Sales))+ geom_smooth(color = "dark red", size =0.1) +
        geom_point(color = "dodgerblue4", size = 0.2, shape = 18)+
        xlab("Date")+ylab("Weekly Sales")+ggtitle("Weekly Sales for all Stores (45)")+scale_x_date(labels = date_format("%Y"))
    }
    else{
      subData <- sales[Store==input$selection,]
      ggplot(data = subData,aes(x=subData$Date,y=subData$Weekly_Sales))+ geom_smooth(color = "dark red", size =0.1) +
        geom_point(color = "dodgerblue4", size = 0.2, shape = 18)+
        xlab("Date")+ylab("Weekly Sales")+ggtitle(paste("Weekly Sales for Store", input$selection))+scale_x_date(labels = date_format("%Y"))
    }
  })
}

shinyApp(ui, server)