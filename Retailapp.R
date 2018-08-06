library(shiny)
library(ggplot2)
library(data.table)
library(scales)
library(magrittr)
library(DT)
library(tidyr)


sales <- read.csv("https://raw.githubusercontent.com/connorH982/projects/master/sales%20data-set.csv")
sales$Store<-as.factor(sales$Store)
sales$Date <- sales$Date %>% as.Date("%d/%m/%Y")
storesDat <- read.csv("https://raw.githubusercontent.com/connorH982/projects/master/stores%20data-set.csv")
storesDat$Store<-as.factor(storesDat$Store)
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Weekly sales over time",
             sidebarLayout(position = "left",
                           
                           sidebarPanel(
                             selectInput(inputId = "selection", 
                                         label = "Select a Store's Sales to visualize", choices = c(1:length(unique(sales$Store)),"All"))
                           ),
                           mainPanel(
                             
                             plotOutput("sales")
                             
                           )
             )
    ),
    tabPanel("Total yearly sales by store",
             p("Table is sorted by 2012 (descending total sales)"),
             dataTableOutput("YearlySales")
    ),
    tabPanel("Store size and type",
             plotOutput("characteristics"))
  )
  
)

server <- function(input, output, session) {
  output$sales <- renderPlot({
    if(input$selection=="All"){
      ggplot(data = sales,aes(x=sales$Date,y=sales$Weekly_Sales)) +
        geom_smooth(color = "dodgerblue4", aes(group = as.factor(sales$Store)), size = 0.1)+
        xlab("Date")+ylab("Weekly Sales")+ggtitle("Weekly Sales for all Stores (45)")+scale_x_date(labels = date_format("%Y")) + geom_smooth(color = "dark red", size =1,aes(x=sales$Date,y=sales$Weekly_Sales, group = NULL))
    }
    else{
      subData <- sales[sales$Store==input$selection,]
      ggplot(data = subData,aes(x=subData$Date,y=subData$Weekly_Sales))+ geom_smooth(color = "dark red", size =0.1) +
        geom_point(color = "dodgerblue4", size = 0.2, shape = 18)+
        xlab("Date")+ylab("Weekly Sales")+ggtitle(paste("Weekly Sales for Store", input$selection))+scale_x_date(labels = date_format("%Y"))
    }
  })
  
  output$YearlySales <-DT::renderDataTable({
    #Prepare data, cut data by year, convert to wide format while summing up yearly revenue
    SaleDT <-aggregate(sales$Weekly_Sales, by = list(sales$Store,cut.Date(sales$Date, "years")),sum)%>%spread(2,3)
    colnames(SaleDT) <- c("Store","2010","2011","2012")
    Type <-storesDat[,-3]
    SaleDT <- merge(Type,SaleDT, by = "Store")
    rownames(SaleDT) <- paste("Store",rownames(SaleDT))
    SaleDT <- SaleDT[order(SaleDT$`2012`,decreasing = TRUE),-1]
    SaleDT$Type<-as.factor(SaleDT$Type)
    
    #Display using Datatable
    brks <- quantile(SaleDT[,-1], probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 150, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(", ., ",", ., ",255)")}
    datatable(SaleDT) %>% formatCurrency(2:4) %>% formatStyle(c("2010","2011","2012"), backgroundColor = styleInterval(brks, clrs),backgroundPosition = 'center')%>%
      formatStyle("Type",backgroundColor = styleEqual(levels(SaleDT$Type),c("tomato","seagreen","goldenrod")))
  })
  
  output$characteristics <- renderPlot({
    ggplot(data = storesDat)+geom_bar(aes(x = storesDat$Store,y = storesDat$Size, fill = storesDat$Type),stat = "identity")+xlab("Store #")+ylab("Store Size")+guides(fill = guide_legend(title = "Store Type"))+
      scale_fill_manual(values = c("tomato","seagreen","goldenrod"))
  })
}

shinyApp(ui, server)