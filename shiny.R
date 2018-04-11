#Shiny App
library(shiny)
library(rsconnect)
library(dplyr)
library(data.table)
library(ggplot2)

# input: Sample Index
index <- AT_landmarks[,c(1,306)]
index <- index %>%
  arrange(Index)
list_of_indices <- c(index$Index)
list_of_scores <- c("precision", "recall", "f1", "w_precision", "w_recall", "w_f1", "m_precision", "m_recall", "m_f1")
landmark_xy <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/analysis/landmark_xy.csv")
list_of_channel <- c("AT", "ZRf")

ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  selectInput("channel", "Channel:", list_of_channel),
  selectInput("sampleindex", "Sample Index:", list_of_indices),
  selectInput("score", "Accuracy Measurement:", list_of_scores),
  mainPanel(plotOutput("plot2")) )

server <- function(input,output) {
  dat <- reactive({
    wd <- getwd()
    dir <- paste0(wd, "/analysis/r", input$sampleindex, "_med_", input$channel, "_result.csv")
    test <- fread(dir)
    test <- test %>%
      left_join(landmark_xy, by="landmark_index")
    #test <- three_air_zone %>% dplyr::filter(Zone == input$taxizone)
    print(test)
    test
  })
  
  output$plot2 <- renderPlot({
    p1 <- ggplot(dat(), 
                 aes(x = y, y = x)) +
      geom_tile(aes(fill = precision)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(1, 19), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(1, 8), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p1
  })
}

shinyApp(ui, server)


