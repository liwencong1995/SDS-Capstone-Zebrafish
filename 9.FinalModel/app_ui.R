# Loading all packages needed in the creation of the Shiny App
library(shiny)
library(rsconnect)
library(dplyr)
library(knitr)
library(data.table)
library(ggplot2)

# Data 

# Loading Output data files from Step ONE: SVM Model
wd <- getwd()
datadir <- paste0(wd, "/3. Input Data/output_data.csv")

AT <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/5. output_AT_2med/AT_2med.csv")
ZRF <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/6. output_ZRF_2med/ZRF_2med.csv")

# Generalized Data Directory
# Data must be stored in a folder callled data under your work directory, and the CSV file must be named as "output_data.csv".
# If you do not know what your work directory is, you can check it by using the function `getwd()`.
wd <- getwd()
datadir <- paste0(wd, "/data/output_data.csv")
data <- fread(datadir)



# input: Sample Index
AT_landmarks <- read_csv("data/raw/AT_landmarks.csv")
index <- AT_landmarks[,c(1,306)]
index <- index %>%
  arrange(Index)
list_of_indices <- c(index$Index, "AT", "ZRF")
list_of_scores <- c("precision", "recall", "f1", "w_precision", "w_recall", "w_f1", "m_precision", "m_recall", "m_f1")
landmark_xy <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/analysis/landmark_xy.csv")
list_of_channel <- c("AT", "ZRF")

ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  selectInput("channel", "Channel:", list_of_channel),
  selectInput("sampleindex", "Sample Index:", list_of_indices),
  selectInput("score", "Accuracy Measurement:", list_of_scores),
  mainPanel(fluidRow(
    splitLayout(cellWidths = c("90%", "60%"), plotOutput("plot1"), plotOutput("plot2"))
  ))
)


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
  
  output$plot1 <- renderPlot({
    p1 <- ggplot(dat(), 
                 aes(x = y, y = x)) +
      geom_tile(aes(fill = precision)) +
      facet_wrap() +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(1, 19), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(1, 8), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p1
  })
  
  output$plot2 <- renderPlot({
    p2 <- qplot(dat()$precision, geom = "histogram") +
      xlab("Precision") +
      ylab("Count")  
    p2
  })
  
}

shinyApp(ui, server)


