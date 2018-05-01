# Loading all packages needed in the creation of the Shiny App
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
#library(knitr)
library(shiny)
library(rsconnect)

# Data 
# Loading Output data files from Step ONE: SVM Model
# Generalized Data Directory
# Data must be stored in a folder callled data under your work directory, and the CSV file must be named as "output_data.csv".
# If you do not know what your work directory is, you can check it by using the function `getwd()`.
wd <- getwd()
#datadir <- paste0(wd, "/data/output_data.csv")
data <- fread("data/output_data.csv")
list_of_variables <- names(data)
landmark_xy <- fread("data/landmark_xy.csv")

# inputs
list_of_indices <- c(unique(data$sample_index), "Aggregated")
list_of_scores <- c("Precision", "Recall", "f1", "w_precision", "w_recall", "w_f1", "m_precision", "m_recall", "m_f1")
column_num <- 19
row_num <- 8

#assign a pair of x-y coordinate to each landmark
# data <- data %>%
#   mutate(column =  floor((landmark_index/row_num) -0.1)+1)
# variables named landmark_index, column, and row must be included in the landmark_xy file
data <- data %>%
  left_join(landmark_xy, by="landmark_index")

# Shiny App
# User Interface
ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Input
    sidebarPanel(
      selectInput("sampleindex", "Sample Index:", list_of_indices),
      selectInput("score", "Accuracy Measurement:", list_of_scores)
    ),
    
    # Output
    mainPanel(plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"),
              plotOutput("plot4"), plotOutput("plot5"), plotOutput("plot6")
    )
  )
)

#Server
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


