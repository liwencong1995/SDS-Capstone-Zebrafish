# All SVM models should produce 6 percision measurements. 
# They are precision score of type 1, recall score of type 1, and F1 score of type 1
# They are precision score of type 2, recall score of type 2, and F1 score of type 2
# They are overall precision score, overall recall score, and overall F1 score
# Majority Voting
#Join original type of the zebrafish
#User Input
#Threshold
#70%
# Get the overall prediction 
# accuracy rate
# format the app
# Ben
# 13 variable

# Shiny App------------------------------------------------------------------
# Loading all packages needed in the creation of the Shiny App
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(shiny)

AT <- fread("7.aggregatedResults/AT_2med_renamed.csv")
landmark_xy <- fread("3.InputData/tidy/landmark_xy.csv")
AT <- AT %>%
  left_join(landmark_xy, by="landmark_index")
list_of_indices <- c(unique(AT$sample_index), "aggregated") 
list_of_scores <- c("precision", "recall", "f1")
# Baselines
# 43 wildtype
c0_p_b <- 43/78
c0_r_b <- 1
c0_f1_b <- 2*c0_p_b*c0_r_b/(c0_p_b + c0_r_b)
# 35 mutant
c1_p_b <- 35/78
c1_r_b <- 1
c1_f1_b <- 2*c1_p_b*c1_r_b/(c1_p_b + c1_r_b)
# overall
p_b <- (c0_p_b * 43 + c1_p_b *35)/78
r_b <- (c0_r_b * 43 + c1_r_b *35)/78
f1_b <- (c0_f1_b * 43 + c1_f1_b *35)/78
  
# User Interface
ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Input
    sidebarPanel(
      selectInput("sampleindex", "Sample Index:", list_of_indices),
      selectInput("score", "Accuracy Measurement:", list_of_scores)
    ),
    
    # Output
    mainPanel(
      #heatmaps and histograms, side by side
      plotOutput("plot1"), plotOutput("plot2"), 
      plotOutput("plot3"), plotOutput("plot4"), 
      plotOutput("plot5"), plotOutput("plot6")
      )
  )
)

# Server---------------------------------------------------------------------
server <- function(input,output) {
  
  #loading data need to create visualizations
  data <- fread("7.aggregatedResults/AT_2med_all.csv")
  list_of_variables <- names(data)
  positions <- which(grepl("f1", list_of_variables) %in% TRUE)
  
  #render data
  dat <- reactive({
    
    #loading data need to create visualizations
    data <- fread("7.aggregatedResults/AT_2med_all.csv")
    list_of_variables <- names(data)

    #getting the percision scores that will be shown as the output
    position_1 <- which(list_of_variables %in% "sample_index")
    position_2 <- which(list_of_variables %in% "landmark_index")
    position_3 <- which(grepl("f1", list_of_variables) %in% TRUE)
    position_4 <- which(list_of_variables %in% "row")
    position_5 <- which(list_of_variables %in% "column")
    positions <- c(position_1, position_2, position_3, position_4, position_5)
    test <- data %>% select(positions)
    
    #filter out the observations not needed
    test_small <- test %>%
      filter(sample_index == input$sampleindex)
    
    #print variables
    print(test[1:10,])
    
    #return dataset
    print(test_small[1:3,])
    test_small
  })
  
  output$plot1 <- renderPlot({
    p1 <- ggplot(dat(), 
                 aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p1
  })
  
  output$plot3 <- renderPlot({
    p3 <- ggplot(dat(), 
                aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = c1_f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p3
  })
  
  output$plot5 <- renderPlot({
    p5 <- ggplot(dat(), 
                aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = c0_f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p5
  })
  
  output$plot2 <- renderPlot({
    p2 <- qplot(dat()$f1, geom = "histogram") +
      geom_vline(xintercept=f1_b, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p2
  })
  
  output$plot4 <- renderPlot({
    p4 <- qplot(dat()$c1_f1, geom = "histogram") +
      geom_vline(xintercept=c1_f1_b, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p4
  })
  
  output$plot6 <- renderPlot({
    p6 <- qplot(dat()$c0_f1, geom = "histogram") +
      geom_vline(xintercept=c0_f1_b, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p6
  })
}

# Shiny App
shinyApp(ui, server)
