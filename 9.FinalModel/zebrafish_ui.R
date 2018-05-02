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
#library(viridis)
library(shiny)

data <- fread("7.aggregatedResults/AT_2med_renamed_2.csv")
landmark_xy <- fread("3.InputData/tidy/landmark_xy.csv")
data <- data %>%
  left_join(landmark_xy, by="landmark_index")
list_of_indices <- c(unique(data$sample_index)) 
#list_of_scores <- c("precision", "recall", "f1")

# User Interface
ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Input
    sidebarPanel(
      selectInput("sampleindex", "Sample Index:", list_of_indices),
      #selectInput("score", "Accuracy Measurement:", list_of_scores),
      
      # Input: Simple integer interval ----
      sliderInput("precision", "Precision Rate Threshold:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01),
      sliderInput("recall", "Recall Rate Threshold:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01),
      sliderInput("f1", "F1 Rate Threshold:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01)
    ),
    
    # Output
    mainPanel(
      tabsetPanel(
        tabPanel("Accuracy Threshold",tableOutput("values")),
        #heatmaps and histograms, side by side
        tabPanel("Type 1 Precision", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot2"), plotOutput("plot1"))
          )), 
        tabPanel("Type 2 Precision", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot4"), plotOutput("plot3"))
          )),
        tabPanel("Precision",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot6"), plotOutput("plot5"))
          )),
        tabPanel("Type 1 Recall", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot8"), plotOutput("plot7"))
        )), 
        tabPanel("Type 2 Recall", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot10"), plotOutput("plot9"))
        )),
        tabPanel("Recall",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot12"), plotOutput("plot11"))
        )),
        tabPanel("Type 1 F1", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot14"), plotOutput("plot13"))
        )), 
        tabPanel("Type 2 F1", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot16"), plotOutput("plot15"))
        )),
        tabPanel("F1",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot18"), plotOutput("plot17"))
        ))
        )
      )
  )
)

# Server---------------------------------------------------------------------
server <- function(input,output) {

  #loading data needed to create visualizations
  # data <- fread("7.aggregatedResults/AT_2med_renamed.csv")
  # positions <- which(grepl(input$score, list_of_variables) %in% TRUE)
  # Baselines
  # 43 wildtype
  data_base <- data %>%
    mutate(type0_p_b = type0_num/(type0_num+type1_num),
           type0_r_b = 1,
           type0_f1_b = 2*type0_p_b*type0_r_b/(type0_p_b + type0_r_b),
           # 35 mutant
           type1_p_b = type1_num/(type0_num+type1_num),
           type1_r_b = 1,
           type1_f1_b = 2*type1_p_b*type1_r_b/(type1_p_b + type1_r_b),
           # overall
           p_b = (type0_p_b * type0_num + type1_p_b *type1_num)/(type0_num+type1_num),
           r_b = (type0_r_b * type0_num + type1_r_b *type1_num)/(type0_num+type1_num),
           f1_b = (type0_f1_b * type0_num + type1_f1_b *type1_num)/(type0_num+type1_num))
  list_of_variables <- names(data_base)
  
  scoreSelect <- reactive({
    #show what is selected
    positions <- which(grepl(input$score, list_of_variables) %in% TRUE)
    test_small <- data_base %>%
      select(positions) %>%
      head()
    scoresS <- names(test_small)
    print(scoresS)
    scoresS
  })
  
  scoreOne <- reactive({
    scoreSelect()[1]
  })
  
  #render data
  dat <- reactive({
    # #loading data need to create visualizations
    # data <- fread("7.aggregatedResults/AT_2med_all.csv")
    #filter out the observations not needed
    test <- data_base %>%
      filter(sample_index == input$sampleindex)
    #return dataset
    print(test[1,])
    test
  })
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    # original type
    type <- dat()$type[1]
    
    # perdicting type
    test_pred <- dat() %>%
      filter(overall_precision >= input$precision,
             overall_recall >= input$recall,
             overall_f1 >= input$f1)%>%
      group_by(pred) %>%
      summarise(N = n()) %>%
      mutate(max = max(N)) %>%
      mutate(predict = ifelse(N == max, TRUE, FALSE)) %>%
      filter(predict == TRUE)
    prediction <- test_pred$pred[1]
    
    data.frame(
      Name = c("Precision Rate Threshold",
               "Recall Rate Threshold",
               "F1 Rate Threshold",
               "Type",
               "Prediction",
               "Number of Type 0 Samples Used In Model",
               "Number of Type 1 Samples Used In Model"),
      Value = as.character(c(input$precision,
                             input$recall,
                             input$f1,
                             type,
                             prediction,
                             mean(dat()$type0_num),
                             mean(dat()$type1_num)
                             )),
      stringsAsFactors = FALSE)
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  # precision -------------------------------------------------------------------------------
  output$plot1 <- renderPlot({
    p1 <- ggplot(dat(),aes(x = column, y = row)) +
      geom_tile(aes(fill = type0_precision)) +
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
      geom_tile(aes(fill = type1_precision)) +
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
      geom_tile(aes(fill = overall_precision)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p5
  })
  
  output$plot2 <- renderPlot({
    baseline <- mean(dat()$type0_p_b)
    p2 <- qplot(dat()$type0_precision, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p2
  })
  
  output$plot4 <- renderPlot({
    baseline <- mean(dat()$type1_p_b)
    p4 <- qplot(dat()$type1_precision, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p4
  })
  
  output$plot6 <- renderPlot({
    baseline <- mean(dat()$p_b)
    p6 <- qplot(dat()$overall_precision, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p6
  })
  # recall ---------------------------------------------------------------------------------
  output$plot7 <- renderPlot({
    p7 <- ggplot(dat(),aes(x = column, y = row)) +
      geom_tile(aes(fill = type0_recall)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p7
  })
  
  output$plot9 <- renderPlot({
    p9 <- ggplot(dat(), 
                 aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = type1_recall)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p9
  })
  
  output$plot11 <- renderPlot({
    p11 <- ggplot(dat(), 
                 aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = overall_recall)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p11
  })
  
  output$plot8 <- renderPlot({
    baseline <- mean(dat()$type0_r_b)
    p8 <- qplot(dat()$type0_recall, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p8
  })
  
  output$plot10 <- renderPlot({
    baseline <- mean(dat()$type1_r_b)
    p10 <- qplot(dat()$type1_recall, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p10
  })
  
  output$plot12 <- renderPlot({
    baseline <- mean(dat()$r_b)
    p12 <- qplot(dat()$overall_recall, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p12
  })
  # f1 --------------------------------------------------------------------------------------
  output$plot13 <- renderPlot({
    p13 <- ggplot(dat(),aes(x = column, y = row)) +
      geom_tile(aes(fill = type0_f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p13
  })
  
  output$plot15 <- renderPlot({
    p15 <- ggplot(dat(), 
                 aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = type1_f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p15
  })
  
  output$plot17 <- renderPlot({
    p17 <- ggplot(dat(), 
                  aes(x = column, y = row)) +
      geom_point() +
      #scale_color_viridis() +
      geom_tile(aes(fill = overall_f1)) +
      xlab("Alpha") +
      ylab("Theta") +
      scale_x_continuous(limits = c(0, 20), breaks=c(1, 10, 19), labels=c("-90.51", "0", "90.51")) +
      scale_y_continuous(limits = c(0, 9), breaks=c(1, 4.5, 8), labels=c("-3.14","0","3.14")) +
      scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25)) 
    p17
  })
  
  output$plot14 <- renderPlot({
    baseline <- mean(dat()$type0_f1_b)
    p14 <- qplot(dat()$type0_f1, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p14
  })
  
  output$plot16 <- renderPlot({
    baseline <- mean(dat()$type1_f1_b)
    p16 <- qplot(dat()$type1_f1, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p16
  })
  
  output$plot18 <- renderPlot({
    baseline <- mean(dat()$f1_b)
    p18 <- qplot(dat()$overall_f1, geom = "histogram") +
      geom_vline(xintercept=baseline, linetype="dashed", color = "red") +
      scale_x_continuous(limits = c(0, 1)) +
      xlab("Precision") +
      ylab("Count")  
    p18
  })
}


# Shiny App
shinyApp(ui, server)

