# Shiny App---------------------------------------------------
# Loading packages needed in the creation of the Shiny App
library(dplyr)
library(data.table)
library(ggplot2)
library(shiny)

# User Input -------------------------------------------------
# Please modify the file directory accordingly
data <- fread("data/output_data_type0.csv")
# data <- fread("7.aggregatedResults/AT_2med_renamed_2.csv")

# List of input variables ------------------------------------
list_of_indices <- c(unique(data$sample_index)) 
# Please add or subtract channels from the list_of_channels accordingly
list_of_channels <- c("type0", "type1")
# list_of_channels <- c("AT", "ZRF")

# User Interface
ui <- fluidPage(
  titlePanel(title=h4("Classification of Wildtype and Mutant Zebrafish Brains via Computational Method", 
                      align="center")),
  
  # Sidebar containing all input variables
  sidebarLayout(
    
    # User Inputs
    sidebarPanel(
      selectInput("sampleindex", "Sample Index:", list_of_indices),
      selectInput("channel", "Channel:", list_of_channels),
      
      # Input accuracy score threshold: 0-1 intervals
      sliderInput("precision", "Precision Rate Threshold:",
                  min = 0, max = 1,
                  value = 0, step = 0.01),
      sliderInput("recall", "Recall Rate Threshold:",
                  min = 0, max = 1,
                  value = 0, step = 0.01),
      sliderInput("f1", "F1 Rate Threshold:",
                  min = 0, max = 1,
                  value = 0, step = 0.01)
    ),
    
    # Output
    mainPanel(
      tabsetPanel(
        tabPanel("Accuracy Threshold",tableOutput("values")),
        #heatmaps and histograms, side by side
        tabPanel("Type 0 Precision", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot2"), plotOutput("plot1"))
          )), 
        tabPanel("Type 1 Precision", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot4"), plotOutput("plot3"))
          )),
        tabPanel("Precision",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot6"), plotOutput("plot5"))
          )),
        tabPanel("Type 0 Recall", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot8"), plotOutput("plot7"))
        )), 
        tabPanel("Type 1 Recall", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot10"), plotOutput("plot9"))
        )),
        tabPanel("Recall",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot12"), plotOutput("plot11"))
        )),
        tabPanel("Type 0 F1", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot14"), plotOutput("plot13"))
        )), 
        tabPanel("Type 1 F1", fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot16"), plotOutput("plot15"))
        )),
        tabPanel("F1",fluidRow(
          splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot18"), plotOutput("plot17"))
        ))
        )
      )
  )
)

# Server------------------------------------------------------
server <- function(input,output) {
  
  #loading data needed to create visualizations
  dat <- reactive({
    
    # Please modify the file directory accordingly
    path <- paste0("data/output_data_", input$channel, ".csv")
    # path <- paste0("7.aggregatedResults/", input$channel, "_2med_renamed_2.csv")
    data <- fread(path)
    
    # Please modify the file directory accordingly
    landmark_xy <- fread("data/landmark_xy.csv")
    # landmark_xy <- fread("3.InputData/tidy/landmark_xy.csv")
    
    # Adding position of each landmark
    data <- data %>%
      left_join(landmark_xy, by="landmark_index")
    
    # Adding baselines to the data file
    data_base <- data %>%
      filter(overall_precision >= input$precision,
             overall_recall >= input$recall,
             overall_f1 >= input$f1) %>%
      mutate(# type 0
             type0_p_b = type0_num/(type0_num+type1_num),
             type0_r_b = 1,
             type0_f1_b = 2*type0_p_b*type0_r_b/(type0_p_b + type0_r_b),
             
             # type 1
             type1_p_b = type1_num/(type0_num+type1_num),
             type1_r_b = 1,
             type1_f1_b = 2*type1_p_b*type1_r_b/(type1_p_b + type1_r_b),
             
             # overall
             p_b = (type0_p_b * type0_num + type1_p_b *type1_num)/(type0_num+type1_num),
             r_b = (type0_r_b * type0_num + type1_r_b *type1_num)/(type0_num+type1_num),
             f1_b = (type0_f1_b * type0_num + type1_f1_b *type1_num)/(type0_num+type1_num)
             )
    
    #filter out the sample not interested
    test <- data_base %>%
      filter(sample_index == input$sampleindex)
    
    #return dataset
    print(test[1,])
    test
    })
  
  # Reactive expression to create data frame of all input values
  sliderValues <- reactive({
    
    # Getting the true type of the sample
    type <- dat()$type[1]
    
    # Doing majority vote and perdicting the type of the sample
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
    
    # summary table
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
  
  # Show the threshold values in an summary table
  output$values <- renderTable({
    sliderValues()
  })
  
  # precision ------------------------------------------------
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
  
  # recall ---------------------------------------------------
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


# Outputting the Shiny App
shinyApp(ui, server)

