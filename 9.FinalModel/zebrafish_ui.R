# landmark_xy <- fread("3.InputData/tidy/landmark_xy.csv")
# #assign position to each landmark
# test <- data %>%
#   left_join(landmark_xy, by="landmark_index")
# list_of_variables <- names(AT)
# #ZRF <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/7.aggregatedResults/ZRF_2med.csv")

#add aggregated data
AT <- AT %>%
  select(-V1)
aggregated <- AT %>%
  select(-V1) %>%
  group_by(landmark_index) %>%
  summarise(f1 = mean(f1),
            c1_f1 = mean(c1_f1),
            c1_precision = mean(c1_precision),
            c1_recall = mean(c1_recall),
            c1_support = mean(c1_support),
            c1_c1 = mean(c1_c1),
            c1_c0 = mean(c1_c0),
            precision = mean(precision),
            pred = mean(pred),
            recall = mean(recall),
            sample_index = "aggregated",
            c0_f1 = mean(c0_f1),
            c0_precision = mean(c0_precision),
            c0_recall = mean(c0_recall),
            c0_support = mean(c0_support),
            c0_c1 = mean(c0_c1),
            c0_c0 = mean(c0_c0),
            min_alpha = mean(min_alpha),
            min_theta = mean(min_theta),
            row = mean(row),
            column = mean(column)
            )
aggregated <- aggregated[,c(2,1,3:22)]
AT_all <- bind_rows(AT, aggregated)
fwrite(AT_all, "7.aggregatedResults/AT_2med_all.csv")

#assign a pair of x-y coordinate to each landmark
# AT <- AT %>%
#   mutate(column =  floor((landmark_index/row_num) -0.1)+1)
AT <- AT %>%
  left_join(landmark_xy, by="landmark_index")

#getting the percision scores that will be shown as the output
grepl("precision", list_of_variables)
positions <- which(grepl("precision", list_of_variables) %in% TRUE)
length(positions)
list_of_variables[positions[1]]
list_of_variables[positions[2]]
list_of_variables[positions[3]]

# All SVM models should produce 6 percision measurements. 
# They are precision score of type 1, recall score of type 1, and F1 score of type 1
# They are precision score of type 2, recall score of type 2, and F1 score of type 2
# They are overall precision score, overall recall score, and overall F1 score

# Shiny App------------------------------------------------------------------
#inputs
AT <- fread("7.aggregatedResults/AT_2med_all.csv")
list_of_indices <- c(unique(AT$sample_index), "aggregated") 
list_of_scores <- c("precision", "recall", "f1")
# Baselines
# 43 wildtype
c0_p_b <- 0.5
c0_r_b <- 0.5
c0_f1_b <- 0.5
# 35 mutant
c1_p_b <- 0.5
c1_r_b <- 0.5
c1_f1_b <- 0.5
# overall
p_b <- 0.5
r_b <- 0.5
f1_b <- 0.5
  
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
