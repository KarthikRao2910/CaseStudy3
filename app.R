library(shiny)
library(ggplot2)
library(kernlab)
library(dplyr)
library(pheatmap)
library(datasets)

ui <- pageWithSidebar(
  headerPanel("Case Study 3"),
  
  sidebarPanel(
    selectInput('dataset','Select a dataset:',c("iris")),
    uiOutput("varselect1"),
    uiOutput("varselect2"),
    numericInput('k', 'Number of clusters', value = 4,
                 min=1, step=1),
    selectInput('kernel','Type of kernel:',c("linear","radial (RBF)"="RBF"))
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
    tabPanel("Plot",plotOutput('plot')),
    tabPanel("Summary of the data", verbatimTextOutput("summary")),
    tabPanel("Plots",plotOutput('plotHeat'), plotOutput('plotBox')),
    tabPanel("Correlation plots", plotOutput('plot1'),plotOutput('plot2')),
    tabPanel("Report",h2("Details of the algorithm"),
             p("Kmeans algorithm is an iterative algorithm that partitions the dataset into pre-defined distinct non-overlapping subgroups where each data only belongs to one group."),
             br(),
             p("Place k points into the space represented by the objects that are being clustered. These points represent initial group centroids. 
             Assign each object to the group that has the closest centroid.When all objects have been assigned, recalculate the positions of the k centroids.Repeat the above steps until the centroids no longer move. This produces a separation of the objects into groups from which the metric to be minimized can be calculated. "
               ),
             br(),br(),br(),
             h2("Data collected"),
             p("I am using the Iris data set that you get with R and also on Kaggle"),
             br(),br(),br(),
             h2("Why is this topic interesting to me"),
             p("As a beginner data scientist I thought why not go ahead with Iris dataset because it is widely used as a stepping stone for learning machine learning and data science concepts."),
             br(),br(),br(),
             h2("How did you analyse the data"),
             p("I created a summary of the data first, and then tabulating it."),
             br(),
             p("I then created heatmaps for the petals and sepals parameters with each species to see correlation. After that I created a boxplot of the given data.Can be found in the Plots tab."),
             br(),
             p("After that I created correlation plots between sepal width and length, and petal width and length. Can be found in the correlation plots tab."),
             p("After that I just followed how to do Kmeans clustering and arrived on the output as seen in the sidebar and the Plot tab.")
             )
  ) 
  
)
)

server <-
  function(input,output,session) {
    
    output$varselect1 <- renderUI({
      selectInput("var1", label="Select first variable for clustering:",
                  choices=names(dataset()), selected=names(dataset())[1])  
    })
    output$varselect2 <- renderUI({
      selectInput("var2", label="Select second variable for clustering:",
                  choices=names(dataset()), selected=names(dataset())[2])  
    })
    
    dataset <- reactive({
      if (input$dataset=="iris") {
        data(iris)
        iris[,-5]
      } else data.frame()
    })
    
    compute <- reactive({
      
      data   <- subset(dataset(), select=c(input$var1,input$var2))
      colnames(data) <- c("x","y")
      
      if(input$k>nrow(unique(data))) updateNumericInput(session,"k", value = nrow(unique(data)))
      if(input$k<1)                  updateNumericInput(session,"k", value = 1)
      
      if (input$kernel=="linear") {
        Kclust <- kmeans(data ,input$k)
        list(kmean.result = data.frame(data, cluster=as.factor(Kclust$cluster)),
             centers = as.data.frame(Kclust$centers))
      } else if (input$kernel=="RBF") {
        Kclust <- kkmeans(as.matrix(data), input$k, kernel="rbfdot")
        list(kmean.result = data.frame(data, cluster=as.factor(Kclust@.Data)),
             centers = data.frame(x=Kclust@centers[,1],
                                  y=Kclust@centers[,2]))
      }
    })
    
    output$plot <- renderPlot({
      data=compute()
      ggplot(data=data$kmean.result, aes(x=x, y=y, color=cluster)) +
        geom_point(size=3) + geom_point(data=data$centers,
                                        aes(x=x, y=y, color='Center'), pch=17, size=7) +
        ggtitle("K-means Clustering") + xlab(input$var1) + ylab(input$var2)
    })
    
    output$summary <- renderPrint({
      summary(dataset())
    })
    
    output$plotHeat <- renderPlot({
      data1 <- as.matrix(iris[, 1:4])
      row.names(data1) <- row.names(iris) # assign row names in the matrix
      pheatmap(data1,
               scale = "column",
               clustering_method = "average", # average linkage
               annotation_row = iris[, 5, drop = FALSE], # the 5th column as color bar
               show_rownames = FALSE,
               main="Heatmap"
      )
      
      
    })
    output$plotBox <- renderPlot({
      ggplot(data = iris) +
        aes(x = Species, y = Sepal.Length, color = Species) +
        geom_boxplot() +  geom_jitter(position = position_jitter(0.2)) + ggtitle("Boxplot of Iris Data")
    })
    
    output$plot1 <- renderPlot({
      ggplot(iris) +
        aes(x = Petal.Length, y = Petal.Width) +
        geom_point(aes(color = Species, shape = Species)) + 
        geom_smooth(method = lm) +
        annotate("text", x = 5, y = 0.5, label = "R=0.96") + 
        xlab("Petal length (cm)") + 
        ylab("Petal width (cm)") + 
        ggtitle("Correlation between Petal length and width")
    })
    
    output$plot2 <- renderPlot({
      ggplot(iris) +
        aes(x = Sepal.Length, y = Sepal.Width) +
        geom_point(aes(color = Species, shape = Species)) + 
        geom_smooth(method = lm) + 
        annotate("text", x = 5, y = 0.5, label = "R=0.96") + 
        xlab("Sepal length (cm)") + 
        ylab("Sepal width (cm)") + 
        ggtitle("Correlation between Sepal length and width") 
    })
  }    


shinyApp(ui = ui, server = server)