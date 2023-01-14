#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(factoextra)
library(cluster)
library(tree)
library(caTools)
library(rpart.plot)
library(DT)
source("global.R")
source("globals.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$rul <- renderDT({
    #rdt
  })
  
  # Accueil
  output$consigne <- renderPrint({
    #scan("Consigne.txt",what = "character" ,encoding = "utf-8", sep = "\n")
    readLines("Consigne.txt", encoding = "utf-8")
    
  })
  
  output$hist <- renderPlot({
    hist(donnees[c(input$att)])
  })
  
  # Description
  output$des <- renderPrint({
    #sum <- readLines("anneal.txt", encoding = "utf-8")
    scan("anneal.txt",what = "character" ,encoding = "utf-8", sep = "\n")
    
  })
  
  
  # Page des données
  output$input_file <- renderDT({
    
    #donnees <- read.table("anneal.data.txt", sep = input$sep, header = F)
    output$SumD <- renderPrint({
      #paste(dim(donnees), summary(donnees), sep=",")
      summary(donnees)
    })
    
    if(input$header){
      #donnees <- read.table("anneal.data.txt", sep = input$sep, header = input$header)
      #nb = length(colnames(donnees))
      #colnames(donnees)=paste("A",1:nb, sep = "")
      donnee
      #Data
    }
    DT::datatable(donnees, extensions = 'Scroller', options = list(scrollX = T))
    
  })
  
  output$rul <- renderPrint({
    rules <- apriori(new_df, parameter = list(supp = 0.05, conf = 0.6))
    rules
  })
  
  output$bestrul <- renderPrint({
    inspect(rules[1:5])
  })
  
  output$dimD <- renderText({
    dim(donnees)
  })
  
  output$Notval <- renderPrint({
    donnees[is.na(donnees),]
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Result", "csv", sep = ".")
    },
    
    content = function(file){
      write.csv(dat, file)
    }
  )

  # Classification
  # Combine the selected variables into a new data frame
  
  output$resN_train <- renderPlot({plot(nn)})
  output$resN_test <- renderPlot({})
  output$resN_MC <- renderPrint({pre})
  
  output$ad_train <- renderPlot({
    
    #rpart.plot(arbre, fallen.leaves = F, cex = .5)
    plot(Arbre)
    text(Arbre)
  })
  output$ad_test <- renderPlot({})
  output$ad_MC <- renderPrint({table_mat})
  output$acc <- renderPrint({accuracy})
  
  output$kppv_train <- renderPlot({
   
  })
  
  output$kppv_test <- renderPlot({
   plot(knn)
  })
  output$kppv_MC <- renderPrint({
    table_knn
  })
  output$acc2 <- renderPrint({accuracy2})
  
  
  output$svm_train <- renderPlot({
    plot(modelsvm, d_svm)
  })
  output$svm_test <- renderPrint({s})
  output$svm_MC <- renderPrint({table_svm})
  output$acc3 <- renderPrint({accuracy_svm})
  
  
  
  ## K-means
  output$k_means <- renderPlot({
    X = dataset[, c(input$xcol, input$ycol)]
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    set.seed(29)
    KM = kmeans(X, input$clusters, iter.max = 300, nstart = 10)
    #par(mar = c(5.1, 4.1, 0, 1))
    #clusplot(selectedData(),
    #     col = clusters()$cluster,
    #     pch = 20, cex = 3)
    clusplot(X,
             KM$cluster,
             lines = 2,
             shade = TRUE,
             color = TRUE,
             plotchar = FALSE,
             main = paste('Clusters of clinets'),
             xlab = input$xcol,
             ylab = input$ycol)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
    #autoplot(clusters(),vars)
  })
  
  
  
  # Hierarchique
  output$hierar <- renderPlot({
    # Clusters
    Data.clusters <- cutree(hc.out_Data, k=input$clusters2)
    
    # Visualize the cluster
    rownames(F_data_std) <- paste(Data$Classes, 1:dim(Data)[1], sep="_")
    
    output$t_hierar <- renderPrint({
      table(Data.clusters, Data$Classes)
    })
    
    fviz_cluster(list(data=F_data_std, cluster=Data.clusters))
    
  })
  
})
