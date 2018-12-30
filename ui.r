##ui.r

library(shiny);library(shinydashboard);library(tibble)
library(xlsx);library(class);library(kknn);library(e1071)
library(caret);library(MASS);library(reshape2);library(ggplot2)
library(kernlab);library(bestglm);library(InformationValue)
library(earth);library(mda);library(glmnet);library(rpart);
library(partykit);library(randomForest);library(xgboost);library(ROCR)
library(xgboost);library(data.table);library(tibble);library(corrplot)
library(Amelia);library(car);library(MPV);library(car);library(Boruta)
library(mlbench);library(neuralnet);library(vcd);library(cluster);
library(HDclassif);library(NbClust);library(sparcl)
library(factoextra);library(caretEnsemble); library(caTools);library(DT)
library(klaR);library(compareGroups)

rm(list = ls())
#setwd("C:/R/")
#update.packages(checkBuilt = TRUE, ask = FALSE)	

# header ---------------------------------------------------
header <- dashboardHeader(
  title = "Machine Learning Templet"
)

# sidebar ---------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file", label = h5("Select Excel File")),
    numericInput("sheet",label = h5("Excel Sheet #"), 1, min=1, max=10),
    numericInput("seed","Random Seed", 1117, min=1, max=5000),
    numericInput("ratio","Test Data Set Ratio", 0.7, min=0.5, max=1.0, step=0.1),
    
    menuItem("EDA", tabName = "menuEDA", icon = icon("bar-chart-o")),
    menuItem("KNN", tabName = "knn", startExpanded = F, icon = icon("check-circle"),
             menuSubItem("KNN", tabName = "subMenu21", icon = icon("angle-right")),
             menuSubItem("KKNN", tabName = "subMenu22", icon = icon("angle-right"))),
    
    menuItem("Cluster", tabName = "cluster",startExpanded = F, icon = icon("compass"),
             menuSubItem("Hclust", tabName = "subMenu91", icon = icon("angle-right")),
             menuSubItem("Kmeans", tabName = "subMenu92", icon = icon("angle-right"))),
    
    
    menuItem("SVM", tabName = "svm",startExpanded = F, icon = icon("flag-checkered"),
             menuSubItem("Linear SVM", tabName = "subMenu31", icon = icon("angle-right")),
             menuSubItem("polynomial SVM", tabName = "subMenu32", icon = icon("angle-right")),
             menuSubItem("RBF SVM", tabName = "subMenu33", icon = icon("angle-right")),
             menuSubItem("Sigmoid SVM", tabName = "subMenu34", icon = icon("angle-right"))),     
    
    menuItem("Ensemble", tabName = "ens", startExpanded = F, icon = icon("connectdevelop")),
    
    menuItem("Logistic", tabName = "logistic",startExpanded = F, icon = icon("cog"),
             menuSubItem("Full Fit Logistic", tabName = "subMenu41", icon = icon("angle-right")),
             menuSubItem("Bestglm_Reduced", tabName = "subMenu42", icon = icon("angle-right")),
             menuSubItem("Bestglm_BIC", tabName = "subMenu43", icon = icon("angle-right"))),      
    
    menuItem("Random Forest", tabName = "randomforest",startExpanded = F, icon = icon("tree"),
             menuSubItem("Classification", tabName = "subMenu61", icon = icon("angle-right")),
             menuSubItem("Regression", tabName = "subMenu62", icon = icon("angle-right")),
             menuSubItem("Cluster", tabName = "subMenu63", icon = icon("angle-right"))),
    
    menuItem("Regression", tabName = "regression",startExpanded = F, icon = icon("bullseye", lib = "font-awesome")), 
    
    menuItem("Regularization", tabName = "regular",startExpanded = F, icon = icon("compass"),
             menuSubItem("Ridge", tabName = "subMenu81", icon = icon("angle-right")),
             menuSubItem("LASSO", tabName = "subMenu82", icon = icon("angle-right")),
             menuSubItem("Elastic Net", tabName = "subMenu82", icon = icon("angle-right"))),
    
    menuItem("Discriminant Analysis", tabName = "discriminant",startExpanded = F, icon = icon("life-ring"),
             menuSubItem("Linear DA", tabName = "subMenu51", icon = icon("angle-right")),
             menuSubItem("Quadratic DA", tabName = "subMenu52", icon = icon("angle-right"))),
    
    menuItem("MARS", tabName = "mars",startExpanded = F, icon = icon("empire"),
             menuSubItem("Classification", tabName = "subMenu71", icon = icon("angle-right")),
             menuSubItem("Regression", tabName = "subMenu72", icon = icon("angle-right"))),
    
    #  menuItem("LASSO", tabName = "lasso",startExpanded = F, icon = icon("cubes")),
    #  menuItem("Elastic", tabName = "elastic",startExpanded = F, icon = icon("dropbox")),
    
    menuItem("Neural Net", tabName = "cnn", startExpanded = F, icon = icon("dropbox")),
    
    menuItem("Xgboost", tabName = "xgboost",startExpanded = F, icon = icon("cogs")), 
    menuItem("Data", tabName = "data",startExpanded = F, icon = icon("stack-overflow"))
    
    # icons : flag-checkered    group  pagelines connectdevelop
    
  ) #sidebarMenu close
  
) # dashboardSidebar close

# body ---------------------------------------------------
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "menuEDA",
            fluidPage(
              sidebarPanel(width=3,
                           radioButtons("type", "Response type:", c("class" = "1", "numeric" = "0" )),
                           numericInput("height", "Box Plot Height:", min =5, max =30000, value = 100)
              ),
              mainPanel(width=9,
                        fluidRow(
                          box(title = "Correlation Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot1")),
                          box(title = "Box Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot2")),
                          box(title = "Missing values", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot0")),
                          box(title = "Data Summary", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view0"))
                        )))), # tab item close
    
    ##### Ensemble tab  #####
    
    
    
    tabItem(tabName = "ens",
            
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("cv","Cross Validation", 5, min=1, max=20, step=1),
                           checkboxGroupInput("mod", "Select model", 
                                              choices = c("rpart" = "rpart", "Earth" = "earth", 
                                                             "KNN" = "knn", "random forest"="rf",
                                                             "LDA"="lda2","QDA"= "stepQDA",
                                                             "SVM_Poly"="svmPoly","SVM_Linear"="svmLinear2",
                                                             "SVM_mRBF"="svmRadialCost",
                                                             'svmRadialSigma'='svmRadialSigma',
                                                             'svmSpectrumString'='svmSpectrumString',
                                                             'tan'='tan'),
                                              selected = c("rpart","earth","knn","rf" ) ) 
                           
              ),
              mainPanel(width=9,
                        fluidRow(
                          box(title = "ROC/Sensitivity/Specificity", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary120")),
                          box(title = "Correlation coefficient", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary121")),
                          box(title = "Ensemble Stacked Model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary122")),
                          box(title = "Ensemble ROC", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary123"))
                          
                        )))),
    
    
    ##### KNN tab  #####
    
    tabItem(tabName = "subMenu21",
            
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("cv","CV #", 10, min=1, max=20, step=1),
                           sliderInput("kmax", "K #", min =1, max =60, value = 30)
              ),
              mainPanel(width=9,
                        box(title = "KNN", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                            verbatimTextOutput("summary1"))
              ))),
    
    tabItem(tabName = "subMenu22",
            
            fluidPage(
              sidebarPanel(width=3,
                           sliderInput("kmax", "K #", min =1, max =60, value = 30),
                           radioButtons("distance","KKNN Distance:", c("Euclidean"="1","Manhattan"="2"))
              ),
              mainPanel(width=9,
                        fluidRow(
                          box(title = "KKNN", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary2")),
                          box(title = "KKNN Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot3")),
                          box(title = "Important varibale Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot4"))        
                          
                        )))),
    
    ##### SVM tab  ####
    tabItem(tabName = "subMenu31",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Linear SVM", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary3")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view1"))
                        )))), # tab item close
    
    tabItem(tabName = "subMenu32",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Polynomial SVM", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary4")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view2"))
                        )))), # tab item close
    
    tabItem(tabName = "subMenu33",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "RBF SVM", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary5")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view3"))
                        )))), # tab item close   
    
    tabItem(tabName = "subMenu34",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Sigmoid SVM", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary6")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view4"))
                        )))), # tab item close         
    
    
    ##### Logistic tab ####
    tabItem(tabName = "subMenu41",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Full Fit Logistic", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary11")),
                          box(title = "Confidence Interval", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary12")),
                          box(title = "Exp(coef)", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary13")),
                          box(title = "MisClassError", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view5")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view6")) 
                        )))), # tab item close
    
    tabItem(tabName = "subMenu42",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Bestglm_Reduced", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary14")),
                          box(title = "MisClassError", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view7")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view8"))
                        )))), # tab item close
    
    
    tabItem(tabName = "subMenu43",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Bestglm_BIC", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary15")),
                          box(title = "MisClassError", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view9")),
                          box(title = "ConfusionMatrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view10"))
                        )))), # tab item close  
    
    ##### Discriminant Analysis tab ####
    
    tabItem(tabName = "subMenu51",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Linear Discriminant Analysis", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary21")),
                          box(title = "Fitted vs Real value", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot5")),
                          box(title = "Histogram and Density Diagram", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot6")),
                          box(title = "MisClassError_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view11")),
                          box(title = "ConfusionMatrix_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view12")),
                          box(title = "MisClassError_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view13")),
                          box(title = "ConfusionMatrix_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view14"))
                        )))), # tab item close
    
    tabItem(tabName = "subMenu52",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Quadratic Discriminant Analysis", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary22")),
                          box(title = "MisClassError_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view15")),
                          box(title = "ConfusionMatrix_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view16")),
                          box(title = "MisClassError_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view17")),
                          box(title = "ConfusionMatrix_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view18")),
                          #    box(title = "Histogram and Density Diagram of QDA Value", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                          #        plotOutput("plot7")),
                          box(title = "Fitted vs Real value", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot8"))
                        )))), # tab item close
    
    
    ##### Random Forest tab  ####
    
    tabItem(tabName = "subMenu61",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("rf.tree","Random Forest Tree #", 200, min=1, max=500, step=1)
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Random Forest Classfication", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary61")),
                          box(title = "Error Rate by Tree #", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot61")),
                          box(title = "Minimun Error Rate Tree ", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary62")),
                          box(title = "Veriable Important", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary63")),
                          box(title = "Variable Importance Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot63")),
                          #  box(title = "Coefficiency of Mean Decress Gini", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                          #      verbatimTextOutput("summary64")),
                          box(title = "ConfusionMatrix_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary65")),
                          box(title = "ConfusionMatrix_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary66")),
                          box(title = "Test Set Result", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary67"))
                        )))), # tab item close
    
    tabItem(tabName = "subMenu62",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("tree","Random Forest Tree #", 50, min=1, max=500, step=1)
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Random Forest Regression", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary32")),
                          box(title = "Error Rate by Tree #", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot13")),
                          box(title = "Minimun Error Rate Tree ", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary33")),
                          box(title = "Best Fit Tree Model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary34")),
                          box(title = "Training DataSet RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary35")),
                          box(title = "Variable Importance Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot14")),
                          box(title = "Increase Node Purity", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary36")),
                          box(title = "Test DataSet RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary37")),
                          box(title = "Predict versus Observe", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot80"))
                        )))),
    
    tabItem(tabName = "subMenu63",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("cluster.tree","Random Forest Tree #", 2000, min=1, max=5000, step=1)
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Random Forest Cluster", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary110")),
                          box(title = "Proximity matrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary111")),
                          box(title = "Variable Importance", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary112")),
                          box(title = "dissimilarity matrix", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary113")),
                          box(title = "Predict versus Observe", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary114")),
                          box(title = "Predict versus Observe", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary115"))
                        )))),
    
    ##### MARS tab  ####
    tabItem(tabName = "subMenu71",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Multivariate Adaptive Classification Splines", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary31")),
                          box(title = "plotmo plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot11")),
                          box(title = "Density Diagram", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot12")),
                          box(title = "Veriable Important", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view21")),
                          box(title = "MisClassError_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view22")),
                          box(title = "ConfusionMatrix_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view23"))
                        )))),
    
    tabItem(tabName = "subMenu72",
            fluidPage(
              mainPanel(width=12, 
                        fluidRow(         
                          box(title = "Multivariate Adaptive Regression Splines", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary59")),
                          box(title = "Plot 1", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot38")),
                          box(title = "Plot 2", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot39")),
                          box(title = "RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view33"))
                        )))),
    
    
    ##### Regression tab ####
    
    tabItem(tabName = "regression",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("feature","Feature #", 3, min=1, max=10, step=1)),
              
              
              mainPanel(width=9, 
                        fluidRow(         
                          
                          box(title = "Cp, Rsq(adj) versus Feature names", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot21")),
                          box(title = "Cp, Rsq(adj) versus Feature #", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot22")),
                          box(title = "Rsq_adj Best model Predictors", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view58")),
                          box(title = "VIF_Full model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view59")),
                          box(title = "RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view60")),
                          box(title = "BestSubset Model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary41")),
                          box(title = "VIF_BestSubset Model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary42")),
                          box(title = "Predict versus Actual", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot23")),
                          box(title = "Residual Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot24")),
                          box(title = "Test DataSet RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary43"))
                        )))),
    
    ##### Ridge tab ####
    
    tabItem(tabName = "subMenu81",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("lambda_r","Lambda Ridge #", 1.0, min=0.0, max=10000, step=0.1) ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Ridge Regression", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary45")),
                          box(title = "Plot 1", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot25")),
                          box(title = "Plot 2", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot26")),
                          box(title = "Veriable Important", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary46")),
                          box(title = "Plot 3", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot27")),
                          box(title = "Plot 4", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot28")),
                          box(title = "RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view25"))
                        )))),
    
    ##### LASSO tab ####
    
    tabItem(tabName = "subMenu82",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("lambda_l","LASSO Ridge #", 1.0, min=0.0, max=10000, step=0.1) ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "LASSO Regression", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary51")),
                          box(title = "Plot 1", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot31")),
                          box(title = "Plot 2", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary52")),
                          box(title = "Veriable Important", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot32")),
                          box(title = "RMSE", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("view26"))
                        )))),
    
    ##### Elastic Net tab #####
    
    tabItem(tabName = "subMenu83",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("alpha_e","Elastic Net Alpha", 1, min=0.0, max=1, step=0.1),
                           numericInput("lambda_e","Elastic Net Lambda", 0.2, min=0.0, max=10, step=0.01) ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Elastic Net", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary55")),
                          box(title = "Elastic Net", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary56")),
                          box(title = "Plot 1", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot35")),
                          box(title = "Best Fit Elastic Net", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary57"))
                          
                        )))),
    
    ##### Hcluster tab ####
    
    tabItem(tabName = "subMenu91",
            fluidPage(
              sidebarPanel(width=3,
                           radioButtons("method","Method :", c("ward.D2"="ward.D2", "single"="single", "complete"="complete",
                                                               "average"="average", "mcquitty"="mcquitty", "median"="median",
                                                               "centroid"="centroid", "kmeans"="kmeans")), 
                           radioButtons("dist","Distance:", c("euclidean" = "euclidean", "maximum"="maximum", 
                                                              "manhattan"="manhattan", "canberra"="canberra",
                                                              "binary"="binary", "minkowski"="minkowski")) ,
                           numericInput("nc","Max nc :", 15, min=3, max=20, step=1),
                           numericInput("cutree","Cutree :", 3, min=2, max=10, step=1)
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Hubert Index", width="100%", collapsible = F, solidHeader = TRUE, status = "primary",
                              plotOutput("plot101")),
                          box(title = "Dendrogram", width="100%", collapsible = F, solidHeader = TRUE, status = "primary",
                              plotOutput("plot102")),
                          box(title = "hierarchical cluster", width = "100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary91")),
                          box(title = "group table", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary92")),
                          box(title = "cross table", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary93"))
                        )))),
    
    tabItem(tabName = "subMenu92",
            fluidPage(
              sidebarPanel(width=3,
                           numericInput("nc","Max nc :", 15, min=3, max=20, step=1),
                           numericInput("group","Group :", 3, min=2, max=20, step=1),
                           numericInput("nstart","nstart :", 25, min=1, max=50, step=1),
                           numericInput("cu2","Cutree :", 3, min=2, max=10, step=1)
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Hubert Index", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot104")),
                          #       box(title = "Dendrogram", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                          #          plotOutput("plot105")),
                          box(title = "Cluster plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot106")),
                          box(title = "Kmeans Result", width = "100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary95")),
                          box(title = "kmeans cluster", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary96")),
                          box(title = "kmeans centers", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary97"))
                          
                        )))),
    
    ##### eXtreme Gradient Boosting tab ####
    
    tabItem(tabName = "xgboost",
            fluidPage(
              sidebarPanel(width=3,
                           radioButtons("nrounds","nrounds: Last model tree count",  c("75"="75","100"="100")),
                           radioButtons("eta","eta: Learning Rate",  c("0.3"="0.3","0.1"="0.12","0.01"="0.01")),
                           radioButtons("gamma","gamma: Minimun loss reduction",  c("0.5"="0.5","0.25"="0.25")),
                           radioButtons("max_depth","Max depth of each tree",  c("2"=2,"3"=3)) 
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "eXtreme Gradient Boosting", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary71")),
                          box(title = "Best Fit eXtreme Gradient Boosting", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary72")),
                          box(title = "ConfusionMatrix_Training Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary73"))
                        )))),
    
    
    
    ##### Neural net tab ####
    
    tabItem(tabName = "cnn",
            fluidPage(
              sidebarPanel(width=3,
                           radioButtons("err","Error Function:", c("ce" = "ce", "sse" = "sse" )),
                           numericInput("hidden","Hidden Layer", 1, min=1, max=10, step=1) 
              ),
              
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Neural Net Plot", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              plotOutput("plot81")),
                          box(title = "Neural net Model", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary75")),
                          box(title = "ConfusionMatrix_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary76")),
                          box(title = "ConfusionMatrix_Test Set", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              verbatimTextOutput("summary77"))
                        )))),
    
    
    
    
    
    #### Data Tab ####
    
    tabItem(tabName = "data",
            fluidPage(
              mainPanel(width=9, 
                        fluidRow(         
                          box(title = "Data", width="100%", collapsible = T, solidHeader = TRUE, status = "primary",
                              tableOutput('data1')
                              
                          )))))
    
  ) # tab items close
  
) #body close

# merge ---------------------------------------------------
dashboardPage(
  header,
  sidebar,
  body
)
