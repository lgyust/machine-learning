
library(shiny);library(shinydashboard);library(tibble)
library(xlsx);library(class);library(kknn);library(e1071)
library(caret);library(MASS);library(reshape2);library(ggplot2)
library(kernlab);library(bestglm);library(InformationValue)
library(earth);library(mda);library(glmnet);library(rpart);
library(partykit);library(randomForest);library(xgboost);library(ROCR)
library(xgboost);library(data.table);library(tibble);library(corrplot)
library(MPV);library(car); library(dplyr); library(magrittr)
library(leaps);library(Boruta);library(mlbench);library(neuralnet);library(vcd)
library(cluster);library(compareGroups);library(HDclassif);library(NbClust);library(sparcl)
library(factoextra);library(caretEnsemble); library(caTools);library(DT)


rm(list = ls())

function(input, output) {
  
  
  ww <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T)
    w <- na.omit(w)
    colnames(w)[ncol(w)] <- "type"
    w.scale <- data.frame(scale(w[,-ncol(w)]))
    w.scale$type <- w$type
    w.scale$type <- ifelse(w.scale$type == levels(w.scale$type)[2], 1, 0)
    w.scale$type <- as.factor(w.scale$type)
    w.scale
  })
  
  qq <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T)
    w <- na.omit(w) 
    w <- data.frame(w)
    colnames(w)[ncol(w)] <- "y"
    w
  })
  
  ee <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    e <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    colnames(e)[ncol(e)] <- "type"
  })
  
  
  y <- reactive({
    y <- ifelse(ww()$type == levels(ww()$type)[2], 1, 0)
  })  
  
  ind <- reactive({ 
    set.seed(input$seed)
    ind <- sample(2, nrow(ww()),replace = TRUE,prob = c(input$ratio,1-input$ratio))
  })
  
  
  train <- reactive({
    train <- ww()[ind()==1,]
    train
  })
  
  test <- reactive({
    test <- ww()[ind()==2,]
    test
  })
  
  ind1 <- reactive({ 
    set.seed(input$seed)
    ind1 <- sample(2, nrow(qq()),replace = TRUE,prob = c(input$ratio,1-input$ratio))
  })
  
  train1 <- reactive({
    train1 <- qq()[ind1()==1, ]
  })
  
  test1 <- reactive({
    test1 <- qq()[ind1()==2, ]
  })
  
  output$plot1 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    y <- ifelse(input$type == 1 , 1, 0)
    subset(w, select = c(1:ncol(w)-y)) %>%  cor() %>%  corrplot.mixed()
  })
  
  output$plot2 <- renderPlot({
    w.melt <- melt(ww(), id.var="type")
    ggplot(data=w.melt, aes(x=type, y=value)) + ylim(0, input$height) +
      geom_boxplot() + facet_wrap(~variable,ncol="blue")
  })
  
  output$plot0 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T)
    missmap(w, main = "Missing values vs observed")
  })
  
  
  output$view0 <- renderPrint({
    str(ww())
  })
  
  
  output$summary1 <- renderPrint({
    control <- trainControl(method = "cv", number=input$cv)
    grid <- expand.grid(k = 1 : input$kmax)
    knn.train <- caret::train(type~., data=train(),
                              method="knn",
                              trControl=control,
                              tuneGrid=grid)
    knn.train
    
    
  })
  
  output$summary2 <- renderPrint({
    
    kknn.train <- train.kknn(type~., data=train(), kmax = input$kmax, distance = input$distance,
                             kernel = c("rectangular","triangular","epanechnikov",
                                        "biweight","triweight","cosine","inversion",
                                        "gaussian","rank"))
    
    kknn.train
    
  })
  
  output$plot3 <- renderPlot({
    
    kknn.train <- train.kknn(type~., data=train(), kmax = input$kmax, distance = input$distance,
                             kernel = c("rectangular","triangular","epanechnikov",
                                        "biweight","triweight","cosine","inversion",
                                        "gaussian","rank"))
    plot(kknn.train)
    
  })
  
  output$plot4 <- renderPlot({ 
    
    TrainData <- ww()[,-ncol(ww())]
    TrainClasses <- ww()[,ncol(ww())]
    knnFit <- caret::train(TrainData, TrainClasses, "knn")
    
    knnImp <- varImp(knnFit)
    dotPlot(knnImp)
  })
  
  
  ###### SVM #########
  
  output$summary3 <- renderPrint({ 
    
    linear.tune <- tune.svm(type~., data=train(), kernel= "linear",
                            cost = c(0.001, 0.01, 0.1, 1, 5, 10))
    summary(linear.tune)
  })
  
  output$view1 <- renderPrint({
    
    linear.tune <- tune.svm(type~., data=train(), kernel="linear",
                            cost=c(0.001,0.01,0.1,1,5,10))
    best.linear <- linear.tune$best.model
    tune.test <- predict(best.linear,newdata=test())
    table(tune.test, test()$type)
  })
  
  output$summary4 <- renderPrint({
    
    poly.tune <- tune.svm(type~., data=train(), kernel="polynomial",
                          degree=c(2,3,4,5), coef0 = c(0.1,0.5,1,2,3,4))
    summary(poly.tune)
  })
  
  output$view2 <- renderPrint({
    
    poly.tune <- tune.svm(type~., data=train(), kernel="polynomial",
                          degree=c(2,3,4,5), coef0 = c(0.1,0.5,1,2,3,4))
    
    best.poly <- poly.tune$best.model
    poly.test <- predict(best.poly, newdata=test())
    table(poly.test, test()$type)
  })
  
  output$summary5 <- renderPrint({
    
    rbf.tune <- tune.svm(type~., data=train(), kernel="radial",
                         gamma = c(0.1,0.5,1,2,3,4))
    summary(rbf.tune)
  })
  
  output$view3 <- renderPrint({
    
    rbf.tune <- tune.svm(type~., data=train(), kernel="radial",
                         gamma = c(0.1,0.5,1,2,3,4))
    best.rbf <- rbf.tune$best.model
    rbf.test <- predict(best.rbf, newdata=test())
    table(rbf.test, test()$type)
  })
  
  output$summary6 <- renderPrint({
    
    sigmoid.tune <- tune.svm(type~., data=train(), kernel= "sigmoid",
                             gamma=c(0.1,0.5,1,2,3,4),
                             coef0 = c(0.1,0.5,1,2,3,4))
    summary(sigmoid.tune)
  })
  
  output$view4 <- renderPrint({
    
    sigmoid.tune <- tune.svm(type~., data=train(), kernel= "sigmoid",
                             gamma=c(0.1,0.5,1,2,3,4),
                             coef0 = c(0.1,0.5,1,2,3,4))
    best.sigmoid <- sigmoid.tune$best.model
    sigmoid.test <- predict(best.sigmoid, newdata=test())
    table(sigmoid.test, test()$type)
  })
  
  #### Logistic Regression ####
  
  output$summary11 <- renderPrint({
    
    full.fit <- glm(type~., family = binomial, data=train())
    summary(full.fit)
  })
  
  output$summary12 <- renderPrint({
    
    full.fit <- glm(type~., family = binomial, data=train())
    confint(full.fit)
  })
  
  output$summary13 <- renderPrint({
    
    full.fit <- glm(type~., family = binomial, data=train())
    exp(coef(full.fit))
  })
  
  output$view5 <- renderPrint({
    
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    full.fit <- glm(type~., family = binomial, data=train())
    
    test.probs <- predict(full.fit, newdata=test(), type= "response")
    misClassError(testY, test.probs)
  })
  
  output$view6 <- renderPrint({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    full.fit <- glm(type~., family = binomial, data=train())
    test.probs <- predict(full.fit, newdata=test(), type= "response")
    confusionMatrix(testY, test.probs)
  })
  
  output$summary14 <- renderPrint({
    
    y <- ifelse(ww()$type == levels(ww()$type)[2], 1, 0)
    trainY=y[ind()==1]
    testY=y[ind()==2]
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    bestglm(Xy=Xy, IC="CV", CVArgs = list(Method="HTF", K=10, REP=1), family = binomial)
    
  })
  
  output$view7 <- renderPrint({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    
    b.glm <- bestglm(Xy=Xy, IC="CV", CVArgs = list(Method="HTF", K=10, REP=1), family = binomial)
    reduce.fit <- glm(formula(b.glm$BestModel), family = binomial, data=train())
    test.cv.probs <- predict(reduce.fit, newdata=test(), type= "response")
    misClassError(testY, test.cv.probs)
  })
  
  output$view8 <- renderPrint({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    b.glm <- bestglm(Xy=Xy, IC="CV", CVArgs = list(Method="HTF", K=10, REP=1), family = binomial)
    reduce.fit <- glm(formula(b.glm$BestModel), family = binomial, data=train())
    test.cv.probs <- predict(reduce.fit, newdata=test(), type= "response")
    confusionMatrix(testY, test.cv.probs)
  })
  
  output$summary15 <- renderPrint({
    
    y <- ifelse(ww()$type == levels(ww()$type)[2], 1, 0)
    trainY=y[ind()==1]
    testY=y[ind()==2]
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    bestglm(Xy=Xy, IC="BIC", family = binomial)
  })
  
  
  
  output$view9 <- renderPrint({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    bic.glm <-bestglm(Xy=Xy, IC="BIC", family = binomial)
    bic.fit <- glm(formula(bic.glm$BestModel), family = binomial, data=train())
    test.bic.probs <- predict(bic.fit, newdata=test(), type= "response")
    misClassError(testY,test.bic.probs)
  })
  
  output$view10 <- renderPrint({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    
    X <- train()[,-ncol(train())]
    Xy <- data.frame(cbind(X, trainY))
    bic.glm <-bestglm(Xy=Xy, IC="BIC", family = binomial)
    bic.fit <- glm(formula(bic.glm$BestModel), family = binomial, data=train())
    test.bic.probs <- predict(bic.fit, newdata=test(), type= "response")
    confusionMatrix(testY, test.bic.probs)
  })
  
  
  #### Discriminant Analysis ####
  
  output$summary21 <- renderPrint({
    lda.fit <- lda(type~., data=train())
    lda.fit
  })
  
  output$plot5 <- renderPlot({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    lda.fit <- lda(type~., data=train())
    train.lda.probs <- predict(lda.fit)$posterior[,2]
    plot(train.lda.probs, trainY, col=testY+10)
  })  
  
  output$plot6 <- renderPlot({
    lda.fit <- lda(type~., data=train())
    plot(lda.fit, type="both")
  })
  
  output$view11 <- renderPrint({
    trainY=y()[ind()==1]
    lda.fit <- lda(type~., data=train())
    train.lda.probs <- predict(lda.fit)$posterior[,2]
    misClassError(trainY,train.lda.probs)
  })
  
  output$view12 <- renderPrint({
    trainY=y()[ind()==1]
    lda.fit <- lda(type~., data=train())
    train.lda.probs <- predict(lda.fit)$posterior[,2]
    confusionMatrix(trainY, train.lda.probs)
  })
  
  
  output$view13 <- renderPrint({
    testY=y()[ind()==2]
    lda.fit <- lda(type~., data=train())
    
    test.lda.probs <- predict(lda.fit, newdata=test())$posterior[,2]
    misClassError(testY, test.lda.probs)
  })
  
  output$view14 <- renderPrint({
    testY=y()[ind()==2]
    lda.fit <- lda(type~., data=train())
    
    test.lda.probs <- predict(lda.fit, newdata=test())$posterior[,2]
    confusionMatrix(testY, test.lda.probs)
  })
  
  output$summary22 <- renderPrint({
    
    qda.fit <- qda(type~., data = train() )
    qda.fit
  })
  
  output$view15 <- renderPrint({
    trainY=y()[ind()==1]
    qda.fit <- qda(type~., data=train())
    train.qda.probs <- predict(qda.fit)$posterior[,2]
    misClassError(trainY, train.qda.probs)
  })
  
  output$view16 <- renderPrint({
    trainY=y()[ind()==1]
    qda.fit <- qda(type~., data=train())
    train.qda.probs <- predict(qda.fit)$posterior[,2]
    confusionMatrix(trainY, train.qda.probs)
  })
  
  
  output$view17 <- renderPrint({
    testY=y()[ind()==2]
    qda.fit <- qda(type~., data=train())
    test.qda.probs <- predict(qda.fit, newdata=test())$posterior[,2]
    misClassError(testY, test.qda.probs)
  })
  
  output$view18 <- renderPrint({
    testY=y()[ind()==2]
    qda.fit <- qda(type~., data=train())
    test.qda.probs <- predict(qda.fit, newdata=test())$posterior[,2]
    confusionMatrix(testY, test.qda.probs)
  })
  
  output$plot7 <- renderPlot({
    qda.fit <- qda(type~., data=train())
    test.prob <- predict(qda.fit,type="response") # 확률을 저장
    pred.qda <- prediction(test.prob, train()$type)
    perf.qda <- performance(pred.qda, "tpr","fpr")
    plot(perf.qda, main="ROC", col=1)
  })
  
  output$plot8 <- renderPlot({
    trainY=y()[ind()==1]
    testY=y()[ind()==2]
    qda.fit <- qda(type~., data=train())
    train.qda.probs <- predict(qda.fit)$posterior[,2]
    plot(train.qda.probs, trainY, col=testY+10)
  })
  
  
  ##### MARS_I ######
  
  output$summary31 <- renderPrint({
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1,
                       glm=list(family=binomial))
    summary(earth.fit)
  })
  
  output$plot11 <- renderPlot({ 
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1,
                       glm=list(family=binomial))
    plotmo(earth.fit)
    
  })
  
  output$plot12 <- renderPlot({ 
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1,
                       glm=list(family=binomial))
    plotd(earth.fit)
    
  })
  
  output$view21 <- renderPlot({ 
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=10, ncross=3,degree=2, minspan=-1,
                       glm=list(family=binomial))
    evimp(earth.fit)
  })
  
  output$view22 <- renderPrint({
    testY=y()[ind()==2]
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1,
                       glm=list(family=binomial))
    test.earth.probs <- predict(earth.fit, newdata = test(),type="response")
    misClassError(testY,test.earth.probs)
  })
  
  output$view23 <- renderPrint({
    testY=y()[ind()==2]
    earth.fit <- earth(type~., data=train(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1,
                       glm=list(family=binomial))
    test.earth.probs <- predict(earth.fit, newdata = test(),type="response")
    confusionMatrix(testY,test.earth.probs)
  })
  
  #### Random Forest_classification ######
  
  output$summary61 <- renderPrint({
    rf.class <- randomForest(type~., mtry = floor(sqrt(ncol(train()))),
                             ntree = 500, data=train())
    rf.class
  })
  
  
  output$plot61 <- renderPlot({ 
    rf.class <- randomForest(type~.,  mtry = floor(sqrt(ncol(train()))),
                             ntree = 500, data=train())
    plot(rf.class)
    
  }) 
  
  output$summary62 <- renderPrint({ 
    rf.class <- randomForest(type~., mtry = floor(sqrt(ncol(train()))),
                             ntree = 500, data=train())
    which.min(rf.class$err.rate[ , 1])
  })
  
  output$summary63 <- renderPrint({
    rf.class.2 <- randomForest(type~., mtry = floor(sqrt(ncol(train()))),
                               data=train(),ntree=input$rf.tree)
    print(rf.class.2)
  })
  
  
  output$plot63 <- renderPlot({ 
    rf.class.2 <- randomForest(type~., mtry = floor(sqrt(ncol(train()))),
                               data=train(), ntree=input$rf.tree)
    varImpPlot(rf.class.2)
    
  })
  
  output$summary64 <- renderPrint({
    rf.class.2 <- randomForest(type~., mtry = floor(sqrt(ncol(train()))),
                               data=train(), ntree=input$rf.tree)
    randomForest::importance(rf.class.2, type = 1)
  })
  
  output$summary65 <- renderPrint({
    rf.class.2 <- randomForest(type~., data=train(), ntree=input$rf.tree)
    rf.class.train <- predict(rf.class.2, newdata=train(),type= "response")
    #caret::confusionMatrix(rf.class.train , train()$type )
    table(rf.class.train, train()$type)
    
  })
  
  output$summary66 <- renderPrint({
    rf.class.2 <- randomForest(type~.,  mtry = floor(sqrt(ncol(train()))),
                               data=train(), ntree=input$rf.tree)
    rf.class.test <- predict(rf.class.2, newdata=test(),type= "response")
    table(rf.class.test , test()$type) 
  })
  
  output$summary67 <- renderPrint({
    rf.class.2 <- randomForest(type~., data=train(),ntree=input$rf.tree)
    rf.class.test <- predict(rf.class.2, newdata=test(),type= "response")
    caret::confusionMatrix(rf.class.test , test()$type) 
    
  })
  
  
  ##### Regression ######
  
  
  output$plot21 <- renderPlot({
    par(mfrow=c(1,2))
    subfit <- regsubsets(y~., data=qq())
    plot(subfit, scale="Cp", main="Cp versus Features")
    plot(subfit, scale="adjr2", main="Rsq(adj) versus Features")
    
  })
  
  output$plot22 <- renderPlot({ 
    par(mfrow=c(1,2))
    subfit <- regsubsets(y~., data=qq())
    b.sum <- summary(subfit)
    plot(b.sum$cp, type="l", xlab="# of Features", ylab="cp", 
         main="Cp versus Feature", col="blue")
    plot(b.sum$adjr2, type="l", xlab="# of Features", ylab="cp", 
         main="R-sq(adj) versus Feature",col="green")
  })
  
  output$view58 <- renderPrint({
    subfit <- regsubsets(y~., data=qq())
    b.summary <-  summary(subfit)
    which.max(b.summary$adjr2)
  })
  
  output$view59 <- renderPrint({
    subfit <- lm(y~., data=qq())
    round(vif(subfit),2)
  })
  
  output$view60 <- renderPrint({
    subfit <- regsubsets(y~., data=qq())
    b.summary <-  summary(subfit)
    round(sqrt(b.summary$rss/ncol(qq())),2)
    
  })
  
  
  output$summary41 <- renderPrint({
    leapSet <- leaps(x=qq()[,-ncol(qq())], y=qq()[,ncol(qq())], nbest = 1,method = "adjr2")
    selectVarsIndex <- leapSet$which[input$feature, ]  # pick selected vars
    newData <- qq()[, selectVarsIndex]   # new data for building selected model
    selectedMod <- lm(y ~ ., data=newData)  # build model
    summary(selectedMod)
  })
  
  
  output$summary42 <- renderPrint({
    leapSet <- leaps(x=qq()[,-ncol(qq())], y=qq()[,ncol(qq())], nbest = 1,method = "adjr2")
    selectVarsIndex <- leapSet$which[input$feature, ]  # pick selected vars
    newData <- qq()[, selectVarsIndex]   # new data for building selected model
    selectedMod <- lm(y ~ ., data=newData)  # build model
    vif(selectedMod)   
  })
  
  output$plot23 <- renderPlot({ 
    leapSet <- leaps(x=qq()[,-ncol(qq())], y=qq()[,ncol(qq())], nbest = 1,method = "adjr2")
    selectVarsIndex <- leapSet$which[input$feature, ]  # pick selected vars
    newData <- qq()[, selectVarsIndex]   # new data for building selected model
    selectedMod <- lm(y ~ ., data=newData)  # build model
    plot(selectedMod$fitted.values, qq()$y, xlab="Predicted", ylab="Actual", main="Predicted vs Actual")
    # ols <- lm(y~., data=train1())
    # plot(ols$fitted.values, train1()$y, xlab="Predicted", ylab="Actual", main="Predicted vs Actual")
  })
  
  output$plot24 <- renderPlot({ 
    leapSet <- leaps(x=qq()[,-ncol(qq())], y=qq()[,ncol(qq())], nbest = 1,method = "adjr2")
    selectVarsIndex <- leapSet$which[input$feature, ]  # pick selected vars
    newData <- qq()[, selectVarsIndex]   # new data for building selected model
    selectedMod <- lm(y ~ ., data=newData)  # build model
    par(mfrow=c(2,2))
    plot(selectedMod)
    #ols <- lm(y~., data=train1())
    
  })
  
  output$summary43 <- renderPrint({
    leapSet <- leaps(x=qq()[,-ncol(qq())], y=qq()[,ncol(qq())], nbest = 1,method = "adjr2")
    selectVarsIndex <- leapSet$which[input$feature, ]  # pick selected vars
    newData <- train1()[, selectVarsIndex] %>% na.omit() # new data for building selected model
    selectedMod <- lm(y ~ ., data=newData)
    reg.y <- predict(selectedMod, newx=newData, type = "response")
    reg.resid <- reg.y - train1()$y
    round(sqrt(mean(reg.resid^2)),2)
    
  })
  
  ##### Ridge Regression ######
  
  output$summary45 <- renderPrint({
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    print(ridge)
    
  })
  
  
  output$plot25 <- renderPlot({ 
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    plot(ridge, label= TRUE)
    
  })
  
  output$plot26 <- renderPlot({ 
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    plot(ridge, xvar= "lambda", label= TRUE)
    
  })
  
  output$summary46 <- renderPrint({
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    
    ridge.coef <- coef(ridge, s=input$lambda_r, exact= TRUE, 
                       x = as.matrix(train1()[,-ncol(train1())]), y = as.matrix(train1()[,ncol(train1())]))
    
    print(ridge.coef)
    
  })
  
  output$plot27 <- renderPlot({ 
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    plot(ridge, xvar= "dev", label= TRUE)
    
  })
  
  output$plot28 <- renderPlot({ 
    
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    newx <- as.matrix(qq()[,-ncol(qq())])
    ridge.y <- predict(ridge, newx=newx, type = "response", s=input$lambda_r)
    
    plot(ridge.y, qq()$y , xlab = "Predicted", ylab = "Actual", main ="Ridge Regression")
    
  })
  
  output$view25 <- renderPrint({
    ridge <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=0)
    newx <- as.matrix(test1()[,-ncol(test1())])
    ridge.y <- predict(ridge, newx=newx, type = "response", s=input$lambda_r)
    ridge.resid <- ridge.y - test1()$y
    sqrt(mean(ridge.resid^2))
    
  })
  
  ##### LASSO #####
  
  output$summary51 <- renderPrint({
    
    lasso <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=1)
    print(lasso)
    
  })
  
  
  output$plot31 <- renderPlot({ 
    
    lasso <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=1)
    plot(lasso, label= TRUE)
    
  })
  
  output$summary52 <- renderPrint({
    
    lasso <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=1)
    lasso.coef <- coef(lasso, s=input$lambda_l, exact=TRUE, 
                       x = as.matrix(train1()[,-ncol(train1())]), y = as.matrix(train1()[,ncol(train1())]))
    lasso.coef
    
  })
  
  output$plot32 <- renderPlot({ 
    
    lasso <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=1)
    newx <- as.matrix(qq()[,-ncol(qq())])
    lasso.y <- predict(lasso, newx=newx , type="response", s=input$lambda_l)
    plot(lasso.y, as.matrix(qq()[,ncol(qq())]) , xlab = "Predicted", ylab = "Observed", main ="LASSO Regression")
  })
  
  output$view26 <- renderPrint({
    lasso <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                    family="gaussian",alpha=1)
    newx <- as.matrix(test1()[,-ncol(test1())])
    lasso.y <- predict(lasso, newx=newx, type="response", s=input$lambda_l)
    lasso.resid <- lasso.y - test1()$y
    sqrt(mean(lasso.resid^2))
    
  })
  
  #### Elastic net ####
  
  output$summary55 <- renderPrint({
    
    grid <- expand.grid(.alpha = seq(0, 1, by=.2), .lambda = seq(0.00, 0.2, by=0.02))
    control <- trainControl(method ="LOOCV")
    enet.train <- caret::train(y~., data= train1(),method="glmnet", trControl=control, tuneGrid=grid)
    enet.train 
    
  })
  
  output$summary56 <- renderPrint({
    
    enet <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                   family ="gaussian", alpha = input$alpha_e , lambda = input$lambda_e)
    enet.coef <- coef(enet, s=input$lambda_e, exact= TRUE)
    enet.coef
    
  })
  
  output$plot35 <- renderPlot({ 
    
    enet <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                   family ="gaussian", alpha = input$alpha_e, lambda = input$lambda_e)
    newx <- as.matrix(qq()[,-ncol(qq())])
    enet.y <- predict(enet, newx=newx, type="response", s=input$lambda_e)
    plot(enet.y, qq()$y, xlab = "Predicted", ylab = "Observed", main ="Elastic net")
  })
  
  output$summary57  <- renderPrint({
    
    enet <- glmnet(as.matrix(train1()[,-ncol(train1())]), as.matrix(train1()[,ncol(train1())]),
                   family ="gaussian", alpha = input$alpha_e, lambda = input$lambda_e)
    newx <- as.matrix(test1()[,-ncol(test1())])
    enet.y <- predict(enet, newx=newx, type="response", s=input$lambda_e)
    enet.resid <- enet.y - test1()$y
    sqrt(mean(enet.resid^2))
    
  })
  
  output$plot36 <- renderPlot({ 
    
    lasso.cv <- cv.glmnet(as.matrix(train1()[,-ncol(train1())]), 
                          as.matrix(train1()[,ncol(train1())]),nfolds = 3)
    
    plot(lasso.cv)
  })
  
  output$view29 <- renderPrint({
    
    lasso.cv <- cv.glmnet(as.matrix(train1()[,-ncol(train1())]), 
                          as.matrix(train1()[,ncol(train1())]),nfolds = 3)
    lasso.cv$lambda.min
    
  })
  
  output$view30 <- renderPrint({
    
    lasso.cv <- cv.glmnet(as.matrix(train1()[,-ncol(train1())]), 
                          as.matrix(train1()[,ncol(train1())]),nfolds = 3)
    lasso.cv$lambda.1se
    
  })
  
  output$summary58 <- renderPrint({
    
    lasso.cv <- cv.glmnet(as.matrix(train1()[,-ncol(train1())]), 
                          as.matrix(train1()[,ncol(train1())]),nfolds = 3)
    coef(lasso.cv, s="lambda.1se")
    
  })
  
  output$view31 <- renderPrint({
    
    lasso.cv <- cv.glmnet(as.matrix(train1()[,-ncol(train1())]), 
                          as.matrix(train1()[,ncol(train1())]),nfolds = 3)
    newx <- as.matrix(test1()[,-ncol(test1())])
    lasso.y.cv = predict(lasso.cv, newx=newx, type ="response", s="lambda.1se")
    lasso.cv.resid = lasso.y.cv - test1()$y
    sqrt(mean(lasso.cv.resid^2))
    
  })
  
  ##### MARS_II ######
  
  output$summary59 <- renderPrint({
    set.seed(input$seed)
    earth.fit <- earth(y~., data=train1(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1)
    summary(earth.fit)
    
  })
  
  output$plot38 <- renderPlot({ 
    set.seed(input$seed)
    earth.fit <- earth(y~., data=train1(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1)
    plotmo(earth.fit)
    
  })
  
  output$plot39 <- renderPlot({ 
    set.seed(input$seed)
    earth.fit <- earth(y~., data=train1(),pmethod="backward",nprune=20,nfold=10)
    plotd(earth.fit)
    
  })
  
  output$view33 <- renderPlot({ 
    set.seed(input$seed)
    earth.fit <- earth(y~., data=train1(), pmethod="cv", nfold=5, ncross=3,degree=1, minspan=-1)
    evimp(earth.fit)
  })
  
  ##### Random Forest_Regression ######
  
  output$summary32 <- renderPrint({
    rf.pros <- randomForest(y~., data=train1())
    rf.pros
  })
  
  
  output$plot13 <- renderPlot({ 
    rf.pros <- randomForest(y~., data=train1())
    plot(rf.pros)
    
  }) 
  
  output$summary33 <- renderPrint({ 
    rf.pros <- randomForest(y~., data=train1())
    which.min(rf.pros$mse)
  })
  
  output$summary34 <- renderPrint({
    rf.pros.2 <- randomForest(y~., data=train1(),ntree=input$tree)
    rf.pros.2 
  })
  
  
  output$summary35 <- renderPrint({
    rf.pros.2 <- randomForest(y~., data=train1(),ntree=input$tree)
    rf.pros.test <- predict(rf.pros.2, newdata=train1())
    rf.resid <- rf.pros.test - train1()$y
    sqrt(mean(rf.resid^2))
  })
  
  output$plot14 <- renderPlot({ 
    rf.pros.2 <- randomForest(y~., data=train1(),ntree=input$tree)
    varImpPlot(rf.pros.2, scale=T, main="Variable Importance Plot - PSA Score")
    
  })
  
  output$summary36 <- renderPrint({
    rf.pros.2 <- randomForest(y~., data=train1(),ntree=input$tree)
    importance(rf.pros.2)
  })
  
  
  output$summary37 <- renderPrint({
    rf.pros.2 <- randomForest(y~., data=train1(), ntree=input$tree)
    newx <- as.matrix(test1()[,-ncol(test1())])
    forest.test <- predict(rf.pros.2, newdata=newx)
    rf.resid = forest.test - test1()$y
    sqrt(mean(rf.resid^2))
  })
  
  
  output$plot80 <- renderPlot({ 
    
    rf.pros.2 <- randomForest(y~., data=train1(), ntree=input$tree)
    newx <- as.matrix(qq()[,-ncol(qq())])
    forest.y <- predict(rf.pros.2, newdata=newx , type="response")
    plot(forest.y, qq()$y , xlab = "Predicted", 
         ylab = "Observed", main ="Random Forest")
    
  })
  
  # random forest cluster
  
  output$summary110 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    rf
  })
  
  output$summary111 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    round(rf$proximity[1:5,1:5],4)
  })
  
  output$summary112 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    summary(rf)
  })
  
  output$summary113 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    dissMat <- sqrt(1 - rf$proximity)
    dissMat[1:2, 1:2]
  })
  
  output$summary114 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    dissMat <- sqrt(1 - rf$proximity)
    pamRF <- pam(dissMat, k=3)
    table(pamRF$clustering)
  })
  
  output$summary115 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    rf <- randomForest(df,ntree=input$cluster.tree, proximity = T)
    dissMat <- sqrt(1 - rf$proximity)
    pamRF <- pam(dissMat, k=3)
    table(pamRF$clustering, train()$type)
  })
  
  # H-cluster 
  
  output$plot101 <- renderPlot({
    par(mfrow=c(2,2))
    df <- as.data.frame(train()[,-ncol(train())])
    NbClust(df,distance=input$dist, min.nc=2, max.nc=input$nc,
            method=input$method, index="hubert")
  })
  
  output$plot102 <- renderPlot({
    df <- as.data.frame(train()[,-ncol(train())])
    dis <- dist(df, method= input$dist)
    hc <- hclust(dis, method=input$method)
    plot(hc, hang=-1, labels = FALSE)
    #   cu <- cutree(hc, input$cutree)
    #   ColorDendrogram(hc, y=cu , main="Dendrogram", branchlength = 40)
  })
  
  
  output$summary91 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    NbClust(df,distance=input$dist, min.nc=2, max.nc=input$nc,
            method=input$method, index="all", alphaBeale = 0.1)
    
  })
  
  
  output$summary92 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    dis <- dist(df, method= input$dist)
    hc <- hclust(dis, method=input$method)
    cu <- cutree(hc, input$cutree)
    table(cu)
  })
  
  output$summary93 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    dis <- dist(df, method= input$dist)
    hc <- hclust(dis, method=input$method)
    cu <- cutree(hc, input$cutree)
    table(cu,train()$type)
  })
  
  # k-means 
  
  output$plot104 <- renderPlot({
    par(mfrow=c(2,2))
    df <- as.data.frame(train()[,-ncol(train())])
    NbClust(df, min.nc=2, max.nc=input$nc,method="kmeans",index="hubert")
  })
  
  output$plot105 <- renderPlot({
    df <- as.data.frame(train()[,-ncol(train())])
    dis <- dist(df, method= input$dist)
    hc <- hclust(dis, method="kmeans")
    # plot(hc, hang=-1, labels = FALSE)
    cu <- cutree(hc, input$cu2)
    ColorDendrogram(hc, y=cu , main="Dendrogram", branchlength = 40)
  })
  
  output$plot106 <- renderPlot({
    df <- as.data.frame(train()[,-ncol(train())])
    km <- kmeans(df,input$group, nstart = input$nstart)
    clusplot(train()[,-ncol(train())], km$cluster, main='2D representation of the Cluster solution',
             color=TRUE, shade=TRUE, labels=2, lines=0)
  })
  
  
  output$summary95 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    km <- kmeans(df,input$group,nstart = input$nstart)
    km
  })
  
  
  output$summary96 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    km <- kmeans(df,input$group,nstart = input$nstart)
    table(km$cluster)
  })
  
  output$summary97 <- renderPrint({
    df <- as.data.frame(train()[,-ncol(train())])
    km <- kmeans(df,input$group,nstart = input$nstart)
    table(km$cluster, train()$type)
  })
  
  
  
  
  ##### DATA Tab ######
  
  
  output$data1 <- renderTable({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T)
    w <- na.omit(w) 
    w
    
  })
  
  ##### xGboost ######
  
  output$summary71 <- renderPrint({
    grid = expand.grid(
      nrounds=c(75,100),
      colsample_bytree = 1,
      min_child_weight = 1,
      eta = c(0.01, 0.1, 0.3), #0.3은 기본값
      gamma = c(0.5,0.25),
      subsample = 0.5,
      max_depth = c(2,3)
    )
    cntrl = trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE,
      returnData = FALSE,
      returnResamp = "final"
    )
    set.seed(input$seed)
    train.xgb = caret::train(
      x=as.matrix(train()[,-ncol(train())]), 
      y=as.matrix(train()[,ncol(train())]),
      trControl = cntrl,
      tuneGrid = grid,
      method = "xgbTree"
    )
    train.xgb
  })
  
  output$summary72 <- renderPrint({
    grid = expand.grid(
      nrounds=75, #input$nrounds,
      colsample_bytree = 1,
      min_child_weight = 1,
      eta = 0.1, # input$eta,
      gamma = 0.25, # input$gamma,
      subsample = 0.5,
      max_depth = 2 # input$max_depth
    )
    cntrl = trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE,
      returnData = FALSE,
      returnResamp = "final"
    )
    
    param <- list(objective = "binary:logistic",
                  booster = "gbtree",
                  eval_metric = "error",
                  eta =input$eta,
                  max_depth = input$max_depth,
                  subsample = 0.5,
                  colsample_bytree = 1,
                  gamma = input$gamma
    ) 
    
    x <- as.matrix(train()[,-ncol(train())])
    x
    # y <- ifelse(train()$type == levels(train()$type)[2], 1, 0)
    # y <- train()$type
    # train.mat <- xgb.DMatrix(data = x, label = y)
    # xgb.fit <- xgb.train(params = param, data=train.mat, nrounds = input$nrounds)
    # xgb.fit
    # impMatrix <- xgb.importance(feature_names = dimnames(x)[[2]], model = xgb.fit)
    # impMatrix
  })
  
  #### neural net ####
  
  output$plot81 <- renderPlot({ 
    par(mfrow=c(1,1))
    n <- names(train())
    form <- as.formula(paste("type ~",paste(n[!n %in% "type"], collapse = "+")))
    fit <- neuralnet(form , data=train(), hidden=input$hidden, threshold=0.01, 
                     err.fct= input$err,linear.output=TRUE)
    plot(fit)
  })
  
  output$summary75 <- renderPrint({
    n <- names(train())
    form <- as.formula(paste("type ~",paste(n[!n %in% "type"], collapse = "+")))
    fit <- neuralnet(form , data=train(), hidden=input$hidden, threshold=0.01, 
                     err.fct= input$err,linear.output=FALSE)
    fit$result.matrix
  })
  
  output$summary76 <- renderPrint({
    n <- names(train())
    form <- as.formula(paste("type ~",paste(n[!n %in% "type"], collapse = "+")))
    fit <- neuralnet(form , data=train(), hidden=input$hidden, threshold=0.01, 
                     err.fct= input$err,linear.output=FALSE)
    resultsTrain <- neuralnet::compute(fit, train()[,1:(ncol(test())-1)])
    predTrain <- resultsTrain$net.result
    predTrain <- ifelse(predTrain >= 0.5, 1, 0)
    table(predTrain, train()$type)
  })
  
  output$summary77 <- renderPrint({
    n <- names(train())
    form <- as.formula(paste("type ~",paste(n[!n %in% "type"], collapse = "+")))
    fit <- neuralnet(form , data=train(), hidden=input$hidden, threshold=0.01, 
                     err.fct= input$err,linear.output=FALSE)
    resultsTest <- neuralnet::compute(fit, test()[,1:(ncol(test())-1)])
    predTest <- resultsTest$net.result
    predTest <- ifelse(predTest >= 0.5, 1, 0)
    table(predTest, test()$type)
  })
  
  #### Ensemble ####
  
  
  output$summary120 <- renderPrint({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    colnames(w)[ncol(w)] <- "type"
    set.seed(input$seed)
    split <- createDataPartition(y=w$type, p= input$ratio, list=F)
    train <- w[split,]
    # levels(train()$type) <- make.names(levels(factor(ee()$type)))
    control <- trainControl(method = "cv", number = input$cv, savePredictions = "final", classProbs = TRUE,
                            index = createResample(train$type, input$cv), sampling = "up", 
                            summaryFunction = twoClassSummary)
    models <- caretList(type~., data=train, trControl = control, metric="ROC",
                        methodList = c(input$mod))
    models
    
  })
  
  output$summary121 <- renderPrint({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    colnames(w)[ncol(w)] <- "type"
    set.seed(input$seed)
    split <- createDataPartition(y=w$type, p= input$ratio, list=F)
    train <- w[split,]
    control <- trainControl(method = "cv", number = input$cv, savePredictions = "final", classProbs = T,
                            index = createResample(train$type,input$cv), sampling = "up", 
                            summaryFunction = twoClassSummary)
    models <- caretList(type~., data=train, trControl = control,metric="ROC",
                        methodList = c(input$mod))
    modelCor(resamples(models))
    
  })
  
  output$summary122 <- renderPrint({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    colnames(w)[ncol(w)] <- "type"
    set.seed(input$seed)
    split <- createDataPartition(y=w$type, p= input$ratio, list=F)
    train <- w[split,]
    test <- w[-split,]
    control <- trainControl(method = "cv", number = input$cv, savePredictions = "final", classProbs = T,
                            index = createResample(train$type,input$cv), sampling = "up", 
                            summaryFunction = twoClassSummary)
    models <- caretList(type~., data=train ,trControl = control,metric="ROC",
                        methodList = c(input$mod))
    model_preds <- lapply(models,predict, newdata=test, type="prob")
    model_preds <- lapply(model_preds, function(x) x[,"NG"])
    model_preds <- data.frame(model_preds)
    stack <- caretStack(models, method="glm",metric="ROC", trControl=trainControl(
      method = "boot", number = 5, savePredictions = "final", classProbs = TRUE,
      summaryFunction = twoClassSummary
    ))
    summary(stack)
  })
  
  output$summary123 <- renderPrint({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T) %>% na.omit()
    colnames(w)[ncol(w)] <- "type"
    set.seed(input$seed)
    split <- createDataPartition(y=w$type, p= input$ratio, list=F)
    train <- w[split,]
    test <- w[-split,]
    control <- trainControl(method = "cv", number = input$cv, savePredictions = "final", classProbs = T,
                            index = createResample(train$type,input$cv), sampling = "up", 
                            summaryFunction = twoClassSummary)
    models <- caretList(type~., data=train, trControl = control,metric="ROC",
                        methodList = c(input$mod))
    model_preds <- lapply(models,predict, newdata=test, type="prob")
    model_preds <- lapply(model_preds, function(x) x[,"NG"])
    model_preds <- data.frame(model_preds)
    
    stack <- caretStack(models, method="glm",metric="ROC", trControl=trainControl(
      method = "boot", number = 5, savePredictions = "final", classProbs = TRUE,
      summaryFunction = twoClassSummary
    ))
    prob <- 1-predict(stack, newdata=test, type="prob")
    model_preds$ensemble <- prob
    colAUC(model_preds, test$type)
  })
  
  
  output$summary124 <- renderPrint({
    
  })   
  
  #### data tab ####
  
  output$data1 <- renderTable({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    w <- read.xlsx(inFile$datapath,input$sheet,header=T)
    w <- na.omit(w) 
    w 
  })
  
  
}
