################### Libraries #######################

rm(list=ls(all=TRUE))

pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, ddalpha, DEoptimR, 
               dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, corrplot, dplyr, randomForest, MASS, forestFloor)


################## Calling Documents #####################

setwd("C:/Users/Usuario/Desktop/Big Data/2.3") 

OGexisting <- read_delim("existingproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)

OGnew <- read_delim("newproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)


################ Product Size #####################

OGexisting$ProductSize <- OGexisting$ProductHeight*OGexisting$ProductWidth*OGexisting$ProductDepth
OGnew$ProductSize <- OGnew$ProductHeight*OGnew$ProductWidth*OGnew$ProductDepth

OGexisting$ProductHeight <- NULL
OGexisting$ProductWidth <- NULL
OGexisting$ProductDepth <- NULL

OGnew$ProductHeight <- NULL
OGnew$ProductWidth <- NULL
OGnew$ProductDepth <- NULL


################ Warranty & Rest ####################

Warr <- filter(OGexisting, ProductType=="ExtendedWarranty")
NoWarr <- filter(OGexisting, ProductType!="ExtendedWarranty")

NewWarr <- filter(OGnew, ProductType=="ExtendedWarranty")
NewNoWarr <- filter(OGnew, ProductType!="ExtendedWarranty")


################## Dummify the data ######################

NoWarrDummy <- dummyVars(" ~ .", data = NoWarr)
NoWarrDummy <- data.frame(predict(NoWarrDummy, newdata = NoWarr))

newDummy <- dummyVars(" ~ .", data = NewNoWarr)
newDummy <- data.frame(predict(newDummy, newdata = NewNoWarr))


################## Correlation ###################

  ## Dummy Variables
NoWarrDummy$BestSellersRank <- NULL
NoWarrDummy$x5StarReviews <- NULL
NoWarrDummy$ProductNum <- NULL


newDummy$BestSellersRank <- NULL
newDummy$x5StarReviews <- NULL
newDummy$ProductNum <- NULL


corrNoWarrDummy <- cor(NoWarrDummy) 
corrplot(corrNoWarrDummy)

corrNewDummy <- cor(newDummy) 
corrplot(corrNewDummy)

  ## Before Dummy 
OGexisting$BestSellersRank <- NULL
OGexisting$x5StarReviews <- NULL
OGexisting$ProductType <- NULL
OGexisting$ProductNum <- NULL

NewNoWarr$BestSellersRank <- NULL
NewNoWarr$x5StarReviews <- NULL
NewNoWarr$ProductType <- NULL
NewNoWarr$ProductNum <- NULL

NoWarr$BestSellersRank <- NULL
NoWarr$x5StarReviews <- NULL
NoWarr$ProductType <- NULL
NoWarr$ProductNum <- NULL

corrOGExist <- cor(OGexisting) 
corrplot(corrOGExist)

corrNew <- cor(NewNoWarr) 
corrplot(corrNew)

corrNoWarr <- cor(NoWarr) 
corrplot(corrNoWarr)

#################### Decision Tree ##########################

set.seed(123)
DecisionTree <- rpart(
  Volume ~ ., 
  data = NoWarr, 
  control = rpart.control(minsplit = 2)
  )

rpart.plot(DecisionTree) #, box.palette="RdBu")


set.seed(123)
DTDummy <- rpart(
  Volume ~ ., 
  data = NoWarrDummy, 
  control = rpart.control(minsplit = 2)
)

rpart.plot(DTDummy) #, box.palette="RdBu")


#################### Variable Weight ###############

set.seed(123)
train=sample(1:nrow(NoWarr), nrow(NoWarr)*0.75)
NoWarr.rf=randomForest(Volume ~ . , data = NoWarr, subset = train , importance=TRUE, ntree=500)

test <- NoWarr[-train,]
NoWarrpred=predict(NoWarr.rf, test)

importance(NoWarr.rf)
varImpPlot(NoWarr.rf)


#################### Linear Model ##################

set.seed(123)
inTrain <- createDataPartition(
  y = NoWarrDummy$Volume,
  p = .75,
  list = FALSE)

training <- NoWarrDummy[ inTrain,]
testing <- NoWarrDummy[-inTrain,]


control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        classProbs=TRUE)
##----------------------
LModel <- list()
for(i in 12:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel1 <- train(Volume ~ 0+training[,i], 
                          data=training,
                          method="lm",
                          tuneLength=10
                          #trControl=control
                          )
    PredLM1 <- predict(LinearModel1, testing)
    metrics <- postResample(PredLM1, testing)
    #metrics.acum <- cbind(metrics.acum, metrics)
}
}

#RFmodel <- list()
#for(i in 12:ncol(training)){
 # if(is.numeric(training[,i])){
    RFmodel <- train(Volume ~ x4StarReviews+PositiveServiceReview, 
                          data=training,
                          method="rf",
                          tuneLength=10,
                          trControl=control
    )
    PredRF <- predict(RFmodel, testing)
    RFmetrics <- postResample(PredRF, testing)
    RFmetrics.acum <- cbind(metrics.acum, metrics)
  #}
#}

##------------

    
  # One Variable #  
temp <- names(training[,1:(ncol(training)-1)]) 
metrics.acumLM1 <-  c()   

for(i in 12:(ncol(training)-1)){
  if(is.numeric(training[,i])){
    LinearModel1 <- lm(as.formula(paste(c("Volume ~ 0+", temp[i]))), training)
    PredLM1 <- predict(LinearModel1, testing)
    metricsLM1<-postResample(PredLM1, testing$Volume)
    metrics.acumLM1<-cbind(metrics.acumLM1, metricsLM1)
  }
}

#names(metrics.acumLM1)[,1:ncol(metrics.acumLM1)] <- colnames(training[1:(ncol(training)-1)])


PredictionModels <- function(VBLEpredicted, VBLEnvariables){
  
  LModel <- train(as.formula(paste(c(VBLEpredicted, "~ 0+", temp[i]))), training, method=VBLEmethod)
}


metrics.acumRF <-  c()
#for(i in 12:(ncol(training)-1)){
  #if(is.numeric(training[,i])){
    RFModel <- train(Volume ~., 
                     data=training, method="rf", 
                     ntrees=100, tuneLength=10, trControl=control, importance= TRUE)
    PredRF <- predict(RFModel, testing)
    metricsRF<-postResample(PredRF, testing$Volume)
    metrics.acumRF<-cbind(metrics.acumRF, metricsRF)
    importance(PredRF)
#  }
#}


# Two Variables #
temp <- names(training[,1:(ncol(training)-1)]) 
metrics.acumLM2 <-  c()   
set.seed(123)
LMmodel2 <- list()
for(i in 12:ncol(training)){
  for(j in i:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel2 <- lm(as.formula(paste(c("Volume ~ 0+", temp[i],temp[j]))), training)
    PredLM2 <- predict(LinearModel2, testing)
    metricsLM2<-postResample(PredLM2, testing$Volume)
    metrics.acumLM2<-cbind(metrics.acumLM2, metricsLM2)
  }
 }
}

predict(LinearModel2, testing)
<-postResample()
<-cbind()

LMmodel1
LinearModel1 <- lm(Volume ~ ., existing)

summary(LinearModel)
coefficients(LinearModel) # model coefficients
confint(LinearModel, level=0.95) # CIs for model parameters 
fitted(LinearModel) # predicted values
residuals(LinearModel) # residuals
anova(LinearModel) # anova table 
vcov(LinearModel) # covariance matrix for model parameters 
influence(LinearModel) # regression diagnostics

ggplot(existing, aes(x=x4StarReviews, y=Volume)) +geom_point() + geom_smooth(method='lm', se=FALSE)


################## Plots #######################

for(i in 1:ncol(existing)){
  if(is.numeric(existing[,i])){
    qqnorm(existing[,i],main=paste("Normal Q-Qplot of", colnames(existing)[i])) #survey[,i] to retrieve column i 
    qqline(existing[,i], col="red") #add qqnormal line
    hist(existing[,i], main=paste("Histogram of", colnames(existing)[i]),xlab=colnames(existing)[i])
  }
}

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'




for (i in 13:ncol(existing)){
  plot_ly(existing[i], main=paste('Boxplot of', colnames(existing[i]),'in regards to Volume'), type="box")
  }

plot_ly(y=NoWarr$Price, type="box")

plot_ly(type="box") %>%
add_boxplot(y = existing[27], jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
            marker = list(color = 'rgb(7,40,89)'),
            line = list(color = 'rgb(7,40,89)'),
            name = "Boxplot of in regards to Volume") %>%
  layout(title = "Box Plot Styling Outliers")


################# Models ##################

inTrain <- createDataPartition(
  y = NoWarr$Volume,
  p = .75,
  list = FALSE)

str(inTrain)

training <- NoWarr[ inTrain,]
testing  <- NoWarr[-inTrain,]

nrow(training)
nrow(testing)
