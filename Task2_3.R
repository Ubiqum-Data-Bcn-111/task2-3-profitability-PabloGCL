################### Libraries #######################

rm(list=ls(all=TRUE))

pacman::p_load(caret, lattice, ggplot2, ModelMetrics, mlbench, RcppRoll, bindrcpp, backports, ddalpha, 
               DEoptimR, dimRed, gower, readr, rpart, cellranger, rpart.plot, plotly, corrplot, dplyr)


################## Calling Documents #####################

setwd("C:/Users/Usuario/Desktop/Big Data/2.3") 

OGexisting <- read_delim("existingproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)
View(OGexisting)

OGnew <- read_delim("newproductattributes2017.2.csv",",", escape_double = FALSE, trim_ws = TRUE)
View(OGnew)


################ Warranty & Rest ####################

Warr <- filter(OGexisting, ProductType=="ExtendedWarranty")
NoWarr <- filter(OGexisting, ProductType!="ExtendedWarranty")


################## Dummify the data ######################

NoWarrDummy <- dummyVars(" ~ .", data = NoWarr)
NoWarrDummy <- data.frame(predict(NoWarrDummy, newdata = NoWarr))

newDummy <- dummyVars(" ~ .", data = OGnew)
newDummy <- data.frame(predict(newDummy, newdata = OGnew))

str(NoWarrDummy)
str(newDummy)


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

OGnew$BestSellersRank <- NULL
OGnew$x5StarReviews <- NULL
OGnew$ProductType <- NULL
OGnew$ProductNum <- NULL

NoWarr$BestSellersRank <- NULL
NoWarr$x5StarReviews <- NULL
NoWarr$ProductType <- NULL
NoWarr$ProductNum <- NULL

corrOGExist <- cor(OGexisting) 
corrplot(corrOGExist)

corrOGNew <- cor(OGnew) 
corrplot(corrOGNew)

corrNoWarr <- cor(NoWarr) 
corrplot(corrNoWarr)

#################### Decision Tree ##########################

set.seed(123)
DecisionTree <- rpart(
  Volume ~ ., 
  data = NoWarr, 
  control = rpart.control(minsplit = 2)
  )

par(xpd = NA, mar = rep(0.7, 4)) 
plot(DecisionTree, compress = TRUE)
text(DecisionTree, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)

rpart.plot(DecisionTree) #, box.palette="RdBu")


set.seed(123)
DTDummy <- rpart(
  Volume ~ ., 
  data = NoWarrDummy, 
  control = rpart.control(minsplit = 2)
)

par(xpd = NA, mar = rep(0.7, 4)) 
plot(DTDummy, compress = TRUE)
text(DTDummy, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)

rpart.plot(DTDummy) #, box.palette="RdBu")


#################### Linear Model ##################

set.seed(123)
inTrain <- createDataPartition(
  y = NoWarr$Volume,
  p = .75,
  list = FALSE)

str(inTrain)

training <- NoWarr[ inTrain,]
testing  <- NoWarr[-inTrain,]

nrow(training)
nrow(testing)

LModel <- list()
for(i in 1:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel1 <- lm(Volume ~ 0+training[,i], training)
    metrics <- postResample(LinearModel1)
    metrics.acum <- cbind(metrics.acum, metrics)
}
}


LModel <- list()
for(i in 1:ncol(training)){
  if(is.numeric(training[,i])){
    LinearModel <- lm(Volume ~ 0+training[,i], training)
    #fit <- train(brand~salary_bin+age_bin, data=training, method="rf", 
    #            metric="Kappa", tuneLength=10, trControl=fitControl, 
    #           ntree=ntree)
    #key <- toString(ntree) 
    
    LMmodel[[i]] <- LinearModel
    names(LMmodel)[i] <- colnames(existing[i])
  }
}


set.seed(123)

LMmodel2 <- list()
c <- c()
for(i in 1:ncol(NoWarr)){
  for(j in i:ncol(NoWarr){
  if(is.numeric(NoWarr[,i])){
    LinearModel2 <- lm(Volume ~ 0+NoWarr[,i]+NoWarr[,j], NoWarr)
    c <- c+1
    LMmodel2[[c]] <- LinearModel2
    names(LMmodel2)[c] <- colnames(existing[i])+colnames(existing[j])
  }
 }
}

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
