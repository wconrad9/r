library(tidyverse)
library(rpart)
library(rattle)
library(ipred)

bagging.m1 <- bagging(price ~ carat,
                                    data = my.data.clean,
                                    nbagg = 10,
                                    coob = TRUE)

bagging.m1


my.data <- read_csv(file.choose())

#view(avocado.data)
#colnames(avocado.data)[5:7] <- c("Sold1", "Sold2", "Sold3")



my.data.clean <- my.data %>%
  
  na.omit() %>%
  head(1000)



#We need to write a function that performs a bagging of simple linear models

#big picture: our bagging function will sample from the data used to create

#the linear model a number of times, as specified by the user,my

#create a number of individual linear models with each sample,

#and then average the results to produce the final bagged model.

myBag <- function(predictor, response, nBaggs) {

  inBagIndexes <- list()
  
  sampledModels <- data.frame(slope = numeric(),
                              yint = numeric())

  for(i in 1:nBaggs) {
    
    indexes <- sample(1:nrow(my.data.clean),
                      size = nrow(my.data.clean),
                      replace = TRUE)
    
    inBagIndexes[[i]] <- c(indexes)
    #print(inBagIndexes[[i]])
    
    trainingData <- my.data.clean[indexes, ]
    
    #manual linear model
    mean.x <- mean(trainingData[[predictor]])
    #mean.x <- mean(trainingData$"Total Volume")
    #mean.y <- trainingData %>% select(response) %>% mean()
    mean.y <- mean(trainingData[[response]])
    #x <- trainingData$predictor
    #y <- trainingData$response

    x <- trainingData %>% select(predictor)
    y <- trainingData %>% select(response)
    
    totalSumSquares <- sum((y - mean.y)^2)
    
    slope <- sum((x - mean.x) * (y - mean.y)) / sum((x-mean.x)^2)
    yint <- mean.y - slope*mean.x
    
    sampledModels[i, ] <- c(slope, yint)
  }
  
  print(sampledModels)
  #Average the bags
  
  slope.final <- mean(sampledModels$slope)
  yint.final <- mean(sampledModels$yint)
  
  #Calculate OOB error
  resids <- data.frame(residual = double())
  for(i in 1:nrow(my.data.clean)){
    
    predictions <- data.frame(prediction = double())
    for(j in 1:nBaggs){
      
      if(i %in% inBagIndexes[[j]]){}
      
      else {
        predict <- my.data.clean[[i, predictor]] * sampledModels[j, c("slope")] + sampledModels[j, c("yint")]
        #print(paste0("sampledModels: ", sampledModels[j, "slope"]))
        #print(paste0("predicor: ", my.data.clean[i, predictor]))
        #print(paste0("predict: ", predict))
        
        predictions <- rbind(predictions, predict)
      }
    }
    
    mean_prediction <- mean(predictions[,1])
    
    
    resid <- mean_prediction - my.data.clean[[i, response]]
    
    resids[i,] <- c(resid)
  }
  
  #Make prediction
  residualSumSquares = sum(((resids$residual)^2), na.rm = TRUE)
  print(paste0("RSS: ", residualSumSquares))
  print(paste0("TSS: ", totalSumSquares))
  
  
  rSquared = 1 - (residualSumSquares/totalSumSquares)
  
  rmse = sqrt((sum((resids$residual)^2, na.rm = TRUE))/nrow(my.data.clean))
  
  
  return(rmse)
}


# Take 70% of the data for training
smp_size <- floor(0.7 * nrow(my.data.clean))

set.seed(123)
train.indices <- sample(seq_len(nrow(my.data.clean)), size = smp_size)

# Get train and test sets
train <- my.data.clean[train.indices, ]
test <- my.data.clean[-train.indices, ]


myBag2 <- function(predictor, response, nBaggs) {
  
  inBagIndexes <- list()
  
  sampledModels <- data.frame(slope = numeric(),
                              yint = numeric())
  
  for(i in 1:nBaggs) {
    
    indexes <- sample(1:nrow(train),
                      size = nrow(train),
                      replace = TRUE)
    
    inBagIndexes[[i]] <- c(indexes)
    #print(inBagIndexes[[i]])
    
    trainingData <- my.data.clean[indexes, ]
    
    #manual linear model
    mean.x <- mean(trainingData[[predictor]])
    #mean.x <- mean(trainingData$"Total Volume")
    #mean.y <- trainingData %>% select(response) %>% mean()
    mean.y <- mean(trainingData[[response]])
    #x <- trainingData$predictor
    #y <- trainingData$response
    
    x <- trainingData %>% select(predictor)
    y <- trainingData %>% select(response)
    
    totalSumSquares <- sum((y - mean.y)^2)
    
    slope <- sum((x - mean.x) * (y - mean.y)) / sum((x-mean.x)^2)
    yint <- mean.y - slope*mean.x
    
    sampledModels[i, ] <- c(slope, yint)
  }
  
  print(sampledModels)
  #Average the bags
  
  slope.final <- mean(sampledModels$slope)
  yint.final <- mean(sampledModels$yint)
  
  
  resids <- data.frame(residual = double())
  for(i in 1:nrow(test)){
    
    print(i)
    
    pred <- test[[i,predictor]]*slope.final + yint.final
    resid <- pred - test[[i,response]]
    
    resids[i,] <- c(resid)
  }
  
  
  #Make prediction
  residualSumSquares = sum(((resids$residual)^2), na.rm = TRUE)
  print(paste0("RSS: ", residualSumSquares))
  print(paste0("TSS: ", totalSumSquares))
  
  
  rSquared = 1 - (residualSumSquares/totalSumSquares)
  print(paste0("rSquared: ", rSquared))
  
  rmse = sqrt((sum((resids$residual)^2, na.rm = TRUE))/nrow(train))
  
  
  return(rmse)
}



lm1 <- lm(AveragePrice ~ `Total Volume`,
          data = my.data.clean)
summary(lm1)

