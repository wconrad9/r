library(tidyverse)
library(ggplot2)
library(readxl)
library(gridExtra)
library(class)
library(stringr)
library(matlab)


cars <- read_csv('OLX_Car_Data_CSV.csv')

view(cars)

#1) Let's use brand and KMs driven to predict the car model. Use KNN to predict car model.

#train.use <- train[-1]
#test.use <- test[-1]


cars.clean <- cars %>%
  na.omit()

train <- cars.clean[, c(4,6)] %>%
  head(12500)

test <- cars.clean[-c(1:12500), c(4,6)]

pred_labels <- cars.clean[c(1:12500), 1]

pred_model <- knn(train, test, pred_labels$Brand, k = 112)

view(pred_model)

#Ok, model created. Now we have to see how those guesses match our actuals.
#expand.grid


test.actuals <- cars.clean[-c(1:12500), 1]

view(test.actuals)

prop.set <- data.frame(predictions = pred_model,
                       actuals = test.actuals$Brand)



view(prop.set)




prop.correct <- mean(as.character(prop.set$predictions) == as.character(prop.set$actuals))

view(prop.correct)


#Leave-One-Out Cross-Validation

cars.clean.sample <- cars.clean[sample(nrow(cars.clean), 1000), ]


LOOCV.prop <- NULL
pred <- NULL

for(k in 1:58) {
  
  for(i in 1:nrow(cars.clean.sample)){
    #remove ith row and use for testing
    current.test <- cars.clean.sample[i, c(4,6)]
    
    #use remaining data for training
    current.train <- cars.clean.sample[-i, c(4,6)]
    current.train.labels <- cars.clean.sample[-i, 1]
    
    #do knn with above 2 data sets
    pred[i] <- as.character(knn(current.train, current.test, current.train.labels$Brand, k))
  }
  
  #Calculate the accuracy of knn for given k value
  LOOCV.prop[k] <- mean(as.character(pred) == as.character(cars.clean.sample$Brand))
  print(k)
}


#Optimal K: 58

view(pred)
view(LOOCV.prop)

cars.clean.sample %>%
  filter(Brand %in% c('Suzuki','Toyota')) %>%
  summarize(min = min(Price))

hist(LOOCV.prop)

#Making visualizations

km.grid <- seq(from = 1, to = 500000, by = 10000)
price.grid <- seq(from = 50000, to = 3000000, by = 50000)

grid <- expand.grid(km.grid, price.grid)

view(grid)

#mode function
MaxTable <- function(x){
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

#Creating scaled versions of variables and then writing a function for testing

cars.clean.sample.new <- cars.clean.sample %>%
  mutate(km.scaled = (`KMs Driven` - mean(`KMs Driven`))/sd(`KMs Driven`),
         price.scaled = (Price - mean(Price))/sd(Price))

cars.scaled <- function(test.km, test.price, k) {
  test.km.scaled <- (test.km - mean(cars.clean.sample$`KMs Driven`))/sd(cars.clean.sample$`KMs Driven`)
  test.price.scaled <- (test.price - mean(cars.clean.sample$Price))/sd(cars.clean.sample$Price)
  
  cars.clean.sample.new %>%
    mutate(distance = ((km.scaled - test.km.scaled)^2 + (price.scaled - test.price.scaled)^2) ) %>%
    arrange(distance) %>%
    head(k) %>%
    summarize(knn.result = MaxTable(Brand)) %>%
    .[[1]] %>%
    return()
}


#Var1 = KM Driven
#Var2 = Price

knn.k58 <- grid %>%
  group_by(Var1, Var2) %>%
  mutate(prediction = cars.scaled(Var1, Var2, 58))

knn.k10 <- grid %>%
  group_by(Var1, Var2) %>%
  mutate(prediction = cars.scaled(Var1, Var2, 10))

cars.clean.sample.new.filtered <- cars.clean.sample.new %>%
  filter(Price <= 3000000) %>%
  filter(`KMs Driven` <= 500000) %>%
  filter(Brand %in% c('Suzuki', 'Toyota', 'Honda')) %>%
  view()

knn.k58 %>%
  ggplot(aes(x = Var1,
             y = Var2)) +
  geom_point(aes(color = factor(prediction)),
             size = 2,
             alpha = 0.3) +
  geom_point(data = cars.clean.sample.new.filtered,
             mapping = aes(x = `KMs Driven`,
                           y = Price,
                           color = factor(cars.clean.sample.new.filtered$Brand,
                                          levels = c('Toyota', 'Suzuki', 'Honda')),
             size = 1))

knn.k10 %>%
  ggplot(aes(x = Var1,
             y = Var2)) +
  geom_point(aes(color = factor(prediction)),
             size = 2,
             alpha = 0.3) +
  geom_point(data = cars.clean.sample.new.filtered,
             mapping = aes(x = `KMs Driven`,
                           y = Price,
                           color = factor(cars.clean.sample.new.filtered$Brand,
                                          levels = c('Toyota', 'Suzuki')),
                           size = 1))


help(factor)

