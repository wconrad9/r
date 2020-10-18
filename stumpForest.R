library(tidyverse)
library(rpart)
library(rattle)
library(fmsb)
library(caret)
library(ggplot2)

###Let's do this thing!

?rpart

titanic <- read_csv(file.choose())
titanic.test.set <- read_csv(file.choose())

titanic$Survived <- factor(titanic$Survived)

titanic.test <- titanic.clean[ ,c(2,3,5,7)]

summary(titanic)

colnames(titanic)

variablesTest <- c("Sex", "Age", "SibSp")

titanic %>%
  filter(Sex == "female") %>%
  summarise(prop_survived = mean(Survived))

stump1 <- rpart(titanic.test,
                na.action = na.omit,
                maxdepth = 1)

var.imp.test <- data.frame(stump1$variable.importance)

nrow(var.imp.test)

predsTest <- ifelse(predict(stump1, titanic.test.set)[,2] > .5, 1, 0)

stump1$variable.importance

stump1.test <- data.frame(stump1$variable.importance)

rownames(stump1.test)

test <- list(5)

j = 1
for(i in rownames(stump1.test)){
  test[[j]] <- data.frame(predictor = i,
                          imp = 10)
  j = j+1
}

for(i in 1:length(test)){
  test[[i]][,2] <- test[[i]][,2] + 5
  print(test[[i]][,2])
}

fancyRpartPlot(stump1)

predsTest <- ifelse(predict(stump1, titanic.test.set)[,2] > .5, 1, 0)

testForest <- list(10)

for(i in 1:10) {
  
  testForest[[i]] <- stump1
  
}

length(testForest)


fancyRpartPlot(testForest[[1]])


length(colnames(titanic))

varsTest <- sample(1:ncol(titanic),
                  size = 5,
                  replace = FALSE)

varsTest

#create test set
titanic.pruned <- subset(titanic, select = -c(PassengerId,Name, Ticket,Cabin))

indexes <- sample(nrow(titanic.pruned),
                  size = nrow(titanic.pruned)*.7)

titanic.pruned.train <- titanic.pruned[indexes, ]
titanic.pruned.test <- titanic.pruned[-indexes, ]


###Begin Function###

myStumps <- function(nStumps, nVar) {
  
  forest <- list(nStumps)
  preds <- list(nStumps)
  stumps.var.imp <- list(nStumps)
  
  for(i in 1:nStumps){
    
    vars <- sample(2:ncol(titanic.pruned.train),
                  size = nVar,
                  replace = FALSE)
    
    titanic.stump <- subset(titanic.pruned.train, select = c(Survived, vars))
    #view(titanic.stump)
    
    forest[[i]] <- rpart(titanic.stump,
                        na.action = na.omit,
                        minsplit = 0,
                        minbucket = 0,
                        cp = .0001,
                        maxdepth = 1)
    
    if(nrow(forest[[i]]$frame) == 3) {
      preds[[i]] <- ifelse(predict(forest[[i]], titanic.pruned.test)[,2]>.5,1,0)
      stumps.var.imp[[i]] <- data.frame(forest[[i]]$variable.importance)
    }
  }
  

  final.preds <- NULL
  
  for(i in 1:nrow(titanic.pruned.test)){
    
    final.pred <- 0
    nvotes <- 0
    
    for(j in 1:length(preds)){
      
      
    if(!(is.null(preds[[j]]))) {

      current_pred <- data.frame(preds[[j]])
      
        if(current_pred[i,] <= 1) {
          final.pred = final.pred + current_pred[i,]
          nvotes = nvotes + 1
        }
      
      }
      
    }
    
    final.pred = final.pred / nvotes
    
    final.preds[i] <- ifelse(final.pred >.5, 1,
                             ifelse(final.pred == .5, round(runif(1,0,1)), 0))
  }
  
  #Variable Importance
  var.imp <- list(ncol(titanic.pruned.train)-1)
  
  #iterate through each possible predictor
  k = 1
  for(i in colnames(titanic.pruned.train[,-1])){
  
    var.imp[[k]] <- data.frame(predictor = i,
                               imp = 0)
    print(paste0("predictor: ", i))
    
    #iterate through each stump
    for(j in 1:length(stumps.var.imp)) {
          
          if( !(is.null(stumps.var.imp[[j]][i,])) && !(is.na(stumps.var.imp[[j]][i,])) ) {
            
            print(stumps.var.imp[[j]][i,])
            
            var.imp[[k]][ ,2] = var.imp[[k]][ , 2] + stumps.var.imp[[j]][i,]
    
        }
    }
    
    k = k + 1
  }
  
  
  #merge all importance matrices
  overall <- data.frame(predictor = NULL,
                        imp = NULL)
  for(i in 1:length(var.imp)){
    overall <- rbind(overall, var.imp[[i]])
  }
  
  overall <- data.frame(overall)
  
  true_vals <- titanic.pruned.test$Survived
  
  accuracy <- mean(final.preds == true_vals)
  
  stumps_performance <- table(final.preds, true_vals)
  
  print(paste0("Kappa: ", Kappa.test(stumps_performance)))
  print(paste0("accuracy: ", accuracy))
  #return(overall)
}


myStumps(8,4) %>%
  ggplot(aes(x = predictor,
             y = imp,
             fill = predictor)) +
  geom_bar(stat = 'identity') +
  xlab('Predictor') +
  ylab('Importance') +
  ggtitle('Random Stump Forest Variable Importance') +
  theme_light()


tune.grid <- expand.grid(mtry = 3:5,
                         splitrule = 'gini',
                         min.node.size = 1)

rf_model1 <- train(Survived ~ .,
                   data = titanic.pruned.train,
                   num.trees = 100,
                   na.action = na.omit,
                   tuneGrid = tune.grid,
                   method = "ranger")

compare1 <- rf_model1$results[,-c(2,3,6,7)]

compare1 <- compare1 %>%
  mutate(trees = 100)

rf_model2 <- train(Survived ~ .,
                   data = titanic.pruned.train,
                   num.trees = 10,
                   na.action = na.omit,
                   tuneGrid = tune.grid,
                   method = "ranger")

compare2 <- rf_model2$results[,-c(2,3,6,7)]

compare2 <- compare2 %>%
  mutate(trees = 10)

rf_model3 <- train(Survived ~ .,
                   data = titanic.pruned.train,
                   num.trees = 25,
                   na.action = na.omit,
                   tuneGrid = tune.grid,
                   method = "ranger")


compare3 <- rf_model3$results[,-c(2,3,6,7)]

compare3 <- compare3 %>%
  mutate(trees = 25)

compare.ranger <- rbind(compare1,compare2, compare3)

compare.ranger <- compare.ranger %>%
  mutate(method = 'ranger')

compare %>%
  ggplot(aes(x = trees,
             y = Accuracy)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(mtry), cols = vars(trees))


myStumps(10,4)

stump.compare1 <- data.frame(mtry = 3,
                            Accuracy = .7687,
                            Kappa = .4622,
                            trees = 10)

stump.compare2 <- data.frame(mtry = 3,
                             Accuracy = .7799,
                             Kappa = .5156,
                             trees = 25)

stump.compare3 <- data.frame(mtry = 3,
                             Accuracy = .75,
                             Kappa = .4422,
                             trees = 100)

stumps.mtry3 <- rbind(stump.compare1, stump.compare2, stump.compare3)

stumps.mtry3 %>%
  mutate(method = 'myStumps')

stump.compare4 <- data.frame(mtry = 4,
                             Accuracy = .7463,
                             Kappa = .4456,
                             trees = 10)

stump.compare5 <- data.frame(mtry = 4,
                             Accuracy = .7799,
                             Kappa = .5156,
                             trees = 25)

stump.compare6 <- data.frame(mtry = 4,
                             Accuracy = .75,
                             Kappa = .4422,
                             trees = 100)


myStumps(100,5)

stump.compare7 <- data.frame(mtry = 5,
                             Accuracy = .7463,
                             Kappa = .5156,
                             trees = 10)

stump.compare8 <- data.frame(mtry = 5,
                             Accuracy = .7799,
                             Kappa = .5156,
                             trees = 25)

stump.compare9 <- data.frame(mtry = 5,
                             Accuracy = .7799,
                             Kappa = .5156,
                             trees = 100)

stumps.compare <- rbind(stump.compare1,
                        stump.compare2,
                        stump.compare3,
                        stump.compare4,
                        stump.compare5,
                        stump.compare6,
                        stump.compare7,
                        stump.compare8,
                        stump.compare9)

stumps.compare <- stumps.compare %>%
  mutate(method = 'myStumps')

overall.compare <- rbind(stumps.compare, compare)

overall.compare %>%
  ggplot(aes(x = trees,
             y = Accuracy,
             fill = method)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  facet_grid(rows = vars(mtry), cols = vars(trees)) +
  coord_cartesian(ylim = c(.7,.82)) +
  xlab('Number of Trees, split by mtry') +
  ylab('Accuracy') +
  ggtitle('Accuracy over mtry and number of trees') +
  theme_light()

overall.compare %>%
  ggplot(aes(x = trees,
             y = Kappa,
             fill = method)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  facet_grid(rows = vars(trees), cols = vars(mtry)) +
  coord_cartesian(ylim = c(.4,.65)) +
  xlab('Number of Trees in Forest') +
  ylab('Kappa') +
  ggtitle('Kappa over mtry and number of trees') +
  theme_light()

