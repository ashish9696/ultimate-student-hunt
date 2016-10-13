path <- "H:/my work/hackathon"

setwd(path)
# load data and libraries -------------------------------------------------

library(data.table)

library(mlr)

library(lubridate)

train <- fread("train.csv")
test <- fread("test.csv")

# See data ----------------------------------------------------------------

head(train)

head(test)

# Check column names & data types -----------------------------------------

str(train)

str(test)

# Setting & Creating date variables -------------------------------------------------

train[,Date := dmy(Date)]

test[,Date := dmy(Date)]

train[,c("year","month","dayofyear","dayofweek","day","week") := list(year(Date), #Year
                                                                      
                                                                      month(Date), #Month
                                                                      
                                                                      yday(Date), #Day of Year
                                                                      
                                                                      wday(Date), #Day of week
                                                                      
                                                                      week(Date), #week of month
                                                                      
                                                                      day(Date) #day of month
                                                                      
)]

test[,c("year","month","dayofyear","dayofweek","day","week") := list(year(Date), #Year
                                                                     
                                                                     month(Date), #Month
                                                                     
                                                                     yday(Date), #Day of Year
                                                                     
                                                                     wday(Date), #Day of week
                                                                     
                                                                     week(Date), #week of month
                                                                     
                                                                     day(Date) #day of month
                                                                     
)]

# Impute missing values as -1 ------------------------------------------------

train[is.na(train)] <- -999

test[is.na(test)] <- -999

# Creating a validation set based on Date again last 2 years of tr --------

validate <- train[Date > "1999-12-31",]

training <- train[Date <= "1999-12-31",]

# Drop date column --------------------------------------------------------

ID <- test$ID

test$Footfall <- mean(train$Footfall)

train[,c("Date","Park_ID","ID","week","Location_Type") := NULL]

test[,c("Date","Park_ID","ID","week","Location_Type") := NULL]

training[,c("Date","Park_ID","ID","week","Location_Type") := NULL]
validate[,c("Date","Park_ID","ID","week","Location_Type") := NULL]
##


# Gradient Boosting -------------------------------------------------------

#create task

training.task <- makeRegrTask(data=training, target = "Footfall")

validate.task <- makeRegrTask(data=validate,target="Footfall")

train.task <- makeRegrTask(data=train,target = "Footfall")

test.task <- makeRegrTask(data=test,target = "Footfall")

#get variable importance chart based on information gain

var_imp <- generateFilterValuesData(training.task, method = c("information.gain"))

plotFilterValues(var_imp,feat.type.cols = TRUE)

#create learner

set.seed(1123)

listLearners("regr")[c("class","type")]
library(deeplearning)
gbm.learner <- makeLearner("regr.gbm")

gbm.learner$par.vals <- list(
  
  interaction.depth = 10,
  
  shrinkage = 0.09,
  
  n.trees = 60
  
)

#run model

deeplearning.model <- train(gbm2.learner,training.task)

#predict on validate data

deeplearning.predict <- predict(deeplearning.model,validate.task)

performance(deeplearning.predict,measures=rmse)

# rmse 

# 108.5145 

# build model on train data -----------------------------------------------

#create learner

set.seed(1123)

listLearners("regr")[c("class","type")]

gbm2.learner <- makeLearner("regr.gbm")

gbm2.learner$par.vals <- list(
  
  interaction.depth = 10,
  
  shrinkage = 0.279,
  
  n.trees = 60
  
)

#train model

t_gbm <- train(gbm2.learner,train.task)

#predictions

p_gbm <- predict(t_gbm,test.task)
performance(p_gbm,measures=rmse)
#write prediction file

f_pred <- p_gbm$data$response

prediction <- data.frame(ID = ID, Footfall = f_pred)

write.csv(prediction,"Student_Hunt_Sub.csv",row.names = FALSE)

##random
#Random forest for classification
library("randomForest")
library(caret)
fit_classify = randomForest(Footfall ~ ., train, importance = TRUE, ntree = 100)
pred = predict(fit_classify, test[,-20])
xtab = table(observed = test[,20], predicted = pred)
confusionMatrix(xtab)

#extract the variable importance
importance(fit_classify, type = 2)

##Random forest for regression
