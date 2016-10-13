rm(list=ls())
library(data.table)
path <- "H:/my work/hackathon"
setwd(path)
getwd()
#load data using fread
train <- fread("traingbm.csv", stringsAsFactors = T)
test <- fread("testgbm.csv", stringsAsFactors = T)
dim(train)
dim(test)
str(train)

#second submission
summary (train)
summary (test)
#combine data set
test[,Footfall := mean(train$Footfall)]
c <- list(train, test)
combin <- rbindlist(c)
#analyse variables
combin[,prop.table(table(Direction_Of_Wind))] 
combin[,prop.table(table(Average_Breeze_Speed))] 
combin[,prop.table(table(Average_Atmospheric_Pressure))]
combin[,prop.table(table(Max_Breeze_Speed))]
combin[,prop.table(table(Min_Breeze_Speed))]
combin[,prop.table(table(Location_Type))]
length(unique(combin$ID))
length(unique(combin$Park_ID))
length(unique(combin$Direction_Of_Wind))
colSums(is.na(combin))
library(ggplot2)
ggplot(combin, aes(Average_Breeze_Speed, fill = Average_Atmospheric_Pressure)) + geom_bar()
ggplot(combin, aes(Average_Breeze_Speed, fill = Location_Type)) + geom_bar()
library(gmodels)
CrossTable(combin$Location_Type, combin$Park_ID)
CrossTable(combin$Location_Type, combin$Direction_Of_Wind)

##nas
combin[,Max_Atmospheric_Pressure_NA := ifelse(sapply(combin$Max_Atmospheric_Pressure, is.na) ==    TRUE,1,0)]
combin[, Avg_Ambient_Pollution := mean(Max_Ambient_Pollution+Min_Ambient_Pollution), by = ID]
combin$Min_Moisture_In_Park[is.na(combin$Min_Moisture_In_Park)] = median(combin$Min_Moisture_In_Park, na.rm = TRUE)
combin$Max_Moisture_In_Park[is.na(combin$Max_Moisture_In_Park)] = median(combin$Max_Moisture_In_Park, na.rm = TRUE)
combin$Average_Moisture_In_Park[is.na(combin$Average_Moisture_In_Park)] = median(combin$Average_Moisture_In_Park, na.rm = TRUE)

combin$Min_Breeze_Speed[is.na(combin$Min_Breeze_Speed)] = median(combin$Min_Breeze_Speed, na.rm = TRUE)
combin$Max_Breeze_Speed[is.na(combin$Max_Breeze_Speed)] = median(combin$Max_Breeze_Speed, na.rm = TRUE)
combin$Average_Breeze_Speed[is.na(combin$Average_Breeze_Speed)] = median(combin$Average_Breeze_Speed, na.rm = TRUE)

combin$Min_Ambient_Pollution[is.na(combin$Min_Ambient_Pollution)] = median(combin$Min_Ambient_Pollution, na.rm = TRUE)
combin$Max_Ambient_Pollution[is.na(combin$Max_Ambient_Pollution)] = median(combin$Max_Ambient_Pollution, na.rm = TRUE)
combin$Avg_Ambient_Pollution[is.na(combin$Avg_Ambient_Pollution)] = median(combin$Avg_Ambient_Pollution, na.rm = TRUE)

combin$Min_Atmospheric_Pressure[is.na(combin$Min_Atmospheric_Pressure)] = median(combin$Min_Atmospheric_Pressure, na.rm = TRUE)
combin$Max_Atmospheric_Pressure[is.na(combin$Max_Atmospheric_Pressure)] = median(combin$Max_Atmospheric_Pressure, na.rm = TRUE)
combin$Average_Atmospheric_Pressure[is.na(combin$Average_Atmospheric_Pressure)] = median(combin$Average_Atmospheric_Pressure, na.rm = TRUE)

combin$Direction_Of_Wind[is.na(combin$Direction_Of_Wind)] = median(combin$Direction_Of_Wind, na.rm = TRUE)
combin$Var1[is.na(combin$Var1)] = median(combin$Var1, na.rm = TRUE)
sapply(combin, class)
c.train <- combin[1:nrow(train),]
c.test <- combin[-(1:nrow(train)),]
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)
colnames(train.h2o)
y.dep <- 16
x.indep <- c(2:15,17:22)
#deep learning
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)
h2o.performance(dlearning.model)
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
sub_dlearning <- data.frame(ID = df_test$ID, Footfall =  predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new_22var.csv", row.names = F)

