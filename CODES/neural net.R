library(neuralnet)
n <- names(train)
f <- as.formula(paste("Footfall ~", paste(n[!n %in% "Footfall"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(10,10),linear.output=T)
test$Footfall=NULL

pr.nn <- compute(nn,test[,])

# # pr.nn_ <- pr.nn$net.result*(max(test$Footfall)-min(test$Footfall))+min(test$Footfall)
# test.r <- (test$Footfall)*(max(train$Footfall)-min(train$Footfall))+min(train$Footfall)

# MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)


# print(paste(MSE.lm,MSE.nn))

prediction <- data.frame(ID = test_test$ID, Footfall = pr.nn$net.result)

write.csv(prediction,"Student_Hunt_neural.csv",row.names = FALSE)

