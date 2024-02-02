#Setting working directory
setwd("C:/Users/idpdl/Desktop/Equipo Dinamita")

#Calling Functions
source("PCA_Functions2.R")
library("randomForest")

#Loading dataset
X <- read.csv("MZVAV-2-2.csv", header = T)

temp.1 <- grepl('8/28/2007', X$Datetime) | grepl('8/29/2007', X$Datetime) | 
  grepl('8/30/2007', X$Datetime) ## Heating Coil Valve Leaking
temp.2 <- grepl('5/6/2008', X$Datetime) ## Cooling Coil Valve Stuck Fully Closed
temp.3 <- grepl('8/31/2007',X$Datetime) | grepl('5/15/2008',X$Datetime) ## Cooling Coil Valve Stuck Fully Open
temp.4 <- grepl('5/7/2008', X$Datetime) ## OA Damper Fully Closed

X1 <- X[temp.1,]; X2 <- X[temp.2,]; X3 <- X[temp.3,]; X4 <- X[temp.4,]

X1$Fault.Type <- "Heating Coil Valve Leaking"
X2$Fault.Type <- "Cooling Coil Valve Stuck Fully Closed"
X3$Fault.Type <- "Cooling Coil Valve Stuck Fully Open"
X4$Fault.Type <- "OA Damper Fully Closed"

X_Fault <- rbind(X1, X2, X3, X4)

set.seed(1)
ind <- sample(2, size = nrow(X_Fault), replace = TRUE, prob = c(0.7, 0.3))
X_train <- X_Fault[ind == 1,]; X_test <- X_Fault[ind == 2,]

RF.mod <- randomForest(x = X_train[,2:18], y = as.factor(X_train$Fault.Type), ntree = 500)

Preds <- predict(RF.mod, newdata = X_test[,2:18])
error <- 100*(1 - sum(Preds == X_test$Fault.Type)/length(Preds))
error
