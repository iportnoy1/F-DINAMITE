#Setting working directory
setwd("C:/Users/idpdl/Desktop/Equipo Dinamita")
set.seed(1)

#Calling Functions
source("PCA_Functions2.R")

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

X_test <- X_Fault #X_test <- X_Fault[ind == 2,]
X_testing <- X_test

#Loading NOC models and RF Model
load("C:/Users/idpdl/Desktop/Equipo Dinamita/RF.mod.RData")
load("C:/Users/idpdl/Desktop/Equipo Dinamita/k_opt.RData")
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.0.RData")
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.1.RData"); NOC.1 <- temp
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.2.RData"); NOC.2 <- temp
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.3.RData"); NOC.3 <- temp
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.4.RData"); NOC.4 <- temp
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.5.RData"); NOC.5 <- temp
load("C:/Users/idpdl/Desktop/Equipo Dinamita/NOC.6.RData"); NOC.6 <- temp

X_Normal <- NOC.0$X_train

################################################################################################
##Detecting with F-DINAMITE
 ind <- sample(2, size = nrow(X_Normal), replace = TRUE, prob = c(0.8, 0.3))
 X_test <- X_Normal[ind == 1,]
  #X_test <- X_test[,2:18]
#par(mfrow=c(3,2))
Detection <- matrix(data = rep(0, k_opt*nrow(X_test)), nrow(X_test), k_opt)
for (i in 1:k_opt) {
  assign("temp", paste("Current.NOC=NOC.", as.character(i), sep = ""))
  eval(parse(text = temp))
  b = Current.NOC$b; Sig = Current.NOC$Sig; a = Current.NOC$a; P = Current.NOC$P
  T2a = Current.NOC$T2a; Sigma_a = Current.NOC$Sigma_a; VarNames = Current.NOC$VarNames
  Qa = Current.NOC$Qa
  output = PCA_detection(b, Sig, a, P, T2a, Sigma_a, X_test[,VarNames], Qa)
  T2 = output$T2; T2a = output$Threshold; Qa = output$Threshold_Q; Q = output$Q
  #plot(T2, xlab = "Sample", ylab = "T^2", main = paste("NOC",toString(i)), col = "Blue")
  #lines(T2a*rep(1,), col = 'Red')
  plot(Q, xlab = "Sample", ylab = "Q", main = paste("NOC",toString(i)), col = "Blue")
  lines(Qa*rep(1,), col = 'Red')
  Detection[,i] <- T2 >= T2a[1] | Q >= Qa[1]
}
Detection <- rowSums(Detection)
DR.FDINAMITE <- 100*sum(Detection == k_opt)/nrow(X_test); DR.FDINAMITE

#Diagnosing Faults
X_testing <- X_testing[Detection == k_opt,]
Preds <- predict(RF.mod, newdata =X_testing[,2:18])
Diagnosis.error <- 100*(1 - sum(Preds == X_testing$Fault.Type)/length(Preds))
Diagnosis.error

################################################################################################
#Detecting with conventional PCA
#output_training = PCA_training(X_train = X_normal)
b=NOC.0$b; Sig=NOC.0$Sig; a=NOC.0$a; P=NOC.0$P; T2a=NOC.0$T2a 
Sigma_a=NOC.0$Sigma_a; Qa=NOC.0$Qa; VarNames = NOC.0$VarNames
output = PCA_detection(b, Sig, a, P, T2a, Sigma_a, X_test = X_test[VarNames], Qa)
T2 = output$T2; T2a = output$Threshold; Qa = output$Threshold_Q; Q = output$Q

#Computing False Alarm Rate (FAR)
DR.PCA <- 100*sum(Q>=Qa[1] | T2>=T2a[1])/nrow(X_test); DR.PCA

#par(mfrow=c(2,1))
plot(T2, xlab = "Sample", ylab = "T^2", col = "Blue", cex.lab=0.7, 
     cex.axis=0.7, cex.main=0.7, cex.sub=0.7, cex=0.7) 
lines(T2a*rep(1,), col = 'Red')
plot(Q, xlab = "Sample", ylab = "Q", col = "Blue", cex.lab=0.7, 
     cex.axis=0.7, cex.main=0.7, cex.sub=0.7, cex=0.7)
lines(Qa*rep(1,), col = 'Red')
