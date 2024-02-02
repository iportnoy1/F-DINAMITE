#Setting working directory
setwd("C:/Users/idpdl/Desktop/Equipo Dinamita")

#Calling Functions
source("PCA_Functions2.R")

#Loading dataset
X <- read.csv("MZVAV-2-2.csv", header = T)
# temp <- grepl('8/19/2008', X$Datetime) | grepl('8/25/2008', X$Datetime) | 
#   grepl('9/4/2008', X$Datetime) | grepl('5/3/2009', X$Datetime) | grepl('5/4/2009', X$Datetime) |
#   grepl('5/5/2009', X$Datetime) |  grepl('5/6/2009', X$Datetime) | grepl('8/28/2007', X$Datetime) |
#   grepl('8/29/2007', X$Datetime) | grepl('8/30/2007', X$Datetime) 
temp <- grepl('8/27/2008', X$Datetime) | grepl('8/28/2008', X$Datetime) | 
  grepl('8/29/2008', X$Datetime) | grepl('8/30/2008', X$Datetime) | grepl('8/31/2008', X$Datetime) |
  grepl('9/1/2008', X$Datetime) |  grepl('9/4/2008', X$Datetime) | grepl('9/5/2008', X$Datetime)
X_normal <- X[temp, ] #Capturing the ending of the spring and the hot days of summer

X_normal <- X_normal[X_normal$Fault.Detection.Ground.Truth == 0, ] 
X_normal <- X_normal[, 2:(ncol(X)-1)]

####################################################################################################
## F-DINAMITE Training

All_var_Names = colnames(X_normal)

#Setting max No. of Clusters
Max.No.of.Clusters = 12

#Here starts our algorithm
#Pre-locating False Alarm Rate vector and retrieving No. of rows of X_normal
FARs = rep(0,Max.No.of.Clusters); N = nrow(X_normal)

set.seed(4)

#Iterative clustering and FAR calculation starts here
for (k in 1:Max.No.of.Clusters) {
  if(k==1){
    #Performing PCA
    output_training = PCA_training(X_train = X_normal)
    b=output_training$b; Sig=output_training$Sig; a=output_training$a
    P=output_training$P; T2a=output_training$T2a; Sigma_a=output_training$Sigma_a
    Qa=output_training$Qa; VarNames = output_training$VarNames
    output = PCA_detection(b, Sig, a, P, T2a, Sigma_a, X_test = X_normal[,VarNames], Qa)
    T2 = output$T2; T2a = output$Threshold; Qa = output$Threshold_Q; Q = output$Q
    Excluded.vars = setdiff(All_var_Names, VarNames)
    #Computing False Alarm Rate (FAR)
    FARs[k]=0.5*((100*sum(Q>Qa[1])/nrow(X_normal))+100*sum(T2>T2a[1])/nrow(X_normal));
    #FARs[k] = FARs[k]+100*sum(Q>Qa[1] | T2>T2a[1])/nrow(X_normal)
    #plot(T2, xlab = "Sample", ylab = "T^2", main = paste("No. of Clusters =", 
    #toString(k)), col = "Blue")
    #lines(T2a, col = 'Red')
    NOC.0 = list(b, Sig, a, P, T2a, Sigma_a, Qa, X_normal, VarNames, Excluded.vars)
    names(NOC.0) = c("b", "Sig", "a", "P", "T2a", "Sigma_a", "Qa", "X_train", 
                     "VarNames", "Excluded.vars")
    save(NOC.0, file = "NOC.0.Rdata")
  }else{
    #Performing Clustering
    idx = kmeans(X_normal, centers = k, algorithm = "Forgy", iter.max = 200)$cluster
    FARs[k] = 0
    for (i in 1:k) {
      #Performing PCA
      X_train=X_normal[idx==i,]; n=nrow(X_train); X_test=X_train; 
      output_training = PCA_training(X_train)
      b=output_training$b; Sig=output_training$Sig; a=output_training$a
      P=output_training$P; T2a=output_training$T2a; Sigma_a=output_training$Sigma_a
      Qa=output_training$Qa; VarNames = output_training$VarNames
      output = PCA_detection(b, Sig, a, P, T2a, Sigma_a, X_test[,VarNames], Qa)
      T2 = output$T2; T2a = output$Threshold; Qa = output$Threshold_Q; Q = output$Q
      #Computing False Alarm Rate (FAR)
      FARs[k] = FARs[k]+n*(0.5*((100*sum(Q>Qa[1])/nrow(X_test))+100*sum(T2>T2a[1])/nrow(X_test)))/N
      #FARs[k] = FARs[k]+n*(100*sum(Q>Qa[1] | T2>T2a[1])/nrow(X_test))/N
    }
  }
}

#Determining the optimum No. of Clusters, k
k_opt = which.min(FARs); k_opt
save(k_opt, file = "k_opt.Rdata")
plot(FARs, ylab = "Weighted FAR (%)", xlab = "No. of Clusters", col = "blue", 
     lty = 2)

#Creating and saving NOCs' databases (*NOC stands for Normal Operating Condition)
idx = kmeans(X_normal, centers = k_opt, algorithm = "Forgy", iter.max = 200)$cluster
#par(mfrow=c(3,2))
for (i in 1:k_opt) {
  X_train=X_normal[idx==i,]
  output_training = PCA_training(X_train)
  b=output_training$b; Sig=output_training$Sig; a=output_training$a
  P=output_training$P; T2a=output_training$T2a; Sigma_a=output_training$Sigma_a
  Qa=output_training$Qa; VarNames = output_training$VarNames
  Excluded.vars = setdiff(All_var_Names, VarNames)
  #mean.Excluded.vars = colMeans(X_train[,Excluded.vars])
  #Arrengin output
  temp = list(b, Sig, a, P, T2a, Sigma_a, Qa, X_train, VarNames, Excluded.vars)
  names(temp) = c("b", "Sig", "a", "P", "T2a", "Sigma_a", "Qa", "X_train", 
                  "VarNames", "Excluded.vars")
  assign(paste("NOC.", as.character(i), sep=""), temp)
  #eval(parse(text = temp))
  #Saving output
  assign("temp2", paste("NOC.", as.character(i), ".Rdata", sep=""))
  save(temp, file = temp2)
  
  #Plotting multiple run charts
  output = PCA_detection(b, Sig, a, P, T2a, Sigma_a, X_test = X_train[,VarNames], Qa)
  T2 = output$T2; T2a = output$Threshold; Qa = output$Threshold_Q; Q = output$Q
  # plot(T2, xlab = "Sample", ylab = "T^2", main = paste("Cluster =",toString(i)), col = "Blue")
  # lines(T2a*rep(1,), col = 'Red')
  # plot(Q, xlab = "Sample", ylab = "Q", main = paste("Cluster =",toString(i)), col = "Blue")
  # lines(Qa*rep(1,), col = 'Red')
}

##Training Random Forest Model for Diagnosis
set.seed(1)
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
ind <- sample(2, size = nrow(X_Fault), replace = TRUE, prob = c(0.7, 0.3))
X_train <- X_Fault[ind == 1,]; 
RF.mod <- randomForest(x = X_train[,2:18], y = as.factor(X_train$Fault.Type), ntree = 500)
save(RF.mod, file = "RF.mod.Rdata")
