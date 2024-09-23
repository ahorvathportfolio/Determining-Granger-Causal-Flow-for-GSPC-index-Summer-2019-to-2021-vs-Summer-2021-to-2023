# Andras Horvath 
# Granger Causality Strength of the 11 sectors in S&P 500 stocks using different
# BigVAR methods.

###################
### 2021 - 2023 ###
###################

###############################
### 11 Sectors S&P 500 Data ###
################################
setwd("Your directory to Final Analysis (in Project Report) folder")

library(MTS)
library(BigVAR)
library(tsDyn)

data = read.csv("Sector Averages 2021-2023.csv", header = T)
data = data[,-1]

dim(data) # [1] 502 11
names = colnames(data)
data_mat = matrix(0,dim(data)[1]-1,1) # -1 because of the differencing coming up
for (i in 1:dim(data)[2])
{
  rtn = matrix(diff(log(as.numeric(data[,i]))), ncol = 1)
  data_mat = cbind(data_mat,rtn)
}
data_mat
data_mat = data_mat[,-1]
colnames(data_mat) = names

myY = as.matrix(data_mat)
my.dim = dim(myY)[2] # dim(myY) = 503 x 11
head(myY)
# Now we are ready to go!
################################################################################

############
### MTS ####           
############             
model=VAR(myY, p = 1, output = F, include.mean = F)                                           
model$coef                                                                                    
refinedmodel = refVAR(model, thres = 1.96)                                                    
refinedmodel$coef # 15 nonzero coefficients
#      [,1]       [,2] [,3]      [,4]       [,5]        [,6] [,7]        [,8]       [,9]      [,10]      [,11]
# [1,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.08201707  0.1197670  0.0000000  0.0000000
# [2,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000
# [3,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000
# [4,]    0  0.0000000    0 0.0000000  0.0000000 -0.07694168    0 -0.07897812  0.0000000  0.0000000  0.0000000
# [5,]    0 -0.1494811    0 0.0000000 -0.1777276  0.00000000    0  0.00000000 -0.1713418 -0.1823757 -0.1290739
# [6,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000
# [7,]    0  0.1663652    0 0.0000000  0.1956458  0.00000000    0  0.00000000  0.0000000  0.2017479  0.1209280
# [8,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.1131444  0.0000000  0.0000000
# [9,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000
#[10,]    0  0.0000000    0 0.1595556  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000
#[11,]    0  0.0000000    0 0.0000000  0.0000000  0.00000000    0  0.00000000  0.0000000  0.0000000  0.0000000

fixed_matrix = matrix(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,
                        FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                        FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), nrow = 11,byrow=TRUE)

### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=VAR(myY, p = 1, output = F, include.mean = F)
      mod1ref=refVAR(mod1, fixed = fixed_matrix)
      mod1.sig = mod1ref$Sigma[i,i]
      mod0=VAR(myY[,-j], p = 1, output = F, include.mean = F)
      mod0ref=refVAR(mod0, fixed = fixed_matrix[,-j])
      mod0.sig = mod0ref$Sigma[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=VAR(myY, p = 1, output = F, include.mean = F)
      mod1ref=refVAR(mod1, fixed = fixed_matrix)
      mod1.sig = mod1ref$Sigma[i,i]
      mod0=VAR(myY[,-j], p = 1, output = F, include.mean = F)
      mod0ref = refVAR(mod0, fixed = fixed_matrix[,-j])
      mod0.sig = mod0ref$Sigma[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # Takes: 1.07 secs

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat MTS.csv")
# Note: The names of the files here will overlap with the 2019-2021 files. This is just because
# of how I chose to rename the directory and put only the important files in a single folder on 
# my GitHub. Just make two separate folders to keep the files straight based on which time frame 
# you are working with.


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength MTS.csv") 
################################################################################

###################
### Basic Lasso ###
###################
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "Basic", gran = c(50,10), h = 1, cv = "LOO", verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "Basic", gran = c(50,10), h = 1, cv = "LOO", verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "Basic", gran = c(50,10), h = 1, cv = "LOO", verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "Basic", gran = c(50,10), h = 1, cv = "LOO", verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # Takes: 4.46 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Basic Lasso.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength Bassic Lasso.csv")
################################################################################

#####################
### BasicEN Lasso ###
#####################
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "BasicEN", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "BasicEN", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "BasicEN", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "BasicEN", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken #  Takes: 4.63 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat BasicEN Lasso.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength BassicEN Lasso.csv") 
################################################################################

###########
### Lag ###
###########
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "Lag", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "Lag", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "Lag", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "Lag", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken #  Takes: 12.64 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Lag.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength Lag.csv")
################################################################################

############
### SCAD ###
############
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "SCAD", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "SCAD", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "SCAD", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "SCAD", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken #  Takes: 25.74 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat SCAD.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength SCAD.csv")
################################################################################

###########
### MCP ###
###########
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "MCP", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "MCP", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "MCP", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "MCP", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken #  Takes: 28.47 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat MCP.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength MCP.csv")
################################################################################

###########
### BGR ###
###########
### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=constructModel(myY, p=1, struct = "BGR", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "BGR", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=constructModel(myY, p=1, struct = "BGR", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod1Results=cv.BigVAR(mod1)
      mod1.sig = cov(mod1Results@resids)[i,i]
      mod0=constructModel(myY[,-j], p=1, struct = "BGR", gran = c(50,10), h = 1, cv = "LOO",verbose = FALSE, IC = TRUE)
      mod0Results=cv.BigVAR(mod0)
      mod0.sig = cov(mod0Results@resids)[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken #  Takes: 9.78 mins

### Print GC.mat
GC.mat
GC.mat_original = as.data.frame(t(GC.mat))
colnames(GC.mat_original) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                              "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                              "Consumer Staples")
write.csv(GC.mat_original, "Your directory to Final Analysis (in Project Report) folder\\GC.mat BGR.csv")


GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Final Analysis (in Project Report) folder\\GC.mat Normalized\\Sectors GC Strength BGR.csv")
################################################################################

#################
### HEAT MAPS ###
#################

setwd("Your directory to Final Analysis (in Project Report) folder")

#######
# MTS #
#######
data = read.csv("Sectors GC Strength MTS.csv", header = T) # Use normalized versions for all of these
data = data[,-1]

# install and load the plotly package
#library(ggcorrplot)

#ggcorrplot(data, type = "lower")

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength MTS 2021-2023", scales=list(x=list(rot=90)))

###############
# BASIC LASSO #
###############
data = read.csv("Sectors GC Strength Bassic Lasso.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength Basic Lasso Penalty 2021-2023", scales=list(x=list(rot=90)))

#################
# BASICEN LASSO #
#################
data = read.csv("Sectors GC Strength BassicEN Lasso.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength Basic Elastic Net Lasso Penalty 2021-2023", scales=list(x=list(rot=90)))

#######
# LAG #
#######
data = read.csv("Sectors GC Strength Lag.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength Lag Penalty 2021-2023", scales=list(x=list(rot=90)))

########
# SCAD #
########
data = read.csv("Sectors GC Strength SCAD.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength SCAD Penalty 2021-2023", scales=list(x=list(rot=90)))

#######
# MCP #
#######
data = read.csv("Sectors GC Strength MCP.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength MCP Penalty 2021-2023", scales=list(x=list(rot=90)))

#######
# BGR #
#######
data = read.csv("Sectors GC Strength BGR.csv", header = T)
data = data[,-1]

library("lattice")
colnames(data) = colnames(data)
rownames(data) = colnames(data)
levelplot(t(as.matrix(data)), main = "GC Strength BGR Penalty 2021-2023", scales=list(x=list(rot=90)))
