# Andras Horvath 
# Granger Causality Strength of the 11 sectors in S&P 500 stocks.
# Summer 2019-Summer 2021 vs Summer 2021-Summer 2023
# 1. Compare the time series themselves in each time frame.
# 2. Compare sparsity structures using different methods.
# 3. Compare the GC strength.

###################
### 2019 - 2021 ###
###################

###############################
### 11 Sectors S&P 500 Data ###
################################
setwd("Your directory to Summer 2019-2021 Data folder")

library(MTS)
library(BigVAR)
library(tsDyn)
fulldata = read.csv("All S&P 500 Stocks 2019-2021.csv", header = T)
fulldata = fulldata[,-1] # removing date column

sectors = read.csv("All S&P 500 Stocks Categories 2019-2021.csv", header = T)

test1 = colnames(fulldata)
test2 = colnames(sectors)

counter = 0
for(i in 1:length(test1))
{
  if(test1[i] == test2[i]) 
  {
    counter = counter + 1
  }
}
counter # Should be 496

sectors[1,]
data_new = rbind(fulldata, sectors[1,])
data_new = rbind(data_new,colnames(fulldata))

# Communication Services
CommunicationServices = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Communication Services")
  {
    CommunicationServices = cbind(CommunicationServices, data_new[,i])
  }
}
CommunicationServices = as.data.frame(CommunicationServices)
CommunicationServices = CommunicationServices[,-1]
CSnames = CommunicationServices[506,]
colnames(CommunicationServices) = CSnames
CommunicationServices = CommunicationServices[-c(505,506),]
write.csv(CommunicationServices, "Your directory to Summer 2019-2021 Data folder\\CommunicationServices.csv") 

# Industrials
Industrials = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Industrials")
  {
    Industrials = cbind(Industrials, data_new[,i])
  }
}
Industrials = as.data.frame(Industrials)
Industrials = Industrials[,-1]
Inames = Industrials[506,]
colnames(Industrials) = Inames
Industrials = Industrials[-c(505,506),]
write.csv(Industrials, "Your directory to Summer 2019-2021 Data folder\\Industrials.csv") 

# Utilities
Utilities = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Utilities")
  {
    Utilities = cbind(Utilities, data_new[,i])
  }
}
Utilities = as.data.frame(Utilities)
Utilities = Utilities[,-1]
Unames = Utilities[506,]
colnames(Utilities) = Unames
Utilities = Utilities[-c(505,506),]
write.csv(Utilities, "Your directory to Summer 2019-2021 Data folder\\Utilities.csv") 

# Energy
Energy = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Energy")
  {
    Energy = cbind(Energy, data_new[,i])
  }
}
Energy = as.data.frame(Energy)
Energy = Energy[,-1]
Enames = Energy[506,]
colnames(Energy) = Enames
Energy = Energy[-c(505,506),]
write.csv(Energy, "Your directory to Summer 2019-2021 Data folder\\Energy.csv") 

# Financials
Financials = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Financials")
  {
    Financials = cbind(Financials, data_new[,i])
  }
}
Financials = as.data.frame(Financials)
Financials = Financials[,-1]
Fnames = Financials[506,]
colnames(Financials) = Fnames
Financials = Financials[-c(505,506),]
write.csv(Financials, "Your directory to Summer 2019-2021 Data folder\\Financials.csv") 

# InformationTech
InformationTech = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Information Technology")
  {
    InformationTech = cbind(InformationTech, data_new[,i])
  }
}
InformationTech = as.data.frame(InformationTech)
InformationTech = InformationTech[,-1]
ITnames = InformationTech[506,]
colnames(InformationTech) = ITnames
InformationTech = InformationTech[-c(505,506),]
write.csv(InformationTech, "Your directory to Summer 2019-2021 Data folder\\InformationTech.csv") 

# ConsumerDiscretionary
ConsumerDiscretionary = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Consumer Discretionary")
  {
    ConsumerDiscretionary = cbind(ConsumerDiscretionary, data_new[,i])
  }
}
ConsumerDiscretionary = as.data.frame(ConsumerDiscretionary)
ConsumerDiscretionary = ConsumerDiscretionary[,-1]
CDnames = ConsumerDiscretionary[506,]
colnames(ConsumerDiscretionary) = CDnames
ConsumerDiscretionary = ConsumerDiscretionary[-c(505,506),]
write.csv(ConsumerDiscretionary, "Your directory to Summer 2019-2021 Data folder\\ConsumerDiscretionary.csv") 

# RealEstate
RealEstate = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Real Estate")
  {
    RealEstate = cbind(RealEstate, data_new[,i])
  }
}
RealEstate = as.data.frame(RealEstate)
RealEstate = RealEstate[,-1]
Rnames = RealEstate[506,]
colnames(RealEstate) = Rnames
RealEstate = RealEstate[-c(505,506),]
write.csv(RealEstate, "Your directory to Summer 2019-2021 Data folder\\RealEstate.csv") 

# Healthcare
Healthcare = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Health Care")
  {
    Healthcare = cbind(Healthcare, data_new[,i])
  }
}
Healthcare = as.data.frame(Healthcare)
Healthcare = Healthcare[,-1]
Hnames = Healthcare[506,]
colnames(Healthcare) = Hnames
Healthcare = Healthcare[-c(505,506),]
write.csv(Healthcare, "Your directory to Summer 2019-2021 Data folder\\Healthcare.csv") 


# Materials
Materials = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Materials")
  {
    Materials = cbind(Materials, data_new[,i])
  }
}
Materials = as.data.frame(Materials)
Materials = Materials[,-1]
Mnames = Materials[506,]
colnames(Materials) = Mnames
Materials = Materials[-c(505,506),]
write.csv(Materials, "Your directory to Summer 2019-2021 Data folder\\Materials.csv") 

# ConsumerStaples
ConsumerStaples = matrix(0,dim(data_new)[1],1)
for(i in 1:dim(data_new)[2])
{
  if(data_new[505,i] == "Consumer Staples")
  {
    ConsumerStaples = cbind(ConsumerStaples, data_new[,i])
  }
}
ConsumerStaples = as.data.frame(ConsumerStaples)
ConsumerStaples = ConsumerStaples[,-1]
ConStnames = ConsumerStaples[506,]
colnames(ConsumerStaples) = ConStnames
ConsumerStaples = ConsumerStaples[-c(505,506),]
write.csv(ConsumerStaples, "Your directory to Summer 2019-2021 Data folder\\ConsumerStaples.csv") 


##################################
### Create Each Sector Average ###
##################################
setwd("C:\\Users\\ahorvath\\OneDrive - Texas Tech University\\Documents\\Spring 2024\\Graduate Classes\\Time Series Individual Study\\GC Strength Project (S&P 500) For Paper\\Summer 2019-2021")

library(MTS)
library(BigVAR)
library(tsDyn)

CommunicationServices = read.csv("CommunicationServices.csv", header = T)
CommunicationServices = CommunicationServices[,-1]

Industrials = read.csv("Industrials.csv", header = T)
Industrials = Industrials[,-1] 

Utilities = read.csv("Utilities.csv", header = T)
Utilities = Utilities[,-1] 

Energy = read.csv("Energy.csv", header = T)
Energy = Energy[,-1] 

Financials = read.csv("Financials.csv", header = T)
Financials = Financials[,-1] 

InformationTech = read.csv("InformationTech.csv", header = T)
InformationTech = InformationTech[,-1] 

ConsumerDiscretionary = read.csv("ConsumerDiscretionary.csv", header = T)
ConsumerDiscretionary = ConsumerDiscretionary[,-1]

RealEstate = read.csv("RealEstate.csv", header = T)
RealEstate = RealEstate[,-1]

Healthcare = read.csv("Healthcare.csv", header = T)
Healthcare = Healthcare[,-1]

Materials = read.csv("Materials.csv", header = T)
Materials = Materials[,-1]

ConsumerStaples = read.csv("ConsumerStaples.csv", header = T)
ConsumerStaples = ConsumerStaples[,-1]

################################################################################
CommunicationServicesAvg = as.data.frame(rowMeans(CommunicationServices))
#CommunicationServicesAvg
IndustrialsAvg = as.data.frame(rowMeans(Industrials))
UtilitiesAvg = as.data.frame(rowMeans(Utilities))
EnergyAvg = as.data.frame(rowMeans(Energy))
FinancialsAvg = as.data.frame(rowMeans(Financials))
InformationTechAvg = as.data.frame(rowMeans(InformationTech))
ConsumerDiscretionaryAvg = as.data.frame(rowMeans(ConsumerDiscretionary))
RealEstateAvg = as.data.frame(rowMeans(RealEstate))
HealthcareAvg = as.data.frame(rowMeans(Healthcare))
MaterialsAvg = as.data.frame(rowMeans(Materials))
ConsumerStaplesAvg = as.data.frame(rowMeans(ConsumerStaples))


data = cbind(CommunicationServicesAvg, IndustrialsAvg, UtilitiesAvg, EnergyAvg, FinancialsAvg, InformationTechAvg, ConsumerDiscretionaryAvg,
             RealEstateAvg, HealthcareAvg, MaterialsAvg, ConsumerStaplesAvg)
colnames(data) = c("CommunicationServicesAvg", "IndustrialsAvg", "UtilitiesAvg", 
                   "EnergyAvg", "FinancialsAvg", "InformationTechAvg", "ConsumerDiscretionaryAvg",
                   "RealEstateAvg", "HealthcareAvg", "MaterialsAvg", "ConsumerStaplesAvg")

write.csv(data, "Your directory to Summer 2019-2021 Data folder\\Sector Averages.csv") 

#####################################
### Just Jump In Here From Now On ###
#####################################
setwd("Your directory to Summer 2019-2021 Data folder")

library(MTS)
library(BigVAR)
library(tsDyn)

data = read.csv("Sector Averages.csv", header = T)
data = data[,-1]

# Let's get the log returns
dim(data) # [1] 504 11
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

# The next lines of code (terminating at GC.mat) were provided by my instructor
# for this research course, Dr. Trindade, where I have made modifications to fit
# for my data and I have also added the extra lines of code to check the time
# required for the calculations in R.
myY = as.matrix(data_mat)
my.dim = dim(myY)[2] # dim(myY) = 503 x 11
head(myY)

### Loop to compute Granger Causality matrix (GC.mat) from j to i (j -> i)
start.time <- Sys.time()
GC.mat = matrix(0,my.dim,my.dim)
for (i in 1:my.dim){
  if (i > 1){
    for (j in 1:(i-1)){
      mod1=VAR(myY, p = 1, output = F, include.mean = F)
      mod1.sig = mod1$Sigma[i,i]
      mod0=VAR(myY[,-j], p = 1, output = F, include.mean = F)
      mod0.sig = mod0$Sigma[i-1,i-1]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
  if (i < my.dim){
    for (j in (i+1):my.dim){
      mod1=VAR(myY, p = 1, output = F, include.mean = F)
      mod1.sig = mod1$Sigma[i,i]
      mod0=VAR(myY[,-j], p = 1, output = F, include.mean = F)
      mod0.sig = mod0$Sigma[i,i]
      GC.mat[i,j] = log(mod0.sig/mod1.sig)
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # 0.43 seconds

### Print GC.mat
GC.mat
# My adviser/instructor for this course, Dr. Trindade, proposed that we use the
# following normalization though this was a major area we thought we could spend
# a lot more time on and come up with other potentially better normalizations.
GCStrength = t(GC.mat-min(GC.mat))/(max(GC.mat)-min(GC.mat))
GCStrength
GCStrengthdata = as.data.frame(GCStrength)
colnames(GCStrengthdata) = c("Communication Services", "Industrial", "Utilities", "Energy", "Financials",
                             "Information Tech", "Consumer Discretionary", "Real Estate", "Health Care", "Materials",
                             "Consumer Staples")
write.csv(GCStrengthdata, "Your directory to Summer 2019-2021 Data folder\\Sectors GC Strength.csv") 


#########################
### Time Series Plots ###
#########################
setwd("Your directory to Summer 2019-2021 Data folder")

library(MTS)
library(BigVAR)
library(tsDyn)
library(ggplot2)

data = read.csv("Sector Averages.csv", header = T)
data = data[,-1]

plot(data$CommunicationServicesAvg, type = "l", ylim = c(10,340), main = "Summer 2019-2021 S&P 500 Sector Averages", ylab = "Adjusted Closing Price")
lines(data$IndustrialsAvg, col = "red")
lines(data$UtilitiesAvg, col = "blue")
lines(data$EnergyAvg, col = "paleturquoise4")
lines(data$FinancialsAvg, col = "#D700FF")
lines(data$InformationTechAvg, col = "olivedrab")
lines(data$ConsumerDiscretionaryAvg, col = "orange")
lines(data$RealEstateAvg, col = "violetred2")
lines(data$HealthcareAvg, col = "slategray1")
lines(data$MaterialsAvg, col = "deepskyblue4")
lines(data$ConsumerStaplesAvg, col = "gold")
legend("topleft", cex = 0.68, legend=c("Communication Services", "Industrials", "Utilities", "Energy", "Financials",
                      "Information Technology", "Consumer Discretionary", "Real Estate", "Healthcare",
                      "Materials", "Consumer Staples"),  fill = c("black","red", "blue", "paleturquoise4",
                                                                   "#D700FF", "olivedrab", "orange",
                                                                   "violetred2", "slategray1",
                                                                  "deepskyblue4", "gold"))


ts.plot(data)

###############
## Sparcity ###
###############
setwd("Your directory to Summer 2019-2021 Data folder")

library(MTS)
library(BigVAR)

data = read.csv("Sector Averages.csv", header = T)
data = data[,-1]

dim(data)
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

# MTS
model=VAR(myY, p = 1, output = F, include.mean = F)                                           
model$coef                                                                                    
refinedmodel = refVAR(model, thres = 1.96)                                                    
refinedmodel$coef # 37 nonzero coefficients


# BigVAR 
model_a <- constructModel(myY, p = 1, "Basic", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resulta = cv.BigVAR(model_a)
SparsityPlot.BigVAR.results(resulta)

model_a2 <- constructModel(myY, p = 1, "Basic", gran = c(150,10), h = 1, cv = "LOO"
                          ,verbose = TRUE, IC = TRUE)
resulta2 = cv.BigVAR(model_a2)
SparsityPlot.BigVAR.results(resulta2) # Result is almost exactly the same. Just the strength
# of one of the coefficients is higher/lower.

#######
model_b <- constructModel(myY, p = 1, "BasicEN", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resultb = cv.BigVAR(model_b)
SparsityPlot.BigVAR.results(resultb)

model_b2 <- constructModel(myY, p = 1, "BasicEN", gran = c(150,10), h = 1, cv = "LOO"
                           ,verbose = TRUE, IC = TRUE)
resultb2 = cv.BigVAR(model_b2)
SparsityPlot.BigVAR.results(resultb2)

#######
model_c <- constructModel(myY, p = 1, "Lag", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resultc = cv.BigVAR(model_c)
SparsityPlot.BigVAR.results(resultc)

model_c2 <- constructModel(myY, p = 1, "Lag", gran = c(150,10), h = 1, cv = "LOO"
                           ,verbose = TRUE, IC = TRUE)
resultc2 = cv.BigVAR(model_c2)
SparsityPlot.BigVAR.results(resultc2)

#######
model_d <- constructModel(myY, p = 1, "SCAD", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resultd = cv.BigVAR(model_d)
SparsityPlot.BigVAR.results(resultd)

model_d2 <- constructModel(myY, p = 1, "SCAD", gran = c(150,10), h = 1, cv = "LOO"
                           ,verbose = TRUE, IC = TRUE)
resultd2 = cv.BigVAR(model_d2)
SparsityPlot.BigVAR.results(resultd2)

#######
model_e <- constructModel(myY, p = 1, "MCP", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resulte = cv.BigVAR(model_e)
SparsityPlot.BigVAR.results(resulte)

model_e2 <- constructModel(myY, p = 1, "MCP", gran = c(150,10), h = 1, cv = "LOO"
                           ,verbose = TRUE, IC = TRUE)
resulte2 = cv.BigVAR(model_e2)
SparsityPlot.BigVAR.results(resulte2)

#######
model_f <- constructModel(myY, p = 1, "BGR", gran = c(150,10), h = 1, cv = "Rolling"
                          ,verbose = TRUE, IC = TRUE)
resultf = cv.BigVAR(model_f)
SparsityPlot.BigVAR.results(resultf)

model_f2 <- constructModel(myY, p = 1, "BGR", gran = c(150,10), h = 1, cv = "LOO"
                           ,verbose = TRUE, IC = TRUE)
resultf2 = cv.BigVAR(model_f2)
SparsityPlot.BigVAR.results(resultf2)


### Conclusion: All very different. So data is unstable.