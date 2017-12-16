#Module 3 Problem Set
#Question 1:
#sections i and ii do not require R.
#iii:
require(scatterplot3d)
x1=seq(0,1,by=0.01)
y1=seq(0,1,by=0.01)
#Create a data frame from all combinations of the supplied vectors or factors.
x_y=expand.grid(x1,y1) 
x=x_y[,1]
y=x_y[,2]
z=4/3*(x+y-x*y)
scatterplot3d(x,y,z,color = "red",main = "Student Name", xlab="X",
              ylab="Y", zlab="The Joint p.d.f. of X and Y")
#iv:
z=4/6*(1+x)
scatterplot3d(x,y,z,color = "red",main = "Student Name",
              xlab="X", ylab="Y", zlab="The Marginal p.d.f. of X")
curve(4/6*(1+x),from=0,to=1,main = "Studnet Name", xlab="X",
      ylab="The Marginal p.d.f. of X")

#Question 2:
#You can take the time to type in all of the data given in the question:
VOL1 = c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
RET1 = c(10,8,13,9,11,14,6,4,12,7,5)
VOL2 = c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)
RET2 = c(10,8,13,9,11,14,6,4,12,7,5)
VOL3 = c(10,8,13,9,11,14,6,4,12,7,5)
RET3 = c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
VOL4 = c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.89)
RET4 = c(8,8,8,8,8,8,8,19,8,8,8)
#Alternatively, understand that if you can think it, R can create it:
View(anscombe)
#i Covariances:
cov(anscombe$x1,anscombe$y1)
cov(anscombe$x2,anscombe$y2)
cov(anscombe$x3,anscombe$y3)
cov(anscombe$x4,anscombe$y4)
#ii: Pearson correlations:
cor(anscombe$x1,anscombe$y1,method="pearson")
cor(anscombe$x2,anscombe$y2,method="pearson")
cor(anscombe$x3,anscombe$y3,method="pearson")
cor(anscombe$x4,anscombe$y4,method="pearson")
#iii: Spearman correlations:
cor(anscombe$x1,anscombe$y1,method="spearman")
cor(anscombe$x2,anscombe$y2,method="spearman")
cor(anscombe$x3,anscombe$y3,method="spearman")
cor(anscombe$x4,anscombe$y4,method="spearman")

#v:
par(mfrow=c(2,2),mar= c(4,4,1,1), oma= c(1,2,2,2))
plot(anscombe$x1,anscombe$y1,main="Stock 1",xlab="Volume",
     ylab="Return",pch=15,col= "blue")
plot(anscombe$x2,anscombe$y2,main="Stock 2",xlab="Volume",
     ylab="Return",pch=23,col="green")
plot(anscombe$x3,anscombe$y3,main="Stock 3",xlab="Volume",
     ylab="Return", pch=19,col="red")
plot(anscombe$x4,anscombe$y4,main="Stock 4",xlab="Volume",
     ylab="Return",pch=24, col="yellow",bg="orange")
mtext("Student Name",line=0.2,adj=0.5,outer=T, cex=1.2, 
      col= "orange")
par(mfrow=c(1,1))

#Question 3:
#ii:
set.seed(5)
x=runif(100000,min = 0,max = 1)
y=x^4
#iii:
plot(x,y,main="Student Name",lwd=2)
#iv:
cor(x,y,method="pearson")
#v:
cor(x,y,method="spearman")	

#Question 4 - GFD
#Load the file:
JPM = read.csv(file="C:\\Users\\Jonathan1\\Desktop\\school\\TA\\MA\\Econometrics M.A\\Econometrics 1\\Problem Sets\\PSetSol_Module3\\JPM.csv",
               na.strings=c("NA",".",""))
#Remove days with 0 volume (not required in question):
JPM = subset(JPM, JPM$Volume!=0)
View(JPM)
require(moments)
#i:
cov(JPM$Close_Percent_Change,JPM$Volume)
cor(JPM$Close_Percent_Change,JPM$Volume, method="pearson")
cor(JPM$Close_Percent_Change,JPM$Volume, method ="spearman")
#ii:
mean(JPM$Close_Percent_Change[JPM$Volume < quantile(JPM$Volume,0.2)])
sd(JPM$Close_Percent_Change[JPM$Volume < quantile(JPM$Volume,0.2)])
skewness(JPM$Close_Percent_Change[JPM$Volume < quantile(JPM$Volume,0.2)])
kurtosis(JPM$Close_Percent_Change[JPM$Volume < quantile(JPM$Volume,0.2)])

#iii:
mean(JPM$Close_Percent_Change[JPM$Volume > quantile(JPM$Volume,0.8)])
sd(JPM$Close_Percent_Change[JPM$Volume > quantile(JPM$Volume,0.8)])
skewness(JPM$Close_Percent_Change[JPM$Volume > quantile(JPM$Volume,0.8)])
kurtosis(JPM$Close_Percent_Change[JPM$Volume > quantile(JPM$Volume,0.8)])

#Question 5 - Univariate sorts - intorduction:
require(doBy)
require(plyr)
#i: load the data file:
SignalsData = read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 1\\Problem Sets\\PSetSol_Module3\\SignalsDATA_201508_Annual - Full file.csv",
                       na.strings=c("NA",".",""))
SignalsData$FDATE = as.character(SignalsData$FDATE)
SignalsData$FDATE = as.Date(SignalsData$FDATE,format = "%Y-%m-%d")
class(SignalsData$FDATE)
SignalsData$FDATE[1:5]

SignalsData$AnnPDATE = as.character(SignalsData$AnnPDATE)
SignalsData$AnnPDATE = as.Date(SignalsData$AnnPDATE,format = "%Y-%m-%d")
class(SignalsData$AnnPDATE)
SignalsData$AnnPDATE[1:5]

#ii: Rankings:
#SIZE
SignalsData$dSIZE = ave(SignalsData$SIZE, SignalsData$AnnPDATE,
                        FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
                                             include.lowest=TRUE)})

#Book to Market (BTM)
SignalsData$dBTM = ave(SignalsData$BTM, SignalsData$AnnPDATE,
                       FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
                                            include.lowest=TRUE)})

#Operating Accruals (OACC)
SignalsData$dOACC = ave(SignalsData$OACC, SignalsData$AnnPDATE,
                        FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
                                             include.lowest=TRUE)})

View(SignalsData)

#iii: mean of RET_CompP1P12

mean(SignalsData$RET_CompP1P12)
sd(SignalsData$RET_CompP1P12)
require(moments)
skewness(SignalsData$RET_CompP1P12)
kurtosis(SignalsData$RET_CompP1P12)

#One way:
summaryBy(RET_CompP1P12 ~ dSIZE, data = SignalsData,
          FUN = c(mean,sd,kurtosis,skewness))
summaryBy(RET_CompP1P12 ~ dBTM, data = SignalsData,
          FUN = c(mean,sd,kurtosis,skewness))
summaryBy(RET_CompP1P12 ~ dOACC, data = SignalsData,
          FUN = c(mean,sd,kurtosis,skewness))

#Alternative way:

tapply(SignalsData$RET_CompP1P12,SignalsData$dSIZE,FUN = mean)
tapply(SignalsData$RET_CompP1P12,SignalsData$dSIZE,FUN = sd)
tapply(SignalsData$RET_CompP1P12,SignalsData$dSIZE, FUN = skewness)
tapply(SignalsData$RET_CompP1P12,SignalsData$dSIZE, FUN = kurtosis)

tapply(SignalsData$RET_CompP1P12,SignalsData$dBTM,FUN = mean)
tapply(SignalsData$RET_CompP1P12,SignalsData$dBTM,FUN = sd)
tapply(SignalsData$RET_CompP1P12,SignalsData$dBTM, FUN = skewness)
tapply(SignalsData$RET_CompP1P12,SignalsData$dBTM, FUN = kurtosis)

tapply(SignalsData$RET_CompP1P12,SignalsData$dOACC,FUN = mean)
tapply(SignalsData$RET_CompP1P12,SignalsData$dOACC,FUN = sd)
tapply(SignalsData$RET_CompP1P12,SignalsData$dOACC, FUN = skewness)
tapply(SignalsData$RET_CompP1P12,SignalsData$dOACC, FUN = kurtosis)





