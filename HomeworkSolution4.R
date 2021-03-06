#Suggested Solution for Module 4 Problem Set

#Question 1:
#Section iv:
set.seed(4)
X_10=rnorm(n=10,mean=5,sd=sqrt(3))
set.seed(4)
X_100=rnorm(n=100,mean=5,sd=sqrt(3))
set.seed(4)
X_1000=rnorm(n=1000,mean=5,sd=sqrt(3))
set.seed(4)
X_100000=rnorm(n=100000,mean=5,sd=sqrt(3))
set.seed(4)
X_1000000=rnorm(n=1000000,mean=5,sd=sqrt(3))
require(moments)
#mean calculation
moment(X_10,order= 1,central=FALSE)
moment(X_100,order= 1,central=FALSE)
moment(X_1000,order= 1,central=FALSE)
moment(X_100000,order= 1,central=FALSE)
moment(X_1000000,order= 1,central=FALSE)
#var calculation
moment(X_10,order= 2,central=TRUE)
moment(X_100,order= 2,central=TRUE)
moment(X_1000,order= 2,central=TRUE)
moment(X_100000,order= 2,central=TRUE)
moment(X_1000000,order= 2,central=TRUE)

#Question 3:
#i Load the data:
SignalsData =read.csv(file="PSetSol_Module4\\SignalsData2013.csv",
                      na.strings=c("NA",".",""))
SignalsData$FDATE = as.character(SignalsData$FDATE)
SignalsData$FDATE = as.Date(SignalsData$FDATE,format = "%Y-%m-%d")
class(SignalsData$FDATE)
SignalsData$FDATE[1:5]

SignalsData$AnnPDATE = as.character(SignalsData$AnnPDATE)
SignalsData$AnnPDATE = as.Date(SignalsData$AnnPDATE,format = "%Y-%m-%d")
class(SignalsData$AnnPDATE)
SignalsData$AnnPDATE[1:5]

View(SignalsData)
#ii: Quantiles:
SignalsData$dSIZE = ave(SignalsData$SIZE, SignalsData$AnnPDATE,
                        FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.2)),
                                             include.lowest=TRUE)})

#Book to Market (BTM)
SignalsData$dBTM = ave(SignalsData$BTM, SignalsData$AnnPDATE,
                       FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.2)),
                                            include.lowest=TRUE)})

#iii: Estimation with unknown variance assuming normal distribution
A = 0.07/2
1-A
#BTM:
#Top portfolio:
MeanEstimate_BTM_TOP = mean(SignalsData$RET_CompP1P12
                            [SignalsData$dBTM == 5])
MeanEstimate_BTM_TOP
SDEstimate_BTM_TOP = sd(SignalsData$RET_CompP1P12
                        [SignalsData$dBTM == 5])
SDEstimate_BTM_TOP
n_BTM_TOP = length(SignalsData$dBTM[SignalsData$dBTM == 5])
n_BTM_TOP 
t_BTM_TOP_0.965 = qt(p=1-A, df = n_BTM_TOP - 1)
t_BTM_TOP_0.965	
t_BTM_TOP_0.035 = qt(p=A, df = n_BTM_TOP - 1)
t_BTM_TOP_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_BTM_TOP = c(MeanEstimate_BTM_TOP - t_BTM_TOP_0.965*
                               SDEstimate_BTM_TOP/sqrt(n_BTM_TOP),
                             MeanEstimate_BTM_TOP - t_BTM_TOP_0.035*SDEstimate_BTM_TOP/sqrt(n_BTM_TOP))
TwoT_IntEstimate_BTM_TOP

#Bottom portfolio
MeanEstimate_BTM_BOTTOM = mean(SignalsData$RET_CompP1P12
                               [SignalsData$dBTM == 1])
MeanEstimate_BTM_BOTTOM
SDEstimate_BTM_BOTTOM = sd(SignalsData$RET_CompP1P12
                           [SignalsData$dBTM == 1])
SDEstimate_BTM_BOTTOM
n_BTM_BOTTOM = length(SignalsData$dBTM[SignalsData$dBTM == 1])
n_BTM_BOTTOM 
t_BTM_BOTTOM_0.965 = qt(p=1-A, df = n_BTM_TOP - 1)
t_BTM_BOTTOM_0.965	
t_BTM_BOTTOM_0.035 = qt(p=A, df = n_BTM_TOP - 1)
t_BTM_BOTTOM_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_BTM_BOTTOM = c(MeanEstimate_BTM_BOTTOM - t_BTM_BOTTOM_0.965*
                                  SDEstimate_BTM_BOTTOM/sqrt(n_BTM_BOTTOM),
                                MeanEstimate_BTM_BOTTOM - t_BTM_BOTTOM_0.035*SDEstimate_BTM_BOTTOM/sqrt(n_BTM_BOTTOM))
TwoT_IntEstimate_BTM_BOTTOM

#Size:
#Top portfolio:
MeanEstimate_SIZE_TOP = mean(SignalsData$RET_CompP1P12
                             [SignalsData$dSIZE == 5])
MeanEstimate_SIZE_TOP
SDEstimate_SIZE_TOP = sd(SignalsData$RET_CompP1P12
                         [SignalsData$dSIZE == 5])
SDEstimate_SIZE_TOP
n_SIZE_TOP = length(SignalsData$dSIZE[SignalsData$dSIZE == 5])
n_SIZE_TOP 
t_SIZE_TOP_0.965 = qt(p=1-A, df = n_SIZE_TOP - 1)
t_SIZE_TOP_0.965	
t_SIZE_TOP_0.035 = qt(p=A, df = n_SIZE_TOP - 1)
t_SIZE_TOP_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_SIZE_TOP = c(MeanEstimate_SIZE_TOP - t_SIZE_TOP_0.965*
                                SDEstimate_SIZE_TOP/sqrt(n_SIZE_TOP),
                              MeanEstimate_SIZE_TOP - t_SIZE_TOP_0.035*SDEstimate_SIZE_TOP/sqrt(n_SIZE_TOP))
TwoT_IntEstimate_SIZE_TOP

#Bottom portfolio
MeanEstimate_SIZE_BOTTOM = mean(SignalsData$RET_CompP1P12
                                [SignalsData$dSIZE == 1])
MeanEstimate_SIZE_BOTTOM
SDEstimate_SIZE_BOTTOM = sd(SignalsData$RET_CompP1P12
                            [SignalsData$dSIZE == 1])
SDEstimate_SIZE_BOTTOM
n_SIZE_BOTTOM = length(SignalsData$dSIZE[SignalsData$dSIZE == 1])
n_SIZE_BOTTOM 
t_SIZE_BOTTOM_0.965 = qt(p=1-A, df = n_SIZE_TOP - 1)
t_SIZE_BOTTOM_0.965	
t_SIZE_BOTTOM_0.035 = qt(p=A, df = n_SIZE_TOP - 1)
t_SIZE_BOTTOM_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_SIZE_BOTTOM = c(MeanEstimate_SIZE_BOTTOM - t_SIZE_BOTTOM_0.965*
                                   SDEstimate_SIZE_BOTTOM/sqrt(n_SIZE_BOTTOM),
                                 MeanEstimate_SIZE_BOTTOM - t_SIZE_BOTTOM_0.035*SDEstimate_SIZE_BOTTOM/sqrt(n_SIZE_BOTTOM))
TwoT_IntEstimate_SIZE_BOTTOM

#iv: Interval estimation with unknown variance
#In this section I use the Mean and SD estimates from section iii.
#BTM:
#Top portfolio: 
z_BTM_TOP_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_BTM_TOP_0.965	
z_BTM_TOP_0.035 = qnorm(p=A, mean=0, sd=1)
z_BTM_TOP_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_BTM_TOP = c(MeanEstimate_BTM_TOP - z_BTM_TOP_0.965*
                               SDEstimate_BTM_TOP/sqrt(n_BTM_TOP),
                             MeanEstimate_BTM_TOP - z_BTM_TOP_0.035*SDEstimate_BTM_TOP/sqrt(n_BTM_TOP))
TwoT_IntEstimate_BTM_TOP

#Bottom portfolio 
z_BTM_BOTTOM_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_BTM_BOTTOM_0.965	
z_BTM_BOTTOM_0.035 = qnorm(p=A, mean=0, sd=1)
z_BTM_BOTTOM_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_BTM_BOTTOM = c(MeanEstimate_BTM_BOTTOM - z_BTM_BOTTOM_0.965*
                                  SDEstimate_BTM_BOTTOM/sqrt(n_BTM_BOTTOM),
                                MeanEstimate_BTM_BOTTOM - z_BTM_BOTTOM_0.035*SDEstimate_BTM_BOTTOM/sqrt(n_BTM_BOTTOM))
TwoT_IntEstimate_BTM_BOTTOM

#Size:
#Top portfolio:
z_SIZE_TOP_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_SIZE_TOP_0.965	
z_SIZE_TOP_0.035 = qnorm(p=A, mean=0, sd=1)
z_SIZE_TOP_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_SIZE_TOP = c(MeanEstimate_SIZE_TOP - z_SIZE_TOP_0.965*
                                SDEstimate_SIZE_TOP/sqrt(n_SIZE_TOP),
                              MeanEstimate_SIZE_TOP - z_SIZE_TOP_0.035*SDEstimate_SIZE_TOP/sqrt(n_SIZE_TOP))
TwoT_IntEstimate_SIZE_TOP

#Bottom portfolio
z_SIZE_BOTTOM_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_SIZE_BOTTOM_0.965	
z_SIZE_BOTTOM_0.035 = qnorm(p=A, mean=0, sd=1)
z_SIZE_BOTTOM_0.035
#Now to calculate the actual interval estimate:
TwoT_IntEstimate_SIZE_BOTTOM = c(MeanEstimate_SIZE_BOTTOM - z_SIZE_BOTTOM_0.965*
                                   SDEstimate_SIZE_BOTTOM/sqrt(n_SIZE_BOTTOM),
                                 MeanEstimate_SIZE_BOTTOM - z_SIZE_BOTTOM_0.035*SDEstimate_SIZE_BOTTOM/sqrt(n_SIZE_BOTTOM))
TwoT_IntEstimate_SIZE_BOTTOM

#v: Is it correct to assume Normality?
#Let's check the distribution:
require(moments)
skewness(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1])
skewness(SignalsData$RET_CompP1P12[SignalsData$dBTM == 5])
skewness(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 1])
skewness(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 5])

kurtosis(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1])
kurtosis(SignalsData$RET_CompP1P12[SignalsData$dBTM == 5])
kurtosis(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 1])
kurtosis(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 5])

par(mfrow=c(2,2),mar= c(4,4,1,1), oma= c(1,2,2,2))
plot(density(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1])
     ,main="BTM Top", col="red")
plot(density(SignalsData$RET_CompP1P12[SignalsData$dBTM == 5])
     ,main="BTM Bottom", col="blue")
plot(density(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 1])
     ,main="SIZE Top", col="red")
plot(density(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 5])
     ,main="SIZE Bottom", col="blue")
mtext("p.d.f of portfolios",line=0.2,adj=0.5,outer=T, cex=1.2, 
      col= "blue")

par(mfrow=c(nr=2, nc=2))
qqnorm(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1],
       main="BTM Top vs. Normal Q-Q plot",
       xlab="Theoretical standard normal quantiles",
       ylab="BTM Top Quantiles", col="red")
qqline(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1],
       col="blue")
qqnorm(SignalsData$RET_CompP1P12[SignalsData$dBTM == 5],
       main="BTM Bottom vs. Normal Q-Q plot",
       xlab="Theoretical standard normal quantiles",
       ylab="BTM Bottom Quantiles", col="red")
qqline(SignalsData$RET_CompP1P12[SignalsData$dBTM == 5],
       col="blue")
qqnorm(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 1],
       main="Size Top vs. Normal Q-Q plot",
       xlab="Theoretical standard normal quantiles",
       ylab="Size Top Quantiles", col="red")
qqline(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 1],
       col="blue")
qqnorm(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 5],
       main="Size Bottom vs. Normal Q-Q plot",
       xlab="Theoretical standard normal quantiles",
       ylab="Size Bottom Quantiles", col="red")
qqline(SignalsData$RET_CompP1P12[SignalsData$dSIZE == 5],
       col="blue")

#vi: Difference in means assuming equal variances
#BTM:
DiffEstimates_BTM = MeanEstimate_BTM_TOP - MeanEstimate_BTM_BOTTOM
DiffEstimates_BTM
VarEstimate_BTM_TOP = var(SignalsData$RET_CompP1P12
                          [SignalsData$dBTM == 5])	
VarEstimate_BTM_TOP
VarEstimate_BTM_BOTTOM = var(SignalsData$RET_CompP1P12
                             [SignalsData$dBTM == 1])	
VarEstimate_BTM_BOTTOM	
VarEstimae_BTM_Pool = ((n_BTM_TOP-1)*VarEstimate_BTM_TOP +
                         (n_BTM_BOTTOM-1)*VarEstimate_BTM_BOTTOM)/(n_BTM_TOP+n_BTM_BOTTOM-2)
VarEstimae_BTM_Pool
SDEstimate_BTM_Pool = sqrt(VarEstimae_BTM_Pool)
SDEstimate_BTM_Pool
t_BTM_DIFF_0.965 = qt(p=1-A, df = n_BTM_TOP+n_BTM_BOTTOM - 2)
t_BTM_DIFF_0.965	
t_BTM_DIFF_0.035 = qt(p=A, df = n_BTM_TOP+n_BTM_BOTTOM - 2)
t_BTM_DIFF_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_BTM = c(DiffEstimates_BTM - t_BTM_DIFF_0.965*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM),DiffEstimates_BTM - t_BTM_DIFF_0.035*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM)) 
TwoT_DIFF_Estimate_BTM

#SIZE:
DiffEstimates_SIZE =MeanEstimate_SIZE_BOTTOM - MeanEstimate_SIZE_TOP  
DiffEstimates_SIZE
VarEstimate_SIZE_TOP = var(SignalsData$RET_CompP1P12
                           [SignalsData$dSIZE == 5])	
VarEstimate_SIZE_TOP
VarEstimate_SIZE_BOTTOM = var(SignalsData$RET_CompP1P12
                              [SignalsData$dSIZE == 1])	
VarEstimate_SIZE_BOTTOM	
VarEstimae_SIZE_Pool = ((n_SIZE_TOP-1)*VarEstimate_SIZE_TOP +
                          (n_SIZE_BOTTOM-1)*VarEstimate_SIZE_BOTTOM)/(n_SIZE_TOP+n_SIZE_BOTTOM-2)
VarEstimae_SIZE_Pool
SDEstimate_SIZE_Pool = sqrt(VarEstimae_SIZE_Pool)
SDEstimate_SIZE_Pool
t_SIZE_DIFF_0.965 = qt(p=1-A, df = n_SIZE_TOP+n_SIZE_BOTTOM - 2)
t_SIZE_DIFF_0.965	
t_SIZE_DIFF_0.035 = qt(p=A, df = n_SIZE_TOP+n_SIZE_BOTTOM - 2)
t_SIZE_DIFF_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_SIZE = c(DiffEstimates_SIZE - t_SIZE_DIFF_0.965*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM),DiffEstimates_SIZE - t_SIZE_DIFF_0.035*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM)) 
TwoT_DIFF_Estimate_SIZE



#vii: Difference in means with inequal variances
#BTM:
adj_df_BTM = (VarEstimate_BTM_TOP/n_BTM_TOP + VarEstimate_BTM_BOTTOM/n_BTM_BOTTOM)^2/((VarEstimate_BTM_TOP/n_BTM_TOP)^2/(n_BTM_TOP-1)+(VarEstimate_BTM_BOTTOM/n_BTM_BOTTOM)^2/(n_BTM_BOTTOM-1))
adj_df_BTM
t_BTM_DIFF_adj_0.965 = qt(p=1-A, df = adj_df_BTM)
t_BTM_DIFF_adj_0.965	
t_BTM_DIFF_adj_0.035 = qt(p=A, df = adj_df_BTM)
t_BTM_DIFF_adj_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_BTM = c(DiffEstimates_BTM - t_BTM_DIFF_adj_0.965*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM),DiffEstimates_BTM - t_BTM_DIFF_adj_0.035*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM)) 
TwoT_DIFF_Estimate_BTM

#SIZE:
adj_df_SIZE = (VarEstimate_SIZE_TOP/n_SIZE_TOP + VarEstimate_SIZE_BOTTOM/n_SIZE_BOTTOM)^2/((VarEstimate_SIZE_TOP/n_SIZE_TOP)^2/(n_SIZE_TOP-1)+(VarEstimate_SIZE_BOTTOM/n_SIZE_BOTTOM)^2/(n_SIZE_BOTTOM-1))
adj_df_SIZE
t_SIZE_DIFF_adj_0.965 = qt(p=1-A, df = adj_df_SIZE)
t_SIZE_DIFF_adj_0.965	
t_SIZE_DIFF_adj_0.035 = qt(p=A, df = adj_df_SIZE)
t_SIZE_DIFF_adj_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_SIZE = c(DiffEstimates_SIZE - t_SIZE_DIFF_adj_0.965*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM),DiffEstimates_SIZE - t_SIZE_DIFF_adj_0.035*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM)) 
TwoT_DIFF_Estimate_SIZE


#viii: Difference in means with inequal variances from non normal distributions
#BTM:
z_BTM_DIFF_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_BTM_DIFF_0.965	
z_BTM_DIFF_0.035 = qnorm(p=A, mean=0, sd=1)
z_BTM_DIFF_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_BTM = c(DiffEstimates_BTM - z_BTM_DIFF_0.965*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM),DiffEstimates_BTM - z_BTM_DIFF_0.035*
                             SDEstimate_BTM_Pool
                           *sqrt(1/n_BTM_TOP+1/n_BTM_BOTTOM)) 
TwoT_DIFF_Estimate_BTM

#SIZE:
z_SIZE_DIFF_0.965 = qnorm(p=1-A, mean=0, sd=1)
z_SIZE_DIFF_0.965	
z_SIZE_DIFF_0.035 = qnorm(p=A, mean=0, sd=1)
z_SIZE_DIFF_0.035

#And the actual estimation:
TwoT_DIFF_Estimate_SIZE = c(DiffEstimates_SIZE - z_SIZE_DIFF_0.965*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM),DiffEstimates_SIZE - z_SIZE_DIFF_0.035*
                              SDEstimate_SIZE_Pool
                            *sqrt(1/n_SIZE_TOP+1/n_SIZE_BOTTOM)) 
TwoT_DIFF_Estimate_SIZE

#ix:var estimate:
#BTM
VarEstimate_BTM_TOP = var(SignalsData$RET_CompP1P12
                          [SignalsData$dBTM == 5])	
VarEstimate_BTM_TOP
ChiSqr_BTM_TOP_0.965 = qchisq(p=1-A, df = n_BTM_TOP-1)
ChiSqr_BTM_TOP_0.965
ChiSqr_BTM_TOP_0.035 = qchisq(p=A, df = n_BTM_TOP-1)
ChiSqr_BTM_TOP_0.035
#The estimate:
TwoT_BTM_TOP_var = c((n_BTM_TOP-1)*VarEstimate_BTM_TOP/ChiSqr_BTM_TOP_0.965,
                     (n_BTM_TOP-1)*VarEstimate_BTM_TOP/ChiSqr_BTM_TOP_0.035)
TwoT_BTM_TOP_var

VarEstimate_BTM_BOTTOM = var(SignalsData$RET_CompP1P12
                             [SignalsData$dBTM == 1])	
VarEstimate_BTM_BOTTOM	
ChiSqr_BTM_BOTTOM_0.965 = qchisq(p=1-A, df = n_BTM_BOTTOM-1)
ChiSqr_BTM_BOTTOM_0.965
ChiSqr_BTM_BOTTOM_0.035 = qchisq(p=A, df = n_BTM_BOTTOM-1)
ChiSqr_BTM_BOTTOM_0.035
#The estimate:
TwoT_BTM_BOTTOM_var = c((n_BTM_BOTTOM-1)*VarEstimate_BTM_BOTTOM/ChiSqr_BTM_BOTTOM_0.965,
                        (n_BTM_BOTTOM-1)*VarEstimate_BTM_BOTTOM/ChiSqr_BTM_BOTTOM_0.035)
TwoT_BTM_BOTTOM_var

#SIZE
VarEstimate_SIZE_TOP = var(SignalsData$RET_CompP1P12
                           [SignalsData$dSIZE == 5])	
VarEstimate_SIZE_TOP
ChiSqr_SIZE_TOP_0.965 = qchisq(p=1-A, df = n_SIZE_TOP-1)
ChiSqr_SIZE_TOP_0.965
ChiSqr_SIZE_TOP_0.035 = qchisq(p=A, df = n_SIZE_TOP-1)
ChiSqr_SIZE_TOP_0.035
#The estimate:
TwoT_SIZE_TOP_var = c((n_SIZE_TOP-1)*VarEstimate_SIZE_TOP/ChiSqr_SIZE_TOP_0.965,
                      (n_SIZE_TOP-1)*VarEstimate_SIZE_TOP/ChiSqr_SIZE_TOP_0.035)
TwoT_SIZE_TOP_var

VarEstimate_SIZE_BOTTOM = var(SignalsData$RET_CompP1P12
                              [SignalsData$dSIZE == 1])	
VarEstimate_SIZE_BOTTOM	
ChiSqr_SIZE_BOTTOM_0.965 = qchisq(p=1-A, df = n_SIZE_BOTTOM-1)
ChiSqr_SIZE_BOTTOM_0.965
ChiSqr_SIZE_BOTTOM_0.035 = qchisq(p=A, df = n_SIZE_BOTTOM-1)
ChiSqr_SIZE_BOTTOM_0.035
#The estimate:
TwoT_SIZE_BOTTOM_var = c((n_SIZE_BOTTOM-1)*VarEstimate_SIZE_BOTTOM/ChiSqr_SIZE_BOTTOM_0.965,
                         (n_SIZE_BOTTOM-1)*VarEstimate_SIZE_BOTTOM/ChiSqr_SIZE_BOTTOM_0.035)
TwoT_SIZE_BOTTOM_var

#x: Ratio between variances:
#BTM
Var_Ratio_BTM = VarEstimate_BTM_TOP/VarEstimate_BTM_BOTTOM
Var_Ratio_BTM
F_BTM_0.965 = qf(p=1-A, df1 =n_BTM_TOP-1 ,df2 =n_BTM_BOTTOM-1)
F_BTM_0.965
F_BTM_0.035 = qf(p=A, df1 =n_BTM_TOP-1 ,df2 =n_BTM_BOTTOM-1)
F_BTM_0.035
#The estimate:
TwoT_Ratio_Estimate_BTM = c(Var_Ratio_BTM/F_BTM_0.965, Var_Ratio_BTM/F_BTM_0.035)
TwoT_Ratio_Estimate_BTM

#SIZE
Var_Ratio_SIZE = VarEstimate_SIZE_TOP/VarEstimate_SIZE_BOTTOM
Var_Ratio_SIZE
F_SIZE_0.965 = qf(p=1-A, df1 =n_SIZE_TOP-1 ,df2 =n_SIZE_BOTTOM-1)
F_SIZE_0.965
F_SIZE_0.035 = qf(p=A, df1 =n_SIZE_TOP-1 ,df2 =n_SIZE_BOTTOM-1)
F_SIZE_0.035
#The estimate:
TwoT_Ratio_Estimate_SIZE = c(Var_Ratio_SIZE/F_SIZE_0.965, Var_Ratio_SIZE/F_SIZE_0.035)
TwoT_Ratio_Estimate_SIZE

#Question 3:
#a: loading data and adding day of the week:
CRSP =read.csv(file="IndexesDATA_201503_Daily.csv",
               na.strings=c("NA",".",""))
CRSP$TrdDATE = as.Date(as.character(CRSP$TrdDATE), format='%Y%m%d')
CRSP$WEEKDAY = weekdays(CRSP$TrdDATE)
View(CRSP)

#b: Estimates:
require(moments)
Mean = tapply(CRSP$SPVWRET, CRSP$WEEKDAY, FUN=mean)
Mean	
Var = tapply(CRSP$SPVWRET, CRSP$WEEKDAY, FUN=var)
Var
Skewness = tapply(CRSP$SPVWRET, CRSP$WEEKDAY, FUN=skewness)
Skewness
Kurtosis = tapply(CRSP$SPVWRET, CRSP$WEEKDAY, FUN=kurtosis)
Kurtosis

#c: Interval estimates, assumtion of normality.

#Monday:
MeanEstimate_Monday = mean(CRSP$SPVWRET
                           [CRSP$WEEKDAY == "Monday"])
MeanEstimate_Monday
SDEstimate_Monday = sd(CRSP$SPVWRET
                       [CRSP$WEEKDAY == "Monday"])
SDEstimate_Monday
VarEstimate_Monday = var(CRSP$SPVWRET
                         [CRSP$WEEKDAY == "Monday"])	
VarEstimate_Monday	
n_Monday = length(CRSP$SPVWRET
                  [CRSP$WEEKDAY == "Monday"])
n_Monday 
t_Monday_0.965 = qt(p=1-A, df = n_Monday)
t_Monday_0.965	
t_Monday_0.035 = qt(p=A, df = n_Monday)
t_Monday_0.035
ChiSqr_Monday_0.965 = qchisq(p=1-A, df = n_Monday-1)
ChiSqr_Monday_0.965
ChiSqr_Monday_0.035 = qchisq(p=A, df = n_Monday-1)
ChiSqr_Monday_0.035

#Mean Estimate:
TwoT_IntEstimate_Monday = c(MeanEstimate_Monday - t_Monday_0.965*
                              SDEstimate_Monday/sqrt(n_Monday),
                            MeanEstimate_Monday - t_BTM_TOP_0.035*SDEstimate_Monday/sqrt(n_Monday))
TwoT_IntEstimate_Monday

#Variance Estimate:
TwoT_Monday_var = c((n_Monday-1)*VarEstimate_Monday/ChiSqr_Monday_0.965,
                    (n_Monday-1)*VarEstimate_Monday/ChiSqr_Monday_0.035)
TwoT_Monday_var 

#Tuesday:
MeanEstimate_Tuesday = mean(CRSP$SPVWRET
                            [CRSP$WEEKDAY == "Tuesday"])
MeanEstimate_Tuesday
SDEstimate_Tuesday = sd(CRSP$SPVWRET
                        [CRSP$WEEKDAY == "Tuesday"])
SDEstimate_Tuesday
VarEstimate_Tuesday = var(CRSP$SPVWRET
                          [CRSP$WEEKDAY == "Tuesday"])	
VarEstimate_Tuesday	
n_Tuesday = length(CRSP$SPVWRET
                   [CRSP$WEEKDAY == "Tuesday"])
n_Tuesday 
t_Tuesday_0.965 = qt(p=1-A, df = n_Tuesday)
t_Tuesday_0.965	
t_Tuesday_0.035 = qt(p=A, df = n_Tuesday)
t_Tuesday_0.035
ChiSqr_Tuesday_0.965 = qchisq(p=1-A, df = n_Tuesday-1)
ChiSqr_Tuesday_0.965
ChiSqr_Tuesday_0.035 = qchisq(p=A, df = n_Tuesday-1)
ChiSqr_Tuesday_0.035

#Mean Estimate:
TwoT_IntEstimate_Tuesday = c(MeanEstimate_Tuesday - t_Tuesday_0.965*
                               SDEstimate_Tuesday/sqrt(n_Tuesday),
                             MeanEstimate_Tuesday - t_BTM_TOP_0.035*SDEstimate_Tuesday/sqrt(n_Tuesday))
TwoT_IntEstimate_Tuesday

#Variance Estimate:
TwoT_Tuesday_var = c((n_Tuesday-1)*VarEstimate_Tuesday/ChiSqr_Tuesday_0.965,
                     (n_Tuesday-1)*VarEstimate_Tuesday/ChiSqr_Tuesday_0.035)
TwoT_Tuesday_var


#Wednesday:
MeanEstimate_Wednesday = mean(CRSP$SPVWRET
                              [CRSP$WEEKDAY == "Wednesday"])
MeanEstimate_Wednesday
SDEstimate_Wednesday = sd(CRSP$SPVWRET
                          [CRSP$WEEKDAY == "Wednesday"])
SDEstimate_Wednesday
VarEstimate_Wednesday = var(CRSP$SPVWRET
                            [CRSP$WEEKDAY == "Wednesday"])	
VarEstimate_Wednesday	
n_Wednesday = length(CRSP$SPVWRET
                     [CRSP$WEEKDAY == "Wednesday"])
n_Wednesday 
t_Wednesday_0.965 = qt(p=1-A, df = n_Wednesday)
t_Wednesday_0.965	
t_Wednesday_0.035 = qt(p=A, df = n_Wednesday)
t_Wednesday_0.035
ChiSqr_Wednesday_0.965 = qchisq(p=1-A, df = n_Wednesday-1)
ChiSqr_Wednesday_0.965
ChiSqr_Wednesday_0.035 = qchisq(p=A, df = n_Wednesday-1)
ChiSqr_Wednesday_0.035

#Mean Estimate:
TwoT_IntEstimate_Wednesday = c(MeanEstimate_Wednesday - t_Wednesday_0.965*
                                 SDEstimate_Wednesday/sqrt(n_Wednesday),
                               MeanEstimate_Wednesday - t_BTM_TOP_0.035*SDEstimate_Wednesday/sqrt(n_Wednesday))
TwoT_IntEstimate_Wednesday

#Variance Estimate:
TwoT_Wednesday_var = c((n_Wednesday-1)*VarEstimate_Wednesday/ChiSqr_Wednesday_0.965,
                       (n_Wednesday-1)*VarEstimate_Wednesday/ChiSqr_Wednesday_0.035)
TwoT_Wednesday_var 

#Thursday:
MeanEstimate_Thursday = mean(CRSP$SPVWRET
                             [CRSP$WEEKDAY == "Thursday"])
MeanEstimate_Thursday
SDEstimate_Thursday = sd(CRSP$SPVWRET
                         [CRSP$WEEKDAY == "Thursday"])
SDEstimate_Thursday
VarEstimate_Thursday = var(CRSP$SPVWRET
                           [CRSP$WEEKDAY == "Thursday"])	
VarEstimate_Thursday	
n_Thursday = length(CRSP$SPVWRET
                    [CRSP$WEEKDAY == "Thursday"])
n_Thursday 
t_Thursday_0.965 = qt(p=1-A, df = n_Thursday)
t_Thursday_0.965	
t_Thursday_0.035 = qt(p=A, df = n_Thursday)
t_Thursday_0.035
ChiSqr_Thursday_0.965 = qchisq(p=1-A, df = n_Thursday-1)
ChiSqr_Thursday_0.965
ChiSqr_Thursday_0.035 = qchisq(p=A, df = n_Thursday-1)
ChiSqr_Thursday_0.035

#Mean Estimate:
TwoT_IntEstimate_Thursday = c(MeanEstimate_Thursday - t_Thursday_0.965*
                                SDEstimate_Thursday/sqrt(n_Thursday),
                              MeanEstimate_Thursday - t_BTM_TOP_0.035*SDEstimate_Thursday/sqrt(n_Thursday))
TwoT_IntEstimate_Thursday

#Variance Estimate:
TwoT_Thursday_var = c((n_Thursday-1)*VarEstimate_Thursday/ChiSqr_Thursday_0.965,
                      (n_Thursday-1)*VarEstimate_Thursday/ChiSqr_Thursday_0.035)
TwoT_Thursday_var


#Friday:
MeanEstimate_Friday = mean(CRSP$SPVWRET
                           [CRSP$WEEKDAY == "Friday"])
MeanEstimate_Friday
SDEstimate_Friday = sd(CRSP$SPVWRET
                       [CRSP$WEEKDAY == "Friday"])
SDEstimate_Friday
VarEstimate_Friday = var(CRSP$SPVWRET
                         [CRSP$WEEKDAY == "Friday"])	
VarEstimate_Friday	
n_Friday = length(CRSP$SPVWRET
                  [CRSP$WEEKDAY == "Friday"])
n_Friday 
t_Friday_0.965 = qt(p=1-A, df = n_Friday)
t_Friday_0.965	
t_Friday_0.035 = qt(p=A, df = n_Friday)
t_Friday_0.035
ChiSqr_Friday_0.965 = qchisq(p=1-A, df = n_Friday-1)
ChiSqr_Friday_0.965
ChiSqr_Friday_0.035 = qchisq(p=A, df = n_Friday-1)
ChiSqr_Friday_0.035

#Mean Estimate:
TwoT_IntEstimate_Friday = c(MeanEstimate_Friday - t_Friday_0.965*
                              SDEstimate_Friday/sqrt(n_Friday),
                            MeanEstimate_Friday - t_BTM_TOP_0.035*SDEstimate_Friday/sqrt(n_Friday))
TwoT_IntEstimate_Friday

#Variance Estimate:
TwoT_Friday_var = c((n_Friday-1)*VarEstimate_Friday/ChiSqr_Friday_0.965,
                    (n_Friday-1)*VarEstimate_Friday/ChiSqr_Friday_0.035)
TwoT_Friday_var


#Saturday:
MeanEstimate_Saturday = mean(CRSP$SPVWRET
                             [CRSP$WEEKDAY == "Saturday"])
MeanEstimate_Saturday
SDEstimate_Saturday = sd(CRSP$SPVWRET
                         [CRSP$WEEKDAY == "Saturday"])
SDEstimate_Saturday
VarEstimate_Saturday = var(CRSP$SPVWRET
                           [CRSP$WEEKDAY == "Saturday"])	
VarEstimate_Saturday	
n_Saturday = length(CRSP$SPVWRET
                    [CRSP$WEEKDAY == "Saturday"])
n_Saturday 
t_Saturday_0.965 = qt(p=1-A, df = n_Saturday)
t_Saturday_0.965	
t_Saturday_0.035 = qt(p=A, df = n_Saturday)
t_Saturday_0.035
ChiSqr_Saturday_0.965 = qchisq(p=1-A, df = n_Saturday-1)
ChiSqr_Saturday_0.965
ChiSqr_Saturday_0.035 = qchisq(p=A, df = n_Saturday-1)
ChiSqr_Saturday_0.035

#Mean Estimate:
TwoT_IntEstimate_Saturday = c(MeanEstimate_Saturday - t_Saturday_0.965*
                                SDEstimate_Saturday/sqrt(n_Saturday),
                              MeanEstimate_Saturday - t_BTM_TOP_0.035*SDEstimate_Saturday/sqrt(n_Saturday))
TwoT_IntEstimate_Saturday

#Variance Estimate:
TwoT_Saturday_var = c((n_Saturday-1)*VarEstimate_Saturday/ChiSqr_Saturday_0.965,
                      (n_Saturday-1)*VarEstimate_Saturday/ChiSqr_Saturday_0.035)
TwoT_Saturday_var

#d some descriptives:
install.packages(Hmisc)
require("Hmisc")
describe(CRSP)
SAT_DATE = CRSP$TrdDATE[CRSP$WEEKDAY == "Saturday"]
max(SAT_DATE)
#Note that Saturday trading stopped in 1952, so there is no point in using it for a trading strategy.
