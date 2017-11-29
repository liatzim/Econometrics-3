
#Question 1:
SignalsData =read.csv("signalsData.csv",
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

	#Quantiles:
	SignalsData$dOACC = ave(SignalsData$OACC, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.2)),
	include.lowest=TRUE)})

#ii: Variance estimates for each portfolio:
	require("doBy")
	summaryBy(RET_CompP1P12 ~ dOACC, data = SignalsData,
	FUN = c(sd))

#iii:
	#Parametric:
	F_TEST_P = var.test(SignalsData$RET_CompP1P12[SignalsData$dOACC==1],
	SignalsData$RET_CompP1P12[SignalsData$dOACC==3],ratio=1, 
	alternative="two.sided", conf.level=0.95)
	F_TEST_P
	

	#a.
	F_TEST_Less = var.test(SignalsData$RET_CompP1P12[SignalsData$dOACC==1],
	SignalsData$RET_CompP1P12[SignalsData$dOACC==3],ratio=1, 
	alternative="greater", conf.level=0.95)
	F_TEST_Less
	
	F_TEST_Greater = var.test(SignalsData$RET_CompP1P12[SignalsData$dOACC==1],
	SignalsData$RET_CompP1P12[SignalsData$dOACC==3],ratio=1, 
	alternative="less", conf.level=0.95)
	F_TEST_Greater

#iv:
	require(moments)
	#Skewness:
	Skew_Stat = (skewness(SignalsData$RET_CompP1P12[SignalsData$dOACC==1])-0)/
	(sqrt(6/length(SignalsData$RET_CompP1P12[SignalsData$dOACC==1])))
	Skew_Stat 
	
	Skew_p_val = 2*(1 - pnorm(q=Skew_Stat, mean=0, sd=1))
	Skew_p_val

#v:
	#Kurtosis:
	Kurt_Stat = (kurtosis(SignalsData$RET_CompP1P12[SignalsData$dOACC==1])-3)/
	(sqrt(24/length(SignalsData$RET_CompP1P12[SignalsData$dOACC==1])))
	Kurt_Stat
	
	Kurt_p_val = 2*(1 - pnorm(q=Kurt_Stat, mean=0, sd=1))
	Kurt_p_val


#Question 2:
#i:
	#Loading the data and ranking:
	#Deciles:
	SignalsData$dBTM = ave(SignalsData$BTM, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})

	View(SignalsData)
	require(Hmisc)
	describe(SignalsData_SUB$dBTM)
#ii:
	#a:
	SignalsData_SUB = subset(SignalsData, SignalsData$dBTM == 1 | 
	SignalsData$dBTM == 5 |	SignalsData$dBTM == 10)
	head(SignalsData_SUB)	
	View(SignalsData_SUB)

	#Parametric Test (wrong):
	pT_Test_MeanP = pairwise.t.test(SignalsData_SUB$RET_CompP1P12, 
	SignalsData_SUB$dBTM, p.adjust.method="bonferroni",pool.sd= FALSE,
	alternative="two.sided")
	pT_Test_MeanP

	#Non-Parametric Test:
	pT_Test_MeanNP = kruskal.test(SignalsData_SUB$RET_CompP1P12, 
	SignalsData_SUB$dBTM, p.adjust.method="bonferroni",pool.sd= FALSE,
	alternative="two.sided")
	pT_Test_MeanNP
	
	#b: where does the change come from:
	#new alpha:
	Alpha = 0.05/3

	
	
	W_Test_10_5 = wilcox.test(SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM == 10],
	SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM ==5],mu=0,alternative = "two.sided",
	conf.level=1-Alpha, conf.int=TRUE, paired=TRUE)
	W_Test_10_5

	W_Test_1_5 =  wilcox.test(SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM == 1],
	SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM ==5],mu=0,alternative = "two.sided",
	conf.level=1-Alpha, conf.int=TRUE, paired=FALSE)
	W_Test_1_5

	W_Test_1_10 =  wilcox.test(SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM == 10],
	SignalsData_SUB$RET_CompP1P12[SignalsData_SUB$dBTM == 1],mu=0,alternative = "two.sided",
	conf.level=1-Alpha, conf.int=TRUE, paired=FALSE)
	W_Test_1_10

	 
#iii:
	#a
	#Non-Parametric:
	F_Var_TestNP = fligner.test(RET_CompP1P12 ~ dBTM, data = SignalsData_SUB)
	F_Var_TestNP

	#b: where does the change come from:

	F_Var_TestNP_1_5 = fligner.test(RET_CompP1P12 ~ (dBTM==1|dBTM==5), data = SignalsData_SUB)
	F_Var_TestNP_1_5

	F_Var_TestNP_1_10 = fligner.test(RET_CompP1P12 ~ (dBTM==1|dBTM==10), data = SignalsData_SUB)
	F_Var_TestNP_1_10
	
	F_Var_TestNP_10_5 = fligner.test(RET_CompP1P12 ~ (dBTM==10|dBTM==5), data = SignalsData_SUB)
	F_Var_TestNP_10_5



#Question 3:
#i: Loading data and determining whether there are outliers or not:
	require(moments)
	skewness(SignalsData$RET_CompP1P12)
	skewness(SignalsData$SIZE)
	skewness(SignalsData$BTM)
	skewness(SignalsData$MktLEV)
	skewness(SignalsData$BookLEV)

	kurtosis(SignalsData$RET_CompP1P12)
	kurtosis(SignalsData$SIZE)
	kurtosis(SignalsData$BTM)
	kurtosis(SignalsData$MktLEV)
	kurtosis(SignalsData$BookLEV)

	par(mfrow=c(5,2),mar= c(4,4,1,1), oma= c(1,2,2,2))
	plot(density(SignalsData$RET_CompP1P12)
	,main="Return", col="red")
	qqnorm(SignalsData$RET_CompP1P12,
	main="Return vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="Return Quantiles", col="red")
	qqline(SignalsData$RET_CompP1P12[SignalsData$dBTM == 1],
	col="blue")
	
	plot(density(SignalsData$SIZE)
	,main="Size", col="red")
	qqnorm(SignalsData$SIZE,
	main="Size vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="Size Quantiles", col="red")
	qqline(SignalsData$SIZE,
	col="blue")

	plot(density(SignalsData$BTM)
	,main="BTM", col="red")
	qqnorm(SignalsData$BTM,
	main="BTM vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="BTM Quantiles", col="red")
	qqline(SignalsData$BTM,
	col="blue")

	plot(density(SignalsData$MktLEV)
	,main="MktLEV", col="red")
	qqnorm(SignalsData$MktLEV,
	main="MktLEV vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="MktLEV Quantiles", col="red")
	qqline(SignalsData$MktLEV,
	col="blue")

	plot(density(SignalsData$BookLEV)
	,main="BookLEV", col="red")
	qqnorm(SignalsData$BookLEV,
	main="BookLEV vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="BookLEV Quantiles", col="red")
	qqline(SignalsData$BookLEV,
	col="blue")

	
#ii: Test of Normality:
	install.packages("tseries", dependencies=TRUE)
	require(tseries)

	JB_RET = jarque.bera.test(SignalsData$RET_CompP1P12)
	JB_RET

	JB_SIZE = jarque.bera.test(SignalsData$SIZE)
	JB_SIZE

	JB_BTM = jarque.bera.test(SignalsData$BTM)
	JB_BTM

	JB_MktLEV = jarque.bera.test(SignalsData$MktLEV)
	JB_MktLEV

	JB_BookLEV = jarque.bera.test(SignalsData$BookLEV)
	JB_BookLEV	

#iii:
	#Winsorization:
	#RET_CompP1P12	
	L = 0.02
	H = 0.98
	#RET_CompP1P12:
	RET_CompP1P12_L = quantile(SignalsData$RET_CompP1P12, prob=L,na.rm=T)
	RET_CompP1P12_H = quantile(SignalsData$RET_CompP1P12, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$RET_CompP1P12[C_SignalsData$RET_CompP1P12<=RET_CompP1P12_L] = RET_CompP1P12_L
	C_SignalsData$RET_CompP1P12[C_SignalsData$RET_CompP1P12>=RET_CompP1P12_H] = RET_CompP1P12_H
	
	#The test:
	JB_RET = jarque.bera.test(C_SignalsData$RET_CompP1P12)
	JB_RET
	
	#SIZE:
	SIZE_L = quantile(SignalsData$SIZE, prob=L,na.rm=T)
	SIZE_H = quantile(SignalsData$SIZE, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$SIZE[C_SignalsData$SIZE<=SIZE_L] = SIZE_L
	C_SignalsData$SIZE[C_SignalsData$SIZE>=SIZE_H] = SIZE_H
	
	#The test:
	JB_SIZE = jarque.bera.test(C_SignalsData$SIZE)
	JB_SIZE

	#BTM:
	BTM_L = quantile(SignalsData$BTM, prob=L,na.rm=T)
	BTM_H = quantile(SignalsData$BTM, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$BTM[C_SignalsData$BTM<=BTM_L] = BTM_L
	C_SignalsData$BTM[C_SignalsData$BTM>=BTM_H] = BTM_H
	
	#The test:
	JB_BTM = jarque.bera.test(C_SignalsData$BTM)
	JB_BTM
	
	#MktLEV:
	MktLEV_L = quantile(SignalsData$MktLEV, prob=L,na.rm=T)
	MktLEV_H = quantile(SignalsData$MktLEV, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$MktLEV[C_SignalsData$MktLEV<=MktLEV_L] = MktLEV_L
	C_SignalsData$MktLEV[C_SignalsData$MktLEV>=MktLEV_H] = MktLEV_H
	
	#The test:
	JB_MktLEV = jarque.bera.test(C_SignalsData$MktLEV)
	JB_MktLEV

	#BookLEV:
	BookLEV_L = quantile(SignalsData$BookLEV, prob=L,na.rm=T)
	BookLEV_H = quantile(SignalsData$BookLEV, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$BookLEV[C_SignalsData$BookLEV<=BookLEV_L] = BookLEV_L
	C_SignalsData$BookLEV[C_SignalsData$BookLEV>=BookLEV_H] = BookLEV_H
	
	#The test:
	JB_BookLEV = jarque.bera.test(C_SignalsData$BookLEV)
	JB_BookLEV

#iv:
	#Truncation:	
	L = 0.02
	H = 0.98
	#RET_CompP1P12:
	RET_CompP1P12_L = quantile(SignalsData$RET_CompP1P12, prob=L,na.rm=T)
	RET_CompP1P12_H = quantile(SignalsData$RET_CompP1P12, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$RET_CompP1P12[C_SignalsData$RET_CompP1P12<=RET_CompP1P12_L] = NA
	C_SignalsData$RET_CompP1P12[C_SignalsData$RET_CompP1P12>=RET_CompP1P12_H] = NA
	C_SignalsData = na.omit(C_SignalsData)
	
	#The test:
	JB_RET = jarque.bera.test(C_SignalsData$RET_CompP1P12)
	JB_RET
	
	#SIZE:
	SIZE_L = quantile(SignalsData$SIZE, prob=L,na.rm=T)
	SIZE_H = quantile(SignalsData$SIZE, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$SIZE[C_SignalsData$SIZE<=SIZE_L] = NA
	C_SignalsData$SIZE[C_SignalsData$SIZE>=SIZE_H] = NA
	C_SignalsData = na.omit(C_SignalsData)

	#The test:
	JB_SIZE = jarque.bera.test(C_SignalsData$SIZE)
	JB_SIZE

	#BTM:
	BTM_L = quantile(SignalsData$BTM, prob=L,na.rm=T)
	BTM_H = quantile(SignalsData$BTM, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$BTM[C_SignalsData$BTM<=BTM_L] = NA
	C_SignalsData$BTM[C_SignalsData$BTM>=BTM_H] = NA
	C_SignalsData = na.omit(C_SignalsData)

	#The test:
	JB_BTM = jarque.bera.test(C_SignalsData$BTM)
	JB_BTM
	
	#MktLEV:
	MktLEV_L = quantile(SignalsData$MktLEV, prob=L,na.rm=T)
	MktLEV_H = quantile(SignalsData$MktLEV, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$MktLEV[C_SignalsData$MktLEV<=MktLEV_L] = NA
	C_SignalsData$MktLEV[C_SignalsData$MktLEV>=MktLEV_H] = NA
	C_SignalsData = na.omit(C_SignalsData)

	#The test:
	JB_MktLEV = jarque.bera.test(C_SignalsData$MktLEV)
	JB_MktLEV

	#BookLEV:
	BookLEV_L = quantile(SignalsData$BookLEV, prob=L,na.rm=T)
	BookLEV_H = quantile(SignalsData$BookLEV, prob=H,na.rm=T)
	C_SignalsData = SignalsData
	C_SignalsData$BookLEV[C_SignalsData$BookLEV<=BookLEV_L] = NA
	C_SignalsData$BookLEV[C_SignalsData$BookLEV>=BookLEV_H] = NA
	C_SignalsData = na.omit(C_SignalsData)

	#The test:
	JB_BookLEV = jarque.bera.test(C_SignalsData$BookLEV)
	JB_BookLEV

	
	