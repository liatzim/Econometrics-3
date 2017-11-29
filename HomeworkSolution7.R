#Suggested Solution for Module7_PS
#Question 1:
#Section a:
	Q1 = read.table("C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module7\\Module7_PS_HPRICE1.txt",
	header=F,na.string=c("na","NA","."))

	names(Q1) = c("price","assess","bdrms","lotsize","sqrft",
	"colonial","lprice","lassess","llotsize","lsqrft")
	
	FitQ1 = lm(price ~ sqrft + bdrms, data = Q1)
	Summary_FitQ1 = summary(FitQ1)
	Summary_FitQ1

	#The R-Squared:
	Summary_FitQ1$adj.r.squared
#Section b:
	CO = coefficients(FitQ1)
	CO[1] + CO[2]*2438 + CO[3]*4
#Section c:
	CO[1] + CO[2]*2438+ CO[3]*4 - 300
#Section d:
	A_ID = c(1,2,3,4,5,6,7)
	sqrft = c(3000,2457,1500,4567,1267,2387,2843)
	bdrms = c(6,4,3,8,3,4,6)
	PRED = data.frame(A_ID,sqrft)
	PRED$bdrms = bdrms
	View(PRED)

	predict(FitQ1,PRED)

	#Sub Section I:
	predict(FitQ1,PRED, se.fit=TRUE, interval="confidence", level=0.95)

	#Sub Section II:
	predict(FitQ1,PRED, se.fit=TRUE, interval="prediction", level=0.95)
	TEMP = predict(FitQ1,PRED, se.fit=TRUE, interval="prediction", level=0.95)
	Point_SE = (TEMP$se.fit^2 + TEMP$residual.scale^2)^0.5
	Point_SE

	#Sub Section III:
	#For average estimates:
	predict(FitQ1,PRED, se.fit=TRUE, interval="confidence", level=0.95)$fit
	#For Point estimates:
	z_Top = qnorm(p=0.975, mean=0, sd=1)
	z_Bottom = qnorm(p=0.025, mean=0, sd=1)
	LWR = TEMP$fit[,1] + z_Bottom*Point_SE
	UPR = TEMP$fit[,1] + z_Top*Point_SE

	INTER = data.frame(LWR,UPR)
	INTER
#Section e:
	
	require(moments)
	skewness(Summary_FitQ1$residuals)
	kurtosis(Summary_FitQ1$residuals)
	plot(density(Summary_FitQ1$residuals),main="Residuals pdf", col="red")

	require(tseries)						
	jarque.bera.test(Summary_FitQ1$residuals)
	
	#Winsorization:
	RES = Summary_FitQ1$residuals
	L = 0.01
	H = 0.99
	RES_H = quantile(RES, prob=H, na.rm=TRUE)
	RES_L = quantile(RES, prob=L, na.rm=TRUE)
	
	#Winsorization:
	RES[RES <= RES_L] = RES_L
	RES[RES > RES_H] = RES_H
	
	skewness(RES)
	kurtosis(RES)

	jarque.bera.test(RES)

	#Truncation:
	RES = Summary_FitQ1$residuals
	RES[RES <= RES_L] = NA
	RES[RES > RES_H] = NA

	RES = na.omit(RES)
	skewness(RES)
	kurtosis(RES)

	jarque.bera.test(RES)


#Question 2:
#Section a:
	SignalsData2013 =read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module7\\SignalsDATA_2013.csv",
	na.strings=c("NA",".",""))

	FitQ2 = lm(RET_CompP1P12 ~ log(SIZE) + BTM, data = SignalsData2013)
	Summary_FitQ2 = summary(FitQ2)
	Summary_FitQ2

#Section b:
	FitQ2_b = lm(RET_CompP1P12 ~ log(SIZE) + BTM + OACC, data = SignalsData2013)
	Summary_FitQ2_b = summary(FitQ2_b)
	Summary_FitQ2_b

	Summary_FitQ2_b$adj.r.squared

#Section c:
	#Step 1 - regress OACC on other variables:
	PARTIAL = lm(OACC ~ log(SIZE) + BTM, data=SignalsData2013)
	Summary_PARTIAL = summary(PARTIAL)
	Summary_PARTIAL
	View(PARTIAL$residuals)

	#Step 2 - regress RET_CompP1P12 on residuals from step 1:
	PARTIAL2 = lm(SignalsData2013$RET_CompP1P12 ~ PARTIAL$residuals)
	Summary_PARTIAL2 = summary(PARTIAL2)
	Summary_PARTIAL2

#Section d:
	#The "true" model with OACC:
	FitQ2_b = lm(RET_CompP1P12 ~ log(SIZE) + BTM + OACC, data = SignalsData2013)
	#The model with omission:
	FitQ2 = lm(RET_CompP1P12 ~ log(SIZE) + BTM, data = SignalsData2013)		

	#The observed bias for log(SIZE):
	Bias_observed = coefficients(FitQ2)["log(SIZE)"] - coefficients(FitQ2_b)["log(SIZE)"]
	Bias_observed

	#Calculation:
	Beta_Hat_3 = coefficients(FitQ2_b)["OACC"]
	Fit_OMIT = lm(OACC ~ log(SIZE) + BTM, data = SignalsData2013)
	Delta_Hat_1 = coefficients(Fit_OMIT)["log(SIZE)"]		
	Bias_CALC = Beta_Hat_3*Delta_Hat_1
	Bias_CALC

	#The observed bias for BTM:
	Bias_observed = coefficients(FitQ2)["BTM"] - coefficients(FitQ2_b)["BTM"]
	Bias_observed

	#Calculation:
	Beta_Hat_3 = coefficients(FitQ2_b)["OACC"]
	Fit_OMIT = lm(OACC ~ log(SIZE) + BTM, data = SignalsData2013)
	Delta_Hat_2 = coefficients(Fit_OMIT)["BTM"]		
	Bias_CALC = Beta_Hat_3*Delta_Hat_2
	Bias_CALC

#Section e:
	FitQ2_e = lm(log(1+RET_CompP1P12) ~ log(SIZE) + BTM, data = SignalsData2013)
	Summary_FitQ2_e = summary(FitQ2_e)
	Summary_FitQ2_e

#Section f:
	RSQ_b = (cor(SignalsData2013$RET_CompP1P12,FitQ2_b$fitted.values))^2
	RSQ_b

	RSQ_e = (cor(SignalsData2013$RET_CompP1P12,FitQ2_e$fitted.values))^2
	RSQ_e
	
	#adjusted R-Squared:
	N = length(SignalsData2013$RET_CompP1P12)
	A_RSQ_b = 1- ((1-RSQ_b)*(N-1))/((N-3-1))
	A_RSQ_b

	A_RSQ_e = 1- ((1-RSQ_e)*(N-1))/((N-2-1))
	A_RSQ_e
	
#Question 4:
#Section a:
	SignalsData =read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module7\\SignalsDATA_201508_Annual.csv",
	na.strings=c("NA",".",""))

	FitQ4_a_TOT = lm(RET_CompP1P12 ~ BTM + log(SIZE) + AG, data = SignalsData)
	Summary_FitQ4_a_TOT = summary(FitQ4_a_TOT)
	Summary_FitQ4_a_TOT

	require(nlme)
	FitQ4_a_YEAR = lmList(RET_CompP1P12 ~ BTM + log(SIZE) + AG | AnnPDATE, data = SignalsData)                     
	Summary_FitQ4_a_YEAR = summary(FitQ4_a_YEAR)  
	names(Summary_FitQ4_a_YEAR)

	Coefficients_YEAR = data.frame(Summary_FitQ4_a_YEAR$coefficients)
	names(Coefficients_YEAR)

	BTM_STAT = c(mean(Coefficients_YEAR$Estimate.BTM),median(Coefficients_YEAR$Estimate.BTM),
	sd(Coefficients_YEAR$Estimate.BTM),min(Coefficients_YEAR$Estimate.BTM),
	max(Coefficients_YEAR$Estimate.BTM),mean(Coefficients_YEAR$Std..Error.BTM),
	sd(Coefficients_YEAR$Std..Error.BTM))
	LOGSIZE_STAT = c(mean(Coefficients_YEAR$Estimate.log.SIZE),median(Coefficients_YEAR$Estimate.log.SIZE),
	sd(Coefficients_YEAR$Estimate.log.SIZE),min(Coefficients_YEAR$Estimate.log.SIZE),
	max(Coefficients_YEAR$Estimate.log.SIZE),mean(Coefficients_YEAR$Std..Error.log.SIZE),
	sd(Coefficients_YEAR$Std..Error.log.SIZE))
	AG_STAT = c(mean(Coefficients_YEAR$Estimate.AG),median(Coefficients_YEAR$Estimate.AG),
	sd(Coefficients_YEAR$Estimate.AG),min(Coefficients_YEAR$Estimate.AG),
	max(Coefficients_YEAR$Estimate.AG),mean(Coefficients_YEAR$Std..Error.AG),
	sd(Coefficients_YEAR$Std..Error.AG))

	YEAR_STAT_Q4a = rbind(BTM_STAT,LOGSIZE_STAT,AG_STAT)
	colnames(YEAR_STAT_Q4a) = c("Mean","Median","SD","Minimum","Maximum","Mean SE","SD SE")
	YEAR_STAT_Q4a

	FitQ4_a_TOT$coefficients

	#R-Squared:
	ADJRSQ_a = data.frame(matrix(unlist(Summary_FitQ4_a_YEAR$adj.r.squared), nrow=49, byrow=T))
	colnames(ADJRSQ_a) = c("Adjusted_R_Squared")
	mean(ADJRSQ_a$Adjusted_R_Squared)
	sd(ADJRSQ_a$Adjusted_R_Squared)
	median(ADJRSQ_a$Adjusted_R_Squared)
	min(ADJRSQ_a$Adjusted_R_Squared)
	max(ADJRSQ_a$Adjusted_R_Squared)
#Section b:
	SignalsData_2010 = subset(SignalsData, SignalsData$AnnPDATE == "2010-06-30")
	
	#BTM:
	Fit_BTM = lm(BTM ~ log(SIZE) + AG, data = SignalsData_2010)
	Summary_Fit_BTM = summary(Fit_BTM)
	VIF_BTM = 1/(1-Summary_Fit_BTM$r.squared)
	VIF_BTM
	
	#log(SIZE):
	Fit_LSIZE = lm(log(SIZE) ~ BTM + AG, data = SignalsData_2010)
	Summary_Fit_LSIZE = summary(Fit_LSIZE)
	VIF_LSIZE = 1/(1-Summary_Fit_LSIZE$r.squared)
	VIF_LSIZE

	#AG:
	Fit_AG = lm(AG ~ BTM + log(SIZE), data = SignalsData_2010)
	Summary_Fit_AG= summary(Fit_AG)
	VIF_AG = 1/(1-Summary_Fit_AG$r.squared)
	VIF_AG

#Section c:
	#On all the data:
	FitQ4_c_TOT = lm(RET_CompP1P12 ~ BTM + log(SIZE)+ I((log(SIZE))^2) + AG, data = SignalsData)                     
	Summary_FitQ4_c_TOT = summary(FitQ4_c_TOT)  
	Summary_FitQ4_c_TOT
	
	Summary_FitQ4_c_TOT$adj.r.squared

	#By year:
	SignalsData = na.omit(SignalsData)
	SignalsData$LSIZESQ = log(SignalsData$SIZE)^2
	FitQ4_c_YEAR = lmList(RET_CompP1P12 ~ BTM + log(SIZE)+ LSIZESQ + AG | AnnPDATE, data = SignalsData)                     
	Summary_FitQ4_c_YEAR = summary(FitQ4_c_YEAR)  
	Summary_FitQ4_c_YEAR

	Coefficients_YEAR = data.frame(Summary_FitQ4_c_YEAR$coefficients)
	names(Coefficients_YEAR)

	BTM_STAT = c(mean(Coefficients_YEAR$Estimate.BTM),median(Coefficients_YEAR$Estimate.BTM),
	sd(Coefficients_YEAR$Estimate.BTM),min(Coefficients_YEAR$Estimate.BTM),
	max(Coefficients_YEAR$Estimate.BTM),mean(Coefficients_YEAR$Std..Error.BTM),
	sd(Coefficients_YEAR$Std..Error.BTM))
	LOGSIZE_STAT = c(mean(Coefficients_YEAR$Estimate.log.SIZE),median(Coefficients_YEAR$Estimate.log.SIZE),
	sd(Coefficients_YEAR$Estimate.log.SIZE),min(Coefficients_YEAR$Estimate.log.SIZE),
	max(Coefficients_YEAR$Estimate.log.SIZE),mean(Coefficients_YEAR$Std..Error.log.SIZE),
	sd(Coefficients_YEAR$Std..Error.log.SIZE))
	LOGSIZESQ_STAT = c(mean(Coefficients_YEAR$Estimate.LSIZESQ),median(Coefficients_YEAR$Estimate.LSIZESQ),
	sd(Coefficients_YEAR$Estimate.LSIZESQ),min(Coefficients_YEAR$Estimate.LSIZESQ),
	max(Coefficients_YEAR$Estimate.LSIZESQ),mean(Coefficients_YEAR$Std..Error.LSIZESQ),
	sd(Coefficients_YEAR$Std..Error.LSIZESQ))
	AG_STAT = c(mean(Coefficients_YEAR$Estimate.AG),median(Coefficients_YEAR$Estimate.AG),
	sd(Coefficients_YEAR$Estimate.AG),min(Coefficients_YEAR$Estimate.AG),
	max(Coefficients_YEAR$Estimate.AG),mean(Coefficients_YEAR$Std..Error.AG),
	sd(Coefficients_YEAR$Std..Error.AG))

	YEAR_STAT_Q4c = rbind(BTM_STAT,LOGSIZE_STAT,LOGSIZESQ_STAT,AG_STAT)
	colnames(YEAR_STAT_Q4c) = c("Mean","Median","SD","Minimum","Maximum","Mean SE","SD SE")
	YEAR_STAT_Q4c

	#The coefficients from the total regression
	FitQ4_c_TOT$coefficients

	#R-Squared
	ADJRSQ_c = data.frame(matrix(unlist(Summary_FitQ4_c_YEAR$adj.r.squared), nrow=49, byrow=T))
	colnames(ADJRSQ_c) = c("Adjusted_R_Squared")
	mean(ADJRSQ_c$Adjusted_R_Squared)
	sd(ADJRSQ_c$Adjusted_R_Squared)
	median(ADJRSQ_c$Adjusted_R_Squared)
	min(ADJRSQ_c$Adjusted_R_Squared)
	max(ADJRSQ_c$Adjusted_R_Squared)

	
	

	