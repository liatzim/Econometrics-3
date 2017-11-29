#Suggested solution for Module 8 Problem Set
#Question 1:
#Section 1:
	SignalsData2013 =read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module7\\SignalsDATA_2013.csv",
	na.strings=c("NA",".",""))
	
	SignalsData2013$FDATE = as.character(SignalsData2013$FDATE)
	SignalsData2013$FDATE = as.Date(SignalsData2013$FDATE,format = "%Y-%m-%d")
	class(SignalsData2013$FDATE)
	SignalsData2013$FDATE[1:5]

	SignalsData2013$AnnPDATE = as.character(SignalsData2013$AnnPDATE)
	SignalsData2013$AnnPDATE = as.Date(SignalsData2013$AnnPDATE,format = "%Y-%m-%d")
	class(SignalsData2013$AnnPDATE)
	SignalsData2013$AnnPDATE[1:5]

	#Deciles:
	SignalsData2013$dBTM = ave(SignalsData2013$BTM, SignalsData2013$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData2013$dBTM = (((SignalsData2013$dBTM - 1)/9) - 0.5)
	
	SignalsData2013$dSIZE = ave(SignalsData2013$SIZE, SignalsData2013$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData2013$dSIZE = (((SignalsData2013$dSIZE - 1)/9) - 0.5)

	SignalsData2013$dOACC = ave(SignalsData2013$OACC, SignalsData2013$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData2013$dOACC = (((SignalsData2013$dOACC - 1)/9) - 0.5)
	
	SignalsData2013$dBookLEV= ave(SignalsData2013$BookLEV, SignalsData2013$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData2013$dBookLEV = (((SignalsData2013$dBookLEV - 1)/9) - 0.5)

	SignalsData2013$dMktLEV = ave(SignalsData2013$MktLEV, SignalsData2013$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData2013$dMktLEV = (((SignalsData2013$dMktLEV - 1)/9) - 0.5)

	summary(SignalsData2013$dBookLEV)

#Section 2:
	#The model:
	Fit_2 = lm(RET_CompP1P12 ~ dBTM + dSIZE + dOACC + dBookLEV + 
	dMktLEV, data = SignalsData2013)
	Summary_Fit_2 = summary(Fit_2)
	Summary_Fit_2
#Section 3:
	COEF_2 = data.frame(Summary_Fit_2$coefficients) 
	View(COEF_2)

	SIG = subset(COEF_2, COEF_2$Pr...t.. <= 0.01)
	NSIG = subset(COEF_2, COEF_2$Pr...t.. > 0.01)

	#Estimates that are different from 0:
	SIG
	#Estimates that are not different from 0:
	NSIG

	#Confidence intervals:
	z_Top = qnorm(p=0.985, mean=0, sd=1)
	z_Bottom = qnorm(p=0.015, mean=0, sd=1)
	COEF_2$LWR = COEF_2$Estimate + z_Bottom*COEF_2$Std..Error
	COEF_2$UPR = COEF_2$Estimate + z_Top*COEF_2$Std..Error
	COEF_2

#Section 4:

	SignalsData =read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module8\\SignalsDATA_201508_Annual.csv",
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

	#Deciles:
	SignalsData$dBTM = ave(SignalsData$BTM, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData$dBTM = (((SignalsData$dBTM - 1)/9) - 0.5)
	
	SignalsData$dSIZE = ave(SignalsData$SIZE, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData$dSIZE = (((SignalsData$dSIZE - 1)/9) - 0.5)

	SignalsData$dOACC = ave(SignalsData$OACC, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData$dOACC = (((SignalsData$dOACC - 1)/9) - 0.5)
	
	SignalsData$dBookLEV= ave(SignalsData$BookLEV, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData$dBookLEV = (((SignalsData$dBookLEV - 1)/9) - 0.5)

	SignalsData$dMktLEV = ave(SignalsData$MktLEV, SignalsData$AnnPDATE,
	FUN=function(x) {cut(x,breaks=quantile(x,probs=seq(0,1,0.1)),
	include.lowest=TRUE)})
	SignalsData$dMktLEV = (((SignalsData$dMktLEV - 1)/9) - 0.5)

	summary(SignalsData$dMktLEV)

	SignalsData = na.omit(SignalsData)
	#The model:
	require(nlme)
	Fit_4 = lmList(RET_CompP1P12 ~ dBTM + dSIZE + dOACC + dBookLEV + 
	dMktLEV | AnnPDATE, data = SignalsData)
	Summary_Fit_4 = summary(Fit_4)
	Summary_Fit_4

	Coefficients_YEAR = data.frame(Summary_Fit_4$coefficients)
	names(Coefficients_YEAR)

	dBTM_STAT = c(mean(Coefficients_YEAR$Estimate.dBTM),median(Coefficients_YEAR$Estimate.dBTM),
	sd(Coefficients_YEAR$Estimate.dBTM),min(Coefficients_YEAR$Estimate.dBTM),
	max(Coefficients_YEAR$Estimate.dBTM),mean(Coefficients_YEAR$Std..Error.dBTM),
	sd(Coefficients_YEAR$Std..Error.dBTM))
	dSIZE_STAT = c(mean(Coefficients_YEAR$Estimate.dSIZE),median(Coefficients_YEAR$Estimate.dSIZE),
	sd(Coefficients_YEAR$Estimate.dSIZE),min(Coefficients_YEAR$Estimate.dSIZE),
	max(Coefficients_YEAR$Estimate.dSIZE),mean(Coefficients_YEAR$Std..Error.dSIZE),
	sd(Coefficients_YEAR$Std..Error.dSIZE))
	dOACC_STAT = c(mean(Coefficients_YEAR$Estimate.dOACC),median(Coefficients_YEAR$Estimate.dOACC),
	sd(Coefficients_YEAR$Estimate.dOACC),min(Coefficients_YEAR$Estimate.dOACC),
	max(Coefficients_YEAR$Estimate.dOACC),mean(Coefficients_YEAR$Std..Error.dOACC),
	sd(Coefficients_YEAR$Std..Error.dOACC))
	dBookLEV_STAT = c(mean(Coefficients_YEAR$Estimate.dBookLEV),median(Coefficients_YEAR$Estimate.dBookLEV),
	sd(Coefficients_YEAR$Estimate.dBookLEV),min(Coefficients_YEAR$Estimate.dBookLEV),
	max(Coefficients_YEAR$Estimate.dBookLEV),mean(Coefficients_YEAR$Std..Error.dBookLEV),
	sd(Coefficients_YEAR$Std..Error.dBookLEV))
	dMktLEV_STAT = c(mean(Coefficients_YEAR$Estimate.dMktLEV),median(Coefficients_YEAR$Estimate.dMktLEV),
	sd(Coefficients_YEAR$Estimate.dMktLEV),min(Coefficients_YEAR$Estimate.dMktLEV),
	max(Coefficients_YEAR$Estimate.dMktLEV),mean(Coefficients_YEAR$Std..Error.dMktLEV),
	sd(Coefficients_YEAR$Std..Error.dMktLEV))

	YEAR_STAT_4 = rbind(dBTM_STAT,dSIZE_STAT,dOACC_STAT,dBookLEV_STAT,dMktLEV_STAT)
	colnames(YEAR_STAT_4) = c("Mean","Median","SD","Minimum","Maximum","Mean SE","SD SE")
	YEAR_STAT_4

	#Significant years, at a 1% level test:
	#BTM:
	BTM_SIG = subset(Coefficients_YEAR, Coefficients_YEAR$Pr...t...dBTM <= 0.01)
	BTM_SIG[,c("Estimate.dBTM","Pr...t...dBTM")]
	
	#SIZE:
	SIZE_SIG = subset(Coefficients_YEAR, Coefficients_YEAR$Pr...t...dSIZE <= 0.01)
	SIZE_SIG[,c("Estimate.dSIZE","Pr...t...dSIZE")]

	#OACC:
	OACC_SIG = subset(Coefficients_YEAR, Coefficients_YEAR$Pr...t...dOACC <= 0.01)
	OACC_SIG[,c("Estimate.dOACC","Pr...t...dOACC")]

	#BookLEV
	BookLEV_SIG = subset(Coefficients_YEAR, Coefficients_YEAR$Pr...t...dBookLEV <= 0.01)
	BookLEV_SIG[,c("Estimate.dBookLEV","Pr...t...dBookLEV")]

	#MktLEV:
	MktLEV_SIG = subset(Coefficients_YEAR, Coefficients_YEAR$Pr...t...dMktLEV <= 0.01)
	MktLEV_SIG[,c("Estimate.dMktLEV","Pr...t...dMktLEV")]

	#Confidence intervals:
	Coefficients_YEAR = cbind(Row.Names = rownames(Coefficients_YEAR), Coefficients_YEAR)
	z_Top = qnorm(p=0.985, mean=0, sd=1)
	z_Bottom = qnorm(p=0.015, mean=0, sd=1)

	#BTM:
	Coefficients_YEAR$BTM.LWR = Coefficients_YEAR$Estimate.dBTM + 
	z_Bottom*Coefficients_YEAR$Std..Error.dBTM
	Coefficients_YEAR$BTM.UPR = Coefficients_YEAR$Estimate.dBTM + 
	z_Top*Coefficients_YEAR$Std..Error.dBTM
	Coefficients_YEAR[,c("Row.Names","Estimate.dBTM","BTM.LWR","BTM.UPR")]
	head(Coefficients_YEAR[,c("Row.Names","Estimate.dBTM","BTM.LWR","BTM.UPR")])
	tail(Coefficients_YEAR[,c("Row.Names","Estimate.dBTM","BTM.LWR","BTM.UPR")])

	#SIZE:
	Coefficients_YEAR$SIZE.LWR = Coefficients_YEAR$Estimate.dSIZE + 
	z_Bottom*Coefficients_YEAR$Std..Error.dSIZE
	Coefficients_YEAR$SIZE.UPR = Coefficients_YEAR$Estimate.dSIZE + 
	z_Top*Coefficients_YEAR$Std..Error.dSIZE
	Coefficients_YEAR[,c("Row.Names","Estimate.dSIZE","SIZE.LWR","SIZE.UPR")]
	head(Coefficients_YEAR[,c("Row.Names","Estimate.dSIZE","SIZE.LWR","SIZE.UPR")])
	tail(Coefficients_YEAR[,c("Row.Names","Estimate.dSIZE","SIZE.LWR","SIZE.UPR")])
	
	#OACC:
	Coefficients_YEAR$OACC.LWR = Coefficients_YEAR$Estimate.dOACC + 
	z_Bottom*Coefficients_YEAR$Std..Error.dOACC
	Coefficients_YEAR$OACC.UPR = Coefficients_YEAR$Estimate.dOACC + 
	z_Top*Coefficients_YEAR$Std..Error.dOACC
	head(Coefficients_YEAR[,c("Row.Names","Estimate.dOACC","OACC.LWR","OACC.UPR")])
	tail(Coefficients_YEAR[,c("Row.Names","Estimate.dOACC","OACC.LWR","OACC.UPR")])
	Coefficients_YEAR[,c("Row.Names","Estimate.dOACC","OACC.LWR","OACC.UPR")]
	
	#BookLEV:
	Coefficients_YEAR$BookLEV.LWR = Coefficients_YEAR$Estimate.dBookLEV+ 
	z_Bottom*Coefficients_YEAR$Std..Error.dBookLEV
	Coefficients_YEAR$BookLEV.UPR = Coefficients_YEAR$Estimate.dBookLEV+ 
	z_Top*Coefficients_YEAR$Std..Error.dBookLEV
	head(Coefficients_YEAR[,c("Row.Names","Estimate.dBookLEV","BookLEV.LWR","BookLEV.UPR")])
	tail(Coefficients_YEAR[,c("Row.Names","Estimate.dBookLEV","BookLEV.LWR","BookLEV.UPR")])
	Coefficients_YEAR[,c("Row.Names","Estimate.dBookLEV","BookLEV.LWR","BookLEV.UPR")]
	
	#MktLEV:
	Coefficients_YEAR$MktLEV.LWR = Coefficients_YEAR$Estimate.dMktLEV+ 
	z_Bottom*Coefficients_YEAR$Std..Error.dMktLEV
	Coefficients_YEAR$MktLEV.UPR = Coefficients_YEAR$Estimate.dMktLEV+ 
	z_Top*Coefficients_YEAR$Std..Error.dMktLEV
	head(Coefficients_YEAR[,c("Row.Names","Estimate.dMktLEV","MktLEV.LWR","MktLEV.UPR")])
	tail(Coefficients_YEAR[,c("Row.Names","Estimate.dMktLEV","MktLEV.LWR","MktLEV.UPR")])
	Coefficients_YEAR[,c("Row.Names","Estimate.dMktLEV","MktLEV.LWR","MktLEV.UPR")]

#Section 5:
	SignalsData2013$dOACCdBTM = SignalsData2013$dOACC + SignalsData2013$dBTM
	Fit_5 = lm(RET_CompP1P12 ~ dBTM + dSIZE + dOACCdBTM + dBookLEV + 
	dMktLEV, data = SignalsData2013)
	Summary_Fit_5 = summary(Fit_5)
	Summary_Fit_5

#Section 6:
	#The unrestricted model:
	Fit_6U = lm(RET_CompP1P12 ~ dBTM + dSIZE + dOACC + dBookLEV + 
	dMktLEV, data = SignalsData2013)
	Summary_Fit_6U = summary(Fit_6U)
	Summary_Fit_6U

	#The restricted model:
	Fit_6R = lm(RET_CompP1P12 ~ dBTM + dSIZE + dOACC ,
	data = SignalsData2013)
	Summary_Fit_6R = summary(Fit_6R)
	Summary_Fit_6R

	#Sum of Squared Residuals:
	SSRur = sum((Summary_Fit_6U$residuals^2))
	SSRr = sum(Summary_Fit_6R$residuals^2)

	N = length(SignalsData2013$RET_CompP1P12)
	q = (N-3-1)-(N-5-1)

	F = ((SSRr-SSRur)/q) / (SSRur/(N-5-1)) 
	F

	#Compare to:
	1 - pf(F,N-3-1,N-5-1)

	#Alternative method with R-Squared:
	F_ALT = ((Summary_Fit_6U$r.squared - Summary_Fit_6R$r.squared)/q) / 
	((1-Summary_Fit_6U$r.squared)/(N-5-1))
	F_ALT	

	#Lagrange Multiplier approach:
	#Step1:
	#The restricted model:
	Fit_6R = lm(RET_CompP1P12 ~ dBTM + dSIZE + dOACC ,
	data = SignalsData2013)
	Summary_Fit_6R = summary(Fit_6R)
	Summary_Fit_6R

	head(Summary_Fit_6R$residuals)

	#Step 2:
	Fit_LM = lm(Summary_Fit_6R$residuals ~ SignalsData2013$dBTM + SignalsData2013$dSIZE 
	+ SignalsData2013$dOACC + SignalsData2013$dBookLEV + SignalsData2013$dMktLEV)
	Summary_Fit_LM = summary(Fit_LM)
	Summary_Fit_LM

	Summary_Fit_LM$r.squared

	#Step 3:
	NLM = Summary_Fit_LM$r.squared*length(Summary_Fit_6R$residuals)
	NLM

	#Step 4: 
	1 - pchisq(q = NLM, df = (N-5-1))
	

#Section 7:
	require(lmtest)
	#Perform a Breusch-Pagan Test:
	bptest(Fit_2)

	#Correction of Variance:
	require(sandwich)
	vcovHC(Fit_2)

	#Extract only Variance:
	VcovHC = vcovHC(Fit_2)
	SE_HC = diag(VcovHC)
	SE_HC

	#Calculate intervals for Section 3:
	z_Top = qnorm(p=0.985, mean=0, sd=1)
	z_Bottom = qnorm(p=0.015, mean=0, sd=1)
	COEF_2$LWR = COEF_2$Estimate + z_Bottom*SE_HC
	COEF_2$UPR = COEF_2$Estimate + z_Top*SE_HC
	COEF_2

	


	