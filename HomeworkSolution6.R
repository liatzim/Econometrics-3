#Suggested Solution for Module6_PS

#Question 3:
#Section a)
	colnames(anscombe) = c("Vol1", "Vol2", "Vol3", "Vol4", "Ret1", "Ret2", "Ret3", "Ret4")

	par(mfrow=c(2, 2))
	plot(anscombe$Vol1, anscombe$Ret1, main="Ret1~Vol1")
	plot(anscombe$Vol2, anscombe$Ret2, main="Ret2~Vol2")
	plot(anscombe$Vol3, anscombe$Ret3, main="Ret3~Vol3")
	plot(anscombe$Vol4, anscombe$Ret4, main="Ret4~Vol4")
#Section b)
	Fit1 = lm(Ret1~Vol1, data=anscombe)
	Fit2 = lm(Ret2~Vol2, data=anscombe)
	Fit3 = lm(Ret3~Vol3, data=anscombe)
	Fit4 = lm(Ret4~Vol4, data=anscombe)

	Fit1$coefficients
	Fit2$coefficients
	Fit3$coefficients
	Fit4$coefficients
#Section c)
	Fit1R = lm(Vol1~Ret1, data=anscombe)
	Fit2R = lm(Vol2~Ret2, data=anscombe)
	Fit3R = lm(Vol3~Ret3, data=anscombe)
	Fit4R = lm(Vol4~Ret4, data=anscombe)

	Fit1R$coefficients
	Fit2R$coefficients
	Fit3R$coefficients
	Fit4R$coefficients

	#things that remained the same - R Squared:
	summary(Fit1)$r.squared
	summary(Fit1R)$r.squared

#Section d)
	par(mfrow=c(2, 2))
	plot(anscombe$Vol1, anscombe$Ret1, main="Ret1~Vol1")
	abline(Fit1,col="red",lwd=2)
	plot(anscombe$Vol2, anscombe$Ret2, main="Ret2~Vol2")
	abline(Fit2,col="red",lwd=2)
	plot(anscombe$Vol3, anscombe$Ret3, main="Ret3~Vol3")
	abline(Fit3,col="red",lwd=2)
	plot(anscombe$Vol4, anscombe$Ret4, main="Ret4~Vol4")
	abline(Fit4,col="red",lwd=2)

#Question 4:
#Section b)
	Q4 =read.csv(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module6\\Module6_PS_Q4.csv",
	na.strings=c("NA",".",""))
	Fit_4b = lm(Return ~ Analyst_PCT,data=Q4)
	Summary_Fit_4b = summary(Fit_4b)	
	Summary_Fit_4b
#Section c)
	#Var of Beta0:
	SUM_SQ_ANALYST = sum(Q4$Analyst_PCT^2)/length(Q4$Analyst_PCT)
	SUM_SQ_ANALYST 

	MEAN_ANALYST_PCT = mean(Q4$Analyst_PCT)
	SUM_SQ_DEV_ANALYST = sum((Q4$Analyst_PCT - MEAN_ANALYST_PCT)^2)

	Var_Beta0 = ((Summary_Fit_4b$sigma^2)*SUM_SQ_ANALYST)/SUM_SQ_DEV_ANALYST
	Var_Beta0
	Var_Beta0^0.5
	
	#Var of Beta1:
	VAR_Beta1 = (Summary_Fit_4b$sigma^2)/SUM_SQ_DEV_ANALYST
	VAR_Beta1 
	VAR_Beta1^0.5

#Section d)
	Firm = c(1)
	Analyst_PCT = c(90.625)
	Analyst = data.frame(Firm,Analyst_PCT)
	predict(Fit_4b, Analyst, se.fit=TRUE, interval="confidence", level=0.95) 

	#Check:
	Fitted_90.625 = Fit_4b$coefficients[1]+ Fit_4b$coefficients[2]*90.625
	Fitted_90.625
#Section e)
	
	predict(Fit_4b, Analyst, se.fit=TRUE, interval="prediction", level=0.95) 
	
	SE_Point = (Pred_e$se.fit^2 + Pred_e$residual.scale^2)^0.5
	SE_Point
 
#Section f)

	require(moments)
	skewness(Summary_Fit_4b$residuals)
	kurtosis(Summary_Fit_4b$residuals)

	require(tseries)						
	jarque.bera.test(Summary_Fit_4b$residuals)

	#Winsorization and truncation:
	RES = Summary_Fit_4b$residuals
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
	RES = Summary_Fit_4b$residuals
	RES[RES <= RES_L] = NA
	RES[RES > RES_H] = NA

	RES = na.omit(RES)
	skewness(RES)
	kurtosis(RES)

	jarque.bera.test(RES)

#Question 5:
	Q5 = read.table(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module6\\Module6_PS_CEOSAL2.txt",
	na.strings=c("NA",".",""))
	View(Q5)
	colnames(Q5) = c("SALARY", "AGE", "COLLEGE", "GRAD", "COMTEN", "CEOTEN", "SALES", "PROFITS",
	"MKTVAL","LSALARY", "LSALES", "LMKTVAL", "COMTENSQ", "CEOTENSQ", "PROFMARG")
	View(Q5)

#Section a)
	summary(Q5)
	require(Hmisc)
	describe(Q5)

	DISC = Q5[, c("SALARY","LSALARY","CEOTEN","PROFITS")]
	View(DISC)
	pairs(DISC, pch=23, bg="green", main="Bivariate plots")

#Section b)
	Fit_5b = lm(LSALARY ~ CEOTEN, data = Q5)
	Summary_Fit_5b = summary(Fit_5b)	
	Summary_Fit_5b

	#Exact change
	100*((exp(Summary_Fit_5b$coefficients[2]))-1)

	#R-Square:
	Summary_Fit_5b$r.squared

#Section c)
	Fit_5c = lm(SALARY ~ CEOTEN, data = Q5)
	Summary_Fit_5c = summary(Fit_5c)	
	Summary_Fit_5c

	#R-Square:
	Summary_Fit_5c$r.squared
	names(Fit_5c)
#Section d)
	#Assuming U is normally distributed:
	E_EXP_U = exp(0.5*Summary_Fit_5b$sigma)
	Y_HAT_EXP = exp(Fit_5b$fitted.values)
	Y_HAT_NEW = E_EXP_U*Y_HAT_EXP
	
	#R-Squared Log-Level:
	(cor(Q5$SALARY,Y_HAT_NEW))^2
	#R-Squared Level-Level:
	(cor(Q5$SALARY, Fit_5c$fitted.values))^2	


	#Without assuming normality and with the MOM method:
	MOM_ESTIM_U = sum(exp(Fit_5b$residuals))/length(Fit_5b$residuals)
	Y_HAT_MOM = MOM_ESTIM_U*Y_HAT_EXP	
	
	#R-Squared Log-Level:
	(cor(Q5$SALARY,Y_HAT_MOM))^2
	#R-Squared Level-Level:
	(cor(Q5$SALARY, Fit_5c$fitted.values))^2	

#Section e)
	Q5EF = read.table(file="C:\\Users\\Jonathan1\\Desktop\\School\\TA\\MA\\Econometrics M.A\\Econometrics 2\\Problem Sets\\PSetSol_Module6\\Module6_PS_RDCHEM.txt",
	header=, as.is=TRUE, na.strings=c("NA", ".", ""))
	colnames(Q5EF) = c("RD", "SALES", "PROFITS", "RDINTENS", "PROFMARG", "SALESSQ", "LSALES", "LRD")
	
	#SLR with no restriction:
	Fit_5e_SLR = lm(SALES ~ RD, data=Q5EF)
	Summary_Fit_5e_SLR = summary(Fit_5e_SLR)
	Summary_Fit_5e_SLR

	#R-Squared:
	Summary_Fit_5e_SLR$r.squared
	
	#SLR Through the Origin:
	Fit_5e_RTO = lm(SALES ~ RD - 1, data=Q5EF)
	Summary_Fit_5e_RTO = summary(Fit_5e_RTO)
	Summary_Fit_5e_RTO

	#Reported R-Squared:
	Summary_Fit_5e_RTO$r.squared
	
#Section f)
	#First Method:
	R_SQ_LIKE = 1-(sum((Fit_5e_RTO$residuals)^2))/(sum((Q5EF$SALES)^2))

	#SLR R-Squared:
	Summary_Fit_5e_SLR$r.squared
	#RTO R-Squared like measure:
	R_SQ_LIKE

	#Second Method:
	(cor(Q5EF$SALES, Fit_5e_RTO$fitted.values))^2


