##This file conatains a suggested solution to Module_2_PSet

##Question 1:
#iii: The price goes up\down according to a Binomial distribution:
	dbinom(x = 80,size = 100, prob = 0.65)
#iv: The probability the price will go up at least 80 times:
	pbinom(80, size = 100, prob = 0.65)
#v:  The number of trades with a positive return, with probability 0.8:
	qbinom(0.8,100,0.65)
#vi:
	nSuccess = 0:20
	totVal = nSuccess*100 - (20 - nSuccess)*200
	totVal
	pmf_totVal = dbinom(x=nSuccess, size=20, prob=0.8)
	pmf_totVal
 
	ExpVal = sum(totVal*pmf_totVal)
	ExpVal
 
	VarVal = sum(((totVal - ExpVal)^2)*pmf_totVal)
	VarVal

##Question 2:
#i: 5th quantile of the normal distribution:
	qnorm(p=0.05, mean=0.001, sd=0.1)
#ii: $10 million times the return that occurs 5% of the time:
	10000000*(qnorm(p=0.05, mean=0.001, sd=0.1))/100 
#iii: 
	-500/(qnorm(p=0.1, mean=0.001, sd=0.1)/100)

##Question 3:
#i: load the file and plot the empirical density and empirical cumulative density functions of the VWRET
	CRSP =read.csv(file="IndexesDATA_201503_Daily.csv",
		na.strings=c("NA",".",""))
	head(CRSP)
#ii:  5 highest values of SVWRET:
	head(sort(CRSP$SPVWRET,TRUE),5)
#iii: 5 lowest values
	tail(sort(CRSP$SPVWRET,TRUE),5)
#iv: Percentiles:
	quantile(CRSP$SPVWRET, prob = c(0.85))
	quantile(CRSP$SPVWRET, prob = c(0.15))
#v:   create a column in the data frame with the day of the week as a factor
	CRSP$TrdDATE = as.Date(as.character(CRSP$TrdDATE),
	format = "%Y%m%d")
	class(CRSP$TrdDATE)
	CRSP$WEEKDAY = factor(weekdays(CRSP$TrdDATE))	
	class(CRSP$WEEKDAY)
	summary(CRSP$WEEKDAY)
	#Calculating stats:
	tapply(CRSP$SPVWRET,CRSP$WEEKDAY,FUN = mean)
	tapply(CRSP$SPVWRET,CRSP$WEEKDAY,FUN = median)
	tapply(CRSP$SPVWRET,CRSP$WEEKDAY,FUN = var)
#vi: plots:
	par(mfrow=c(nr=1,nc=2))
	plot(density(CRSP$VWRET), main="Name and ID", xlab="SPVWRET", 
	ylab="p.d.f of SPVWRET", col="blue")
	plot(ecdf(CRSP$SPVWRET), main="Name and ID", xlab="SPVWRET",
	ylab="The c.d.f of SPVWRET", col="red")
#vii: Skewness and Kurtosis
	require(moments)
	skewness(CRSP$VWRET)
	kurtosis(CRSP$VWRET)

#Question 4:
#i: Load data and combine:
	AAPL = read.csv(file="AAPL.csv", na.strings=c("NA",".",""))
	IBM = read.csv(file="IBM.csv",na.strings=c("NA",".",""))
	CSCO = read.csv(file="CSCO.csv",na.strings=c("NA",".",""))
	head(AAPL)
	head(IBM)
	head(CSCO)
	#Merge:
	AAPL$Date = as.Date(AAPL$Date, format="%m/%d/%Y")
	IBM$Date = as.Date(IBM$Date, format="%m/%d/%Y")
	CSCO$Date = as.Date(CSCO$Date, format="%m/%d/%Y")
	AAPL = AAPL[,c("Date","Close_Percent_Change")]
	colnames(AAPL) = c("Date","AAPL_DRET")
	IBM = IBM[,c("Date","Close_Percent_Change")]
	colnames(IBM) = c("Date","IBM_DRET")
	CSCO = CSCO[,c("Date","Close_Percent_Change")]
	colnames(CSCO) = c("Date","CSCO_DRET")
	TEMP = merge(AAPL,IBM,by="Date") 
	DESCRIPT = merge(TEMP,CSCO, by="Date")
	DESCRIPT = na.omit(DESCRIPT)
	head(DESCRIPT)
#ii: Descriptive:
	require("doBy")
	require("Hmisc")
	describe(DESCRIPT)
	summary(DESCRIPT)	
#iii: boxplots:
	par(mfrow=c(nr=1, nc=3))
	boxplot(DESCRIPT$AAPL[!is.na(DESCRIPT$AAPL)],
	col="green", ylab="Daily Apple Return", horizontal=FALSE) 
	boxplot(DESCRIPT$IBM[!is.na(DESCRIPT$IBM)],
	col="blue", ylab="Daily IBM Return", horizontal=FALSE) 
	boxplot(DESCRIPT$CSCO[!is.na(DESCRIPT$CSCO)],
	col="yellow", ylab="Daily Cisco Return", horizontal=FALSE)
	mtext(text="Boxplots of Apple, IBM, and Cisco Daily Returns", 
	side=3, outer=TRUE, line=-1)
#iv: Q-Q plots:
	par(mfrow=c(nr=1, nc=3))
	qqnorm(DESCRIPT$AAPL[!is.na(DESCRIPT$AAPL)],
	main="Daily Apple Ret. vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="Daily Apple Ret. Quantiles", col="red")
	qqline(DESCRIPT$AAPL[!is.na(DESCRIPT$AAPL)],
	col="blue")
	qqnorm(DESCRIPT$CSCO[!is.na(DESCRIPT$CSCO)],
	main="Daily Cisco Ret. vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="Daily Cisco Ret. Quantiles", col="red")
	qqline(DESCRIPT$CSCO[!is.na(DESCRIPT$CSCO)],
	col="blue")
	qqnorm(DESCRIPT$IBM[!is.na(DESCRIPT$IBM)],
	main="Daily IBM Ret. vs. Normal Q-Q plot",
	xlab="Theoretical standard normal quantiles",
	ylab="Daily IBM Ret. Quantiles", col="red")
	qqline(DESCRIPT$IBM[!is.na(DESCRIPT$IBM)],
	col="blue")
#v: Empirical p.d.f and c.d.f:
	par(mfrow=c(nr=1, nc=2))
	plot(density(DESCRIPT$AAPL),main="Apple p.d.f",col="blue")
	plot(ecdf(DESCRIPT$AAPL),main="Apple e.c.d.f",col="red")
	skewness(DESCRIPT$AAPL)
	kurtosis(DESCRIPT$AAPL)
	
	par(mfrow=c(nr=1, nc=2))
	plot(density(DESCRIPT$CSCO),main="CISCO p.d.f",col="blue")
	plot(ecdf(DESCRIPT$CSCO),main="CISCO e.c.d.f",col="red")
	skewness(DESCRIPT$CSCO)
	kurtosis(DESCRIPT$CSCO)

	par(mfrow=c(nr=1, nc=2))
	plot(density(DESCRIPT$IBM),main="IBM p.d.f",col="blue")
	plot(ecdf(DESCRIPT$IBM),main="IBM e.c.d.f",col="red")
	skewness(DESCRIPT$IBM)
	kurtosis(DESCRIPT$IBM)






