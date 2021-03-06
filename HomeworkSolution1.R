

##Question 1:
##i: Create a Vector:
VEC = c(0.009312,-0.001045,-0.004701,-0.014949,
        -0.002183,0.002501,0.011713,-0.007162)
VEC
##ii: What class is the vector you created in a?
class(VEC)

##iii: Create an additional vector with the following numbers:
DAT = c("2015-03-20","2015-03-23","2015-03-24","2015-03-25",
        "2015-03-26","2015-03-27","2015-03-30","2015-03-31")
DAT
##iv: Convert the vector you created into class "Date"
DATE = as.Date(DAT,"%Y-%m-%d")
DATE
class(DATE)
## More date formats:
format(DATE, format = "%B %d %Y")
format(DATE, format = "%b %d %y")
##v: Combine the two vectors into one data frame, then name the columns "DATE" and "DAILY_RETURN" accordingly:
DF = data.frame(DATE,VEC,stringsAsFactors = FALSE)
colnames(DF) = c("DATE","DAILY_RETURN")
View(DF)
##vi: Calculate the maximum, minimum, mean, median and standard deviation of the daily returns?
max(DF$DAILY_RETURN)
min(DF$DAILY_RETURN)
mean(DF$DAILY_RETURN)
median(DF$DAILY_RETURN)
sd(DF$DAILY_RETURN)
##vii: What day of the week gave the maximum return? (you cannot use a calender to solve this section)
DF$DAY = weekdays(DF$DATE)
View(DF)
DF$DAY[DF$DAILY_RETURN == max(DF$DAILY_RETURN)]
#More day formats:
DAY = format(DF$DATE,"%a")
DAY

##Question 2:
##i: read the data set into R
INDEX = read.csv(file= "IndexesDATA_201503_Daily.csv",
                 na.strings=c("NA",".",""))
VIX = read.csv(file= "VIXDATA_201406_Daily.csv", 
               na.strings=c("NA",".",""))
head(INDEX)
head(VIX)
##ii: dimensions of the data sets:
dim(INDEX)
dim(VIX)
##iii:merge the data sets, to do so we must manipulate the date column
class(INDEX$TrdDATE)
class(VIX$TrdDATE)
#WRONG WAY TO DO IT:
COMBINED = merge(INDEX,VIX,by="TrdDATE")
COMBINED
#Correct way:	
INDEX$TrdDATE = as.Date(as.character(INDEX$TrdDATE), format='%Y%m%d')
VIX$TrdDATE = as.Date(VIX$TrdDATE,format="%d%B%Y")
COMBINED = merge(INDEX,VIX,by="TrdDATE")
head(COMBINED)
##iv: count the number of trading days in 1995:
length(COMBINED$VWRET[as.numeric(format(COMBINED$TrdDATE, 
                                        format = "%Y")) == 1995])
##v: add a column with the day of the week
COMBINED$TrdDAY = weekdays(COMBINED$TrdDATE)
# calculate average:
mean(COMBINED$VWRET[COMBINED$TrdDAY == "Monday"])
##vi: average above average of VIX:
mean(COMBINED$SPVWRET[COMBINED$VIX > mean(COMBINED$VIX)])
#average below below pct:
mean(COMBINED$SPVWRET[COMBINED$VIX < mean(COMBINED$VIX)])

##Question 3:
##i: reduce the data frame:
head(INDEX)
RED = INDEX[,c("TrdDATE","VWRET","SPVWRET","SPINDEX")]
head(RED)
##ii: 
length(RED$VWRET[as.numeric(format(RED$TrdDATE, "%Y")) == 1990 
                 & RED$VWRET > RED$SPVWRET])
##iii:Plot 
plot(RED$TrdDATE,RED$VWRET,main="Student name and ID",
     ylab="Value Weighted Return",xlab="Date",col="green",pch=23)
##iv:
G_VWRET = (1+RED$VWRET[!is.na(RED$VWRET)])
Cum_VWRET = (cumprod(G_VWRET)-1)*100
plot(RED$TrdDATE,Cum_VWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "VW Return",col="blue")

G_SPVWRET = (1+RED$SPVWRET[!is.na(RED$SPVWRET)])
Cum_SPVWRET = (cumprod(G_SPVWRET)-1)*100
plot(RED$TrdDATE,Cum_SPVWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "SPVW Return",col="red")
##v:
#merge
par(mfrow=c(nr=2,nc=1))
plot(RED$TrdDATE,Cum_VWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "VW Return",col="blue")
plot(RED$TrdDATE,Cum_SPVWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "SPVW Return",col="red")
#merge and export
pdf(file="Plots.pdf")
par(mfrow=c(nr=2,nc=1))
plot(RED$TrdDATE,Cum_VWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "VW Return")
plot(RED$TrdDATE,Cum_SPVWRET,main="Studnet name and ID",
     xlab = "Year", ylab = "SPVW Return")
dev.off()
##vi:
TrdYear = as.numeric(format(RED$TrdDATE, format = "%Y"))
TrdMonth = as.numeric(format(RED$TrdDATE, format = "%m"))
Y1 = RED$TrdDATE[TrdYear == 2002 & (TrdMonth>=5 & TrdMonth<=9)]
X1 = RED$SPINDEX[TrdYear == 2002 & (TrdMonth>=5 & TrdMonth<=9)]
plot(Y1,X1, main="The S&P 500 index during May - Sep 2002", 
     ylab ="Daily values of the S&P 500 index", xlab="Trading dates", 
     bg="green",pch=24)
abline(h=max(X1),lty=2,lwd=2,col="black")
abline(h=min(X1),lty=2,lwd=2,col="blue")
#vii:
write.csv(RED,file="REDUCED.csv")

##Question 4 - Optional
#i: read the three data sets into R
setwd("PSetSol_Module1")

GOOGL = read.csv(file= "GOOG_20151021_PS1.csv",
                 na.strings=c("NA",".",""))
YHOO = read.csv(file= "YHOO_20151021_PS1.csv",
                na.strings=c("NA",".",""))
MSFT = read.csv(file= "MSFT_20151021_PS1.csv",
                na.strings=c("NA",".",""))
head(GOOGL)
head(YHOO)
head(MSFT)

#ii:
GOOGL$Date = as.Date(GOOGL$Date, format="%m/%d/%Y")
YHOO$Date = as.Date(YHOO$Date, format="%m/%d/%Y")
MSFT$Date = as.Date(MSFT$Date, format="%m/%d/%Y")
GOOGL = GOOGL[,c("Date","Close_Percent_Change")]
colnames(GOOGL) = c("Date","GOOGL_DRET")
YHOO = YHOO[,c("Date","Close_Percent_Change")]
colnames(YHOO) = c("Date","YHOO_DRET")
MSFT = MSFT[,c("Date","Close_Percent_Change")]
colnames(MSFT) = c("Date","MSFT_DRET")
TEMP = merge(GOOGL,YHOO,by="Date") 
MERGED = merge(TEMP,MSFT, by="Date")
MERGED = na.omit(MERGED)
head(MERGED)

#iii:
#Google:
max(MERGED$GOOGL_DRET)
MERGED$Date[MERGED$GOOGL_DRET == max(MERGED$GOOGL_DRET)]
#Microsoft:
min(MERGED$MSFT_DRET)
MERGED$Date[MERGED$MSFT_DRET == min(MERGED$MSFT_DRET)]
#iv: 
Year = as.numeric(format(MERGED$Date, format = "%Y"))
GOOG = MERGED$GOOGL_DRET[Year > 2004 & Year < 2010]
GOOG_RET = (1+GOOG[!is.na(GOOG)])
GOOG_RET = (cumprod(GOOG_RET)-1)*100
#Google Return in %:
tail(GOOG_RET,n=1)
#Would have:
tail(GOOG_RET,n=1) + 100	

YAHOO = MERGED$YHOO_DRET[Year > 2004 & Year < 2010]
YAHOO_RET = (1+YAHOO[!is.na(YAHOO)])
YAHOO_RET = (cumprod(YAHOO_RET)-1)*100
#Yahoo Return in %:
tail(YAHOO_RET,n=1)
#Would have:
tail(YAHOO_RET,n=1) + 100

MICRO = MERGED$MSFT_DRET[Year > 2004 & Year < 2010]
MICRO_RET = (1+MICRO[!is.na(MICRO)])
MICRO_RET = (cumprod(MICRO_RET)-1)*100
#Microsoft Return in %:
tail(MICRO_RET,n=1)
#Would have:
tail(MICRO_RET,n=1) + 100

