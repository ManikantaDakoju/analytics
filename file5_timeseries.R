start <- as.Date("2020-01-01")
end <- as.Date("2020-02-29")
end - start



bday = '2-March-1993'
str(bday)
bday1 <- as.Date(bday, format='%d-%B-%Y')
str(bday1)
weekdays(bday1)
Sys.Date() - bday1
internship = seq.Date(from=Sys.Date(), to = as.Date('2020-04-01'), by=1)
weekdays(internship)
range(internship)

library(quantmod)

quantmod::getSymbols("SBIN.NS", src = "yahoo", from = start, to = end)
head(SBIN.NS)
df = SBIN.NS
head(df)
names(df)
colnames(df)
#create new column names
unlist(strsplit("a.b.c", "\\."))
unlist(strsplit(names(df),"\\."))
(newcolNames <- unlist(strsplit(names(df), "\\."))[seq(3,18,3)])
names(df)=newcolNames
head(df)
str(df)
index(df)#row names
coredata(df)#Column names

#time series
plot(df$Open)
plot(df, legend.loc = 'left')
plot(df, legend.loc = 'left', multi.panel = T)
plot(df[,c('Open','Close')], legend.loc = 'top')
plot(df[,c('Open','Close')], legend.loc = 'top', multi.panel = T)
plot(df[,c('Open','Close')], legend.loc = 'top', subset="2020-01-01/2020-01-15")
plot(df[,1:4], multi.panel = T)
plot(df[,1:4], multi.panel = T, type='h')
plot(df[,1:4], multi.panel = F, legend.loc = 'top')
candleChart(df, up.col = "green", dn.col = "red", theme = "white")

#properties
periodicity(df)
to.weekly(df)
to.monthly(df)
to.quarterly(df)
to.yearly(df)
to.period(df,period="weeks")
nyears(df)
nmonths(df)
ndays(df)
.indexwday(df)  # weekday number
weekdays(index(df))
start(df)
end(df)
time(df)
head(df)
tail(df)

#apply functions
apply.weekly(df, FUN=mean)
apply.monthly(df, FUN=mean)
apply.yearly(df, FUN=mean)
apply.quarterly(df, FUN=mean)

#
periodicity(df)
to.weekly(df)
to.daily(df)
.indexwday(df)
df[.indexwday(df)==5]

#endpoint
endpoints(df, on='weeks')
df[endpoints(df, on='weeks')]
endpoints(df, on='months')
df[endpoints(df, on='months')]

#means
(wep <- endpoints(df, on='weeks'))
period.apply(df,INDEX=wep, FUN=mean)

#split data
(sdata <- split(df, f='months'))
sdata[1]
sdata[3]
sdata[2]
(sdata <- split(df, f='weeks'))
sdata[3]

#subset
df['2020']
df['2020-01']
df['2020-01-03']

#First & Last
first(df, '1 week') # extract first week data
first(last(df, '1 week'),'3 days') #get first 3 days of the last week

#data
df[c('2020-01-16','2020-01-15')]
df[seq(as.Date('2020-01-01'), as.Date('2020-01-31'),2),]
df[seq(start(df), end(df), 3)]

#weekend
.indexwday(df)
df[.indexwday(df)==5]



#TS data
AirPassengers
monthplot(AirPassengers)
class(AirPassengers)
(inputdata= as.vector(AirPassengers))
length(AirPassengers)
ts(inputdata, frequency = 4, start = 1988)

#Monthly.Ts
(inputdata = as.vector(AirPassengers))
(monTS <- ts (inputdata, frequency = 12, start = c(2010,3)))

(monTSlagged <- stats::lag(monTS, k=-1))
monTS
monTSlagged
(monTS - monTSlagged)

diff(monTS, lag=1)
cbind(monTS, monTSlagged, difference=(monTS - monTSlagged), diff(monTS))
head(monTS)


#Moving Average, SMA, ES--------------
library(TTR)
head(monTS)
SMA(monTS,n=3)
EMA(monTS,n=3)
plot(monTS)
lines(SMA(monTS,n=12), col='blue')

#Find Trend, seasonal, Irregular Components
AirPassengers
plot(AirPassengers)
plot(decompose(AirPassengers))
#What are the different trends

#Forecast & Arima
library(forecast)
auto.arima(monTS)
autoplot(monTS)
auto.arima(monTS, seasonal = T)
arimaModel = auto.arima(monTS)
checkresiduals(arimaModel)

#Forecast
fcModel <- forecast(arimaModel)
fcModel$mean
autoplot(fcModel)
