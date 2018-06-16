##########################################################
#                                                       #
#                                                       #
#                                                       #
#Final code for thesis forecasting GDP for Canada       #
#
#
#########################################################


## load packages
#install.packages("quantmod")
suppressMessages(library("qauntmod"))
#install.packages("midasr")
suppressMessages(library("midasr"))
#install.packages("MCMCpack")
suppressMessages(library("MCMCpack"))
#install.packages("BoomSpikeSlab")
suppressMessages(library("BoomSpikeSlab"))
#install.packages("bsts")
#install.packages("Boom")
suppressMessages(library("Boom"))
suppressMessages(library("bsts"))
suppressMessages(library("tseries"))
suppressMessages(library("zoo"))
suppressMessages(library("dplyr"))
#install.packages("caret")
#install.packages("TTR")
suppressMessages(library("TTR"))
suppressMessages(library("caret"))
library("quantmod", lib.loc="~/R/win-library/3.1")


######################################################



## Data


### GDP

### Load Data from St. Louis Fed. website.



####Response variable: Canadian GDP quarterly

# http://www5.statcan.gc.ca/cansim/a26?lang=eng&id=3800064#customizeTab
# Table 380-0064
# Gross domestic product, expenditure-based
# quarterly (dollars x 1,000,000)

setwd("U:\\Users\\jonduan\\Dropbox\\phd\\writing\\finalcode")

gdp <- read.csv("cansim-3800064-eng-8915745936579071146.csv", skip = 6, stringsAsFactors = F)
gdp <- head(gdp, -3)
names(gdp) <- c("Date","gdp")
head(gdp)
tail(gdp)
gdp.p <- ts(gdp$gdp, start = c(1981,1), frequency = 4 )
gdp.growth.p <- (gdp$gdp-lag(gdp$gdp))/lag(gdp$gdp)*100
tail(gdp.growth.p)
head(gdp.growth.p)




gdp.growth <- getSymbols("NAEXKP01CAQ657S",
                         src = "FRED", auto.assign = FALSE)
#
#setwd("U:\\Users\\jonduan\\Dropbox\\phd\\writing\\figure")
pdf(paste("gdp-growth","-report.pdf",sep=""))
chartSeries(gdp.growth ,theme="white", name = "GDP Growth for Canada")
graphics.off()






adf.test(gdp.growth, alternative = "stationary")
kpss.test(gdp.growth)



# tail(gdp.growth.p)
# head(gdp.growth.p)
# head(gdp)
# gdp.growth
# gdp.growth["1980-01-01/1981-01-01"]
names(gdp.growth) <- "gdp.growth"


gdp.growth.p <- c( rep(NA, 4)  , gdp.growth.p)
gdp.growth.p[1:5] <- gdp.growth["1980-01-01/1981-01-01"]$gdp.growth

gdp.growth.p <- ts(gdp.growth.p, start = c(1980,1), frequency = 4 )



gdp.growth.f <- gdp.growth["1980-01-01/2014-10-01"]
gdp.p <- ts(gdp$gdp, start = c(1981,1), frequency = 4 )
gdp.growth.c <- merge(gdp.growth.f ,gdp.growth.p, gdp.p)
gdp.df <- Lag(gdp.growth.c$gdp.growth.p, k = 1:7)
ngdp <- 7
# tail(gdp.df)
# tail(gdp.df)

names(gdp.df) <- paste(sprintf("%s", "gdp"), sprintf("%s", 1:ngdp),  sprintf("%s", "q"), sep=".")



#### Predictor 1 x: Canadian Unemployment rate monthly (seasonally adjusted)



labor <- getSymbols("LRUNTTTTCAM156S",
src = "FRED",  auto.assign = FALSE)
chartSeries(labor ,theme="white", name = "Unemployment Rate for Canada")


labor <- c(labor, xts(6.80, as.Date("2015-04-01")), 
           xts(6.80, as.Date("2015-05-01")),
           xts(6.80, as.Date("2015-06-01")), 
           xts(6.80, as.Date("2015-07-01")))
                            





library("forecast")




decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl$time.series[,2]
    season <- x.stl$time.series[,1]
    remainder <- x - trend - season
  }
  else #Nonseasonal data
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,
              transform=transform,lambda=lambda))
}




labor.decom<- decomp(labor)
plot(labor.decom$remainder)
labor.d <- labor.decom$remainder
plot(labor.d, main = "Detrend and deseasonalized unemployement rate")



adf.test(labor.d, alternative = "stationary")
kpss.test(labor.d)




# scale the data , standardize.
labor.n <- scale(labor.d)
sum(is.na(labor.n))
names(labor.n) <- "labor"
plot(labor.n, main = "Standardized unemployement rate")



nlabor <- 23
labor.df <- Lag(labor.n, 0:nlabor)
tail(labor.df)
pickmonth <- function(df, a){ tail(head(df, a), 1)}
labor.q.t <- aggregate(labor.df, as.yearqtr, pickmonth, 1)

tail(labor.q.t)[,1:4]

labor.q.t.plus <- apply(labor.q.t, 2, lead, na.rm=TRUE)
tail(labor.q.t.plus, )[,1:3]
labor.q.t.plus <- zoo(x = labor.q.t.plus, order.by = index(labor.q.t), frequency = 4)
tail(labor.q.t.plus, )[,1:3]
names(labor.q.t.plus) <- paste(sprintf("%s","labor"), sprintf("%s", 0:nlabor),   sprintf("%s", "q"), sep=".")




#### Predictor 2 z: Toronto Stock Exchange (TSX) composite index. Daily.





tsx <- getSymbols("^GSPTSE",src="yahoo",
from = '1970-01-01', auto.assign = FALSE)
chartSeries(tsx ,theme="white", name = "S&P/TSX Composite index")




pdf(paste("tsx","-report.pdf",sep=""))
chartSeries(tsx ,theme="white", name = "S&P/TSX Composite index")
graphics.off()




tsx.diff <- diff(log(tsx$GSPTSE.Adjusted)) * 100
chartSeries(tsx.diff ,theme="white", name = "Log return S&P/TSX Composite index")




# detrend and deseasonalized
tsx.decom <- decomp(tsx.diff)
tsx.d <- tsx.decom$remainder
chartSeries(tsx.d ,theme="white", name = "Detrended Log return S&P/TSX Composite index")
# standardize
tsx.n <- scale(tsx.d)
chartSeries(tsx.n ,theme="white", name = "Standardized Detrended Log return S&P/TSX Composite index")
sum(is.na(tsx.n))
adf.test(tsx.n[-1], alternative = "stationary")
kpss.test(tsx.n)
names(tsx.n) <- "tsx"





ntsx <- 22*12  #264  for one year ahead and extra 4 months and 6 trade day
tsx.df <- lag(tsx.n, 0:ntsx)
names(tsx.df)
# tail(tsx.df, 25)[,1:6]
pickday <- function(df, a){ tail(head(df, a), 1)}


leading <- nrow(tsx.df["2015-07-01/2015-08-31"])


#tsx.q.42 <- aggregate(tsx.df, as.yearqtr, pickday, 42)



tsx.q.raw <- aggregate(tsx.df, as.yearqtr, pickday, leading)
#
# tail(tsx.q.raw, )[,1]
# head(tsx.q.raw, )[,1]
# str(tsx.q.raw)

# shift to back one quarter
tsx.q.raw.plus <- apply(tsx.q.raw, 2, lead, na.rm=TRUE)

# tail(tsx.q.raw.plus)[,1:4]
# get the time order as a zoo object
tsx.q.raw.plus <- zoo(x = tsx.q.raw.plus, order.by = index(tsx.q.raw), frequency = 4)


names(tsx.q.raw.plus) <- paste(sprintf("%s","tsx"), sprintf("%s", 0:ntsx),   sprintf("%s", "q"), sep=".")


#########################################################################
#### Crude Oil Price: West Texas

oil <- getSymbols("DCOILWTICO",src="FRED",
from = '1986-01-02', auto.assign = FALSE)
chartSeries(oil ,theme="white", name = "Crude Oil Prices: West Texas Intermediate (WTI)")



names(oil) <- "oil"

head(as.data.frame(oil))
sum(is.na(oil))
pdf(paste("oil","-report.pdf",sep=""))
chartSeries(oil ,theme="white", name = "Crude Oil Prices: West Texas Intermediate (WTI)")
graphics.off()
chartSeries(oil ,theme="white", name = "Crude Oil Prices: West Texas Intermediate (WTI)")


sum(is.na(na.locf(oil)))
oil <- na.locf(oil)


oil.diff <- diff(log(oil$oil)) * 100 


oil.leading <- length(oil.diff["2015-07-01/2015-08-31"])
oil.leading
chartSeries(oil.diff ,theme="white", name = "Log return Crude Oil Prices: West Texas Intermediate (WTI)")
# detrend and deseasonalized
oil.decom <- decomp(oil.diff)
oil.d <- oil.decom$remainder
chartSeries(oil.d ,theme="white", name = "Detrended Log return Crude Oil Prices: West Texas Intermediate (WTI)")
oil.n <- scale(oil.d)
chartSeries(oil.n ,theme="white", name = "Standardized Detrended Log return Crude Oil Prices: West Texas Intermediate (WTI)")
sum(is.na(oil.n))
adf.test(oil.n[-1], alternative = "stationary")
kpss.test(oil.n)
names(oil.n) <- "oil"
head(oil.n)
noil <- 22*12  #264  for one year ahead and extra 4 months and 6 trade day
# oil.df <- lag(oil.n[-1], 0:noil)
# 0:noil
head(oil.n)
oil.df <- Lag(oil.n[-1], 0:noil)
names(oil.df)
pickday <- function(df, a){ tail(head(df, a), 1)}
oil.q.t <- aggregate(oil.df, as.yearqtr, pickday, oil.leading)
head(oil.q.t, )[,1]
tail(oil.q.t, )[,1]
tail(oil.df, 25)[,1:6]
oil.q.t.plus <- apply(oil.q.t, 2, lead, na.rm=TRUE)
tail(oil.q.t.plus, )[,1]
oil.q.t.plus <- zoo(x = oil.q.t.plus, order.by = index(oil.q.t), frequency = 4)
tail(oil.q.t.plus, )[,1]
names(oil.q.t.plus) <- paste(sprintf("%s","oil"), sprintf("%s", 0:noil),   sprintf("%s", "q"), sep=".")




#### yield curve
i10 <- getSymbols("IRLTLT01CAM156N",
src = "FRED",  auto.assign = FALSE)
chartSeries(i10,theme="white", name = "Long-Term Government Bond Yields: 10-year")
ib <- getSymbols("INTGSTCAM193N",
src = "FRED",  auto.assign = FALSE)
chartSeries(ib,theme="white", name = "Interest Rates, Government Securities, Treasury Bills for Canada")
spread <- i10-ib
tail(spread, 100)
names(spread) <- "spread"
chartSeries(spread,theme="white", name = "spread Interest Rates, Government Securities, Treasury Bills for Canada")
pdf(paste("spread","-report.pdf",sep=""))
chartSeries(spread ,theme="white", name = "Spread for Canada 10 year - 3 month")
graphics.off()
sum(is.na(spread))
# detrend and deseasonalized
spread.decom <- decomp(spread)
spread.d <- spread.decom$remainder
# scaled spread
spread.n <- scale(spread.d)
chartSeries(spread.n["2007/2011"] ,theme="white", name = "Standardized Detrended Spread for Canada")
chartSeries(spread.n["2004/2015"] ,theme="white", name = "Standardized Detrended Spread for Canada")
sum(is.na(spread.n))
adf.test(spread.n[-1], alternative = "stationary")
kpss.test(spread.n)
names(spread.n) <- "spread"
tail(spread.n, 4)
spread.n <- c(spread.n, xts(0, as.Date("2015-04-01")))
nspread <- 23
spread.df <- Lag(spread.n, 0:nspread)
names(spread.df)

pickmonth <- function(df, a){ tail(head(df, a), 1)}
tail(spread.df, 4)[,1:4]
spread.q.t <- aggregate(spread.df, as.yearqtr, pickmonth, 3)
names(spread.q.t) <- paste(sprintf("%s","spread"), sprintf("%s", 0:nspread),   sprintf("%s", "q"), sep=".")
tail(spread.q.t)[,1:4]


spread.q.t.plus <- apply(spread.q.t, 2, lag, na.rm=TRUE)
?lead
nrow(spread.q.t.plus)
tail(spread.q.t.plus, )[,1:4]

# index(spread.q.t) + 0.25

spread.q.t.plus <- zoo(x = spread.q.t.plus, order.by = index(spread.q.t) , frequency = 4)
tail(spread.q.t.plus, )[,1:4]




#names(spread.q.t.plus) <- paste(sprintf("%s","spread"), sprintf("%s", 0:nspread),   sprintf("%s", "q"), sep=".")




df.t.q <-merge( gdp.growth.c, gdp.df, labor.q.t.plus, spread.q.t.plus,  tsx.q.raw.plus,oil.q.t.plus )
write.zoo(df.t.q, file = "df_t_q.csv",
append = FALSE, quote = TRUE, sep = ",",
eol = "\n", na = "NA", dec = ".", row.names = F,
col.names = TRUE, qmethod = c("escape", "double"),
fileEncoding = "")
tail(df.t.q)





dat <- read.csv("df_t_q.csv")
names(dat)[c(60:83)]
names(dat)[c(1:ncol(dat))]
dat.z.3 <-  read.zoo(dat, format = "%Y-%m-%d", colClasses = c("NULL", "numeric"))
dat.train.3 <- window(dat.z.3, start = "1986-07-01", end = "2015-04-01" )
head(dat.train.3)
dat.train.3 <- window(dat.z.3, start = "1986-10-01", end = "2015-04-01" )
head(dat.train.3)
names(dat.train.3)
y <- dat.train.3[,2]
gdp <- dat.train.3[,3]
y
x.3 <- dat.train.3[,11:ncol(dat.train.3)]
ssgl <- AddGeneralizedLocalLinearTrend(list(), y)
ssglar4 <-  AddAr(ssgl, y, lags = 4 )
y4 <- y
length(y4)
y4[length(y4)]
y4[length(y4)] <- NA
y4
model31 <- bsts(y~.,
state.specification=ssglar4,
data=x.3,
niter=10000,expected.model.size= 4,
bma.method = "SSVS",
seed=123)
model4 <- bsts(y4~.,
state.specification=ssglar4,
data=x.3,
niter=10000,expected.model.size= 4,
bma.method = "SSVS",
seed=123)


model5 <- bsts(y[-length(y)]~.,
state.specification=ssglar4,
data=x.3[-nrow(x.3),],
niter=10000,expected.model.size= 4,
bma.method = "SSVS",
seed=123)
pred <- predict(model5, horizon = 1, burn = 2000, newdata = x.3[nrow(x.3),])
pred
plot(pred)
(pred)$median
str(pred)
pred$median
save(model5, file = "U:\\Users\\jonduan\\Dropbox\\phd\\data\\model5.RData")
save(model4, file = "U:\\Users\\jonduan\\Dropbox\\phd\\data\\model4.RData")

plot(model3)
plot(model4)
plot(model5)





get.gdp <- function(model = model, gdp = gdp, burn = burn, h = 48, year = 2003, quarter = 2 ){
  errors<- bsts.prediction.errors(model, burn = burn)
  forecast <- t(as.numeric(model$original.series) - t(errors))
  onestep.for.bsts <- colMeans(forecast[-c(1:burn),])[(length(gdp)-(h-1)):length(gdp)]
  onestep.for.bsts <- ts( onestep.for.bsts, start = c(year,quarter), frequency =4 )
  onestep.for.bsts.gdp <-  gdp[(length(gdp)-h):(length(gdp)-1)]*(1+ (onestep.for.bsts)/100)
  onestep.for.bsts.gdp <- ts( onestep.for.bsts.gdp, start = c(year,quarter), frequency =4 )
  return(onestep.for.bsts.gdp)
}

h = 48
year= 2003
quarter = 2
onestep.for.arima.gdp <-  gdp[(length(gdp)-h):(length(gdp)-1)]*(1+ (onestep.for.arima)/100)
onestep.for.arima.gdp <- ts(onestep.for.arima.gdp, start = c(year,quarter), frequency =4 )

onestep.for.boost.gdp <-  gdp[(length(gdp)-h):(length(gdp)-1)]*(1+ (onestep.for.boost)/100)
onestep.for.boost.gdp <- ts( onestep.for.boost.gdp, start = c(year,quarter), frequency =4 )



get.gdp.rate <- function(model = model, y = y, burn = burn, h = 48, year = 2003, quarter = 2 ){
  errors<- bsts.prediction.errors(model, burn = burn)
  forecast <- t(as.numeric(model$original.series) - t(errors))
  onestep.for.bsts <- colMeans(forecast[-c(1:burn),])[(length(gdp)-(h-1)):length(gdp)]
  onestep.for.bsts <- ts( onestep.for.bsts, start = c(year,quarter), frequency =4 )
  return(onestep.for.bsts)
}



gdpPreLine <- function(time = dat.train[,1][92:139], origin = y1[92:139], forecast1 = colMeans(forecast_reg4)[92:139], forecast2 =  fitted(new.arima.fit)[92:139], forecast3 = onestep.for.boost, title = "GDP Growth, BSTS and ARIMA Forecast, 2003-2014", ylab = "Scaled GPD growth", legend = "bottomleft")
{suppressMessages(library(RColorBrewer))      
 #colors <- rainbow(3)
 colors <- brewer.pal(4,"Set1")
 linetype <- c(1:4) 
 #plotchar <- seq(18,18+3,1)
 plotchar <- c(15, 16,17,18)
 plot(time ,origin, type= "b",  lwd=2.5,
      lty=linetype[1], col=colors[1], pch=plotchar[1],
      xlab = "time",ylab = ylab)
 
 #add a title and subtitle
 title(title)
 
 lines(time,forecast1,  
       type= "b",  lwd=2.5,
       lty=linetype[2], col=colors[2], pch=plotchar[2])
 
 lines(time,forecast2, 
       type= "b",  lwd=2.5,
       lty=linetype[3], col=colors[3], pch=plotchar[3])
 
 lines(time,forecast3, 
       type= "b",  lwd=2.5,
       lty=linetype[4], col=colors[4], pch=plotchar[4])
 
 
 
 # add a legend
 legend(legend, leg = c("True","BSTS", "ARIMA", "Boosting" ),
        1:4, cex=1.0, col=colors,
        pch=plotchar, lty=linetype, bty = "n")
 
}



pdf(paste(Figure.dir, "bsts_arima_boost.pdf",sep=""),  width = 14, height = 6)
gdpPreLine(time = index(y[67:114]), origin = y[67:114], forecast1 = onestep.for.bsts, forecast2 =  onestep.for.arima, forecast3 = onestep.for.boost,  ylab = "GDP growth(percent)", legend = "bottomleft", title = "GDP Growth, BSTS , ARIMA and Boosting Forecast, 2003-2014")
graphics.off()

# quarterly (dollars x 1,000,000)
pdf(paste(Figure.dir, "bsts_arima_boost_gdp.pdf",sep=""),  width = 14, height = 6)
gdpPreLine(time = index(gdp[67:114]), origin = gdp[67:114], forecast1 = onestep.for.bsts.gdp, forecast2 = onestep.for.arima.gdp, forecast3 = onestep.for.boost.gdp,  ylab = "GDP quarterly (dollars x 1,000,000)", legend = "topleft", title = "GDP Level, BSTS , ARIMA and Boosting Forecast, 2003-2014")
graphics.off()


pdf(paste(Figure.dir, "spike.pdf",sep=""),  width = 8, height = 8)
s <- seq(-1,1,0.01)
cols <- brewer.pal(3,"Set2")
plot(s, dnorm(s,0, 0.01), type="l", col = cols[1] , ylab = "Density", lwd=2)
lines(s, dnorm(s,0, 0.5), lty= 2 , col=cols[2], lwd=2  )
legend(0.6,35,legend=c("Spike","Slab"),lty=1:2,col=cols, lwd=2)
graphics.off()



pdf(paste(Figure.dir, "bsts_arima_boost_1.pdf",sep=""), width = 14, height = 6)
gdpPreLine(time = index(yTest[1:16]), origin = yTest[1:16], forecast1 = onestep.for.bsts[1:16], forecast2 =onestep.for.arima[1:16], forecast3 = onestep.for.boost[1:16], ylab = "GDP growth(percent)", legend = "topright")
graphics.off()




gdpPreLine(time = index(yTest[17:32]), origin = yTest[17:32], forecast1 = onestep.for.bsts[17:32], forecast2 =onestep.for.arima[17:32], forecast3 =onestep.for.boost[17:32], ylab = "GDP growth(percent)", legend = "bottomright")
pdf(paste(Figure.dir, "bsts_arima_boost_2.pdf",sep=""),  width = 14, height = 6)
gdpPreLine(time = index(yTest[17:32]), origin = yTest[17:32], forecast1 = onestep.for.bsts[17:32], forecast2 =onestep.for.arima[17:32], forecast3 =onestep.for.boost[17:32], ylab = "GDP growth(percent)", legend = "bottomright")
graphics.off()
gdpPreLine(time = index(yTest[33:48]), origin = yTest[33:48], forecast1 = onestep.for.bsts[33:48], forecast2 =onestep.for.arima[33:48], forecast3 =onestep.for.boost[33:48], ylab = "GDP growth(percent)", legend = "topright")
pdf(paste(Figure.dir, "bsts_arima_boost_3.pdf",sep=""),  width = 14, height = 6)
gdpPreLine(time = index(yTest[33:48]), origin = yTest[33:48], forecast1 = onestep.for.bsts[33:48], forecast2 =onestep.for.arima[33:48], forecast3 =onestep.for.boost[33:48], ylab = "GDP growth(percent)", legend = "topright")
graphics.off()

