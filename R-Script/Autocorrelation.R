install.packages('quantmod')
install.packages("xts")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("seastests")
install.packages("ggplot")
install.packages("TTR")
install.packages("car")
install.packages("AER")
install.packages("dynlm")
install.packages("tseries")


library(quantmod)
library(xts)
library(ggplot2)
library(gridExtra)
library(seastests)
library(TTR)
library(stats)
library(car)
library(dynlm)
library(AER)
library(tseries)


options(max.print=10)

symbolBasket <- c('SPY', 'HSI')

getSymbols(symbolBasket , src='yahoo')

SPY<-SPY[-(0:2000)]

SPY<-subset(SPY,select=-c(1:5))
SPY_change<-(0)
for(i in 2:nrow(SPY)){
SPY_change<-append(SPY_change,log(SPY[[i,1]]/SPY[[i-1,1]]))
}
SPY$change<-SPY_change
acf(SPY$change, lag=10, pl=TRUE)
acf(SPY$change, plot=FALSE)

durbinWatsonTest(SPY_change)


SPY_change_2<-c()
for (i in 2:nrow(SPY)){
SPY_change_2<-append(SPY_change_2,SPY$change[[i,1]])
}

SPY_change_2<-append(SPY_change_2,0)
SPY$change_2<-SPY_change_2
SPY <- head(SPY, - 1)
  
model <- lm(formula = change ~ change_2, data = SPY)
dwtest(formula = model,  alternative = "two.sided")