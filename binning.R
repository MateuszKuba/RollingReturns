stoxx50 <-read.csv("sp500.csv")[c(1,5)]
stoxx50.xts <- xts(stoxx50[,2],as.Date(stoxx50[,1]))
stoxx50.lagged12 <- diff(stoxx50.xts,lag=12,differences = 1)/lag(stoxx50.xts,12)


stoxx50_bymonth <- list()

for ( i in 1:12 ) {
  
  stoxx50_month <-stoxx50.lagged12[month(stoxx50.lagged12) == i]
  stoxx50_bymonth[[i]] <- stoxx50_month[!is.na(index(stoxx50_month))]
  
}

#lapply(stoxx50_bymonth, mean,na.rm=TRUE)

df <- melt(stoxx50_bymonth)
qplot(factor(L1), value, data = df, geom = "boxplot")
