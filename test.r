
library(quantmod)
spy = getSymbols("^GSPC", from="2012-01-01", to="2014-01-01",src="yahoo", auto.assign=F)
nrow(spy)
a<-spy[,1]
length(a)
tmp_a<-Lag(Vo(spy),c(1))
b<-vo(spy)-tmp_a

a<-Lag(Vo(spy),c(2))
a

df <- data.frame(matrix(ncol = 2, nrow = length(a)))


lineChart(spy)
addMACD()
addBBands() 
