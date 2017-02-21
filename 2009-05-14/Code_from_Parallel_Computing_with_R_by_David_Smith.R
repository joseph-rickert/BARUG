
require("doNWS")

# Numeric Iterator
i <- iter(1:3)
nextElem(i)

# Long sequences
i <- icount(1e9)
nextElem(i)

# Irregular sequences
require(gmp)
iprime <- function() {
 lastPrime <- 1

 nextEl <- function() {
  lastPrime <<- as.numeric(nextprime(lastPrime))
  lastPrime
 }
 it <- list(nextElem=nextEl)
 class(it) <- c('abstractiter','iter')
 it
}

p <- iprime()
nextElem(p)

# Matrix dimensions
M <- matrix(1:25,ncol=5)
r <- iter(M,by="row")
nextElem(r)

# Data File
# write.table(MSFT,"MSFT.csv",sep=",",row.names=F)
rec <- iread.table("MSFT.csv",sep=",", header=T, row.names=NULL)
nextElem(rec)

# Database
m <- dbDriver('SQLite')
con <- dbConnect(m, dbname="arrests")
it <- iquery(con, 'select * from USArrests', n=10)
nextElem(it)

## Birthday function
birthday <- function(n) {
  ntests <- 10000
  pop <- 1:365
  anydup <- function(i) 
      any(duplicated(
		sample(pop, n, replace=TRUE)))
  sum(sapply(seq(ntests), anydup)) / ntests
}

registerDoSEQ()
system.time(
x <- foreach (j=1:100) %dopar% birthday (j)
)
# 43s
plot(unlist(x),type="l")

s <- sleigh(workerCount=2)
registerDoNWS(s)
system.time(
x <- foreach (j=1:100) %dopar% birthday (j)
)
# 28s

## Backtesting

# load INTC, IEF
require('quantmod')
require ('PerformanceAnalytics')
require ('TTR')
load ('Vhayu.Rdata')
chartSeries(INTC)
addMACD(fast=12, slow=26, signal=9)

# compare returns
Ra <- Return.calculate(Cl(INTC))
Rb <- Return.calculate(Cl(IEF))
chart.CumReturns (cbind (Ra,Rb), main='Returns', legend.loc='topright')

# Define a very simple MACD-based example trading rule. 
simpleRule <- function (z, fast=12, slow=26, signal=9, long, benchmark)
{
  x <- MACD(z, nFast=fast, nSlow=slow, nSig=signal, maType="EMA")
  position <- sign(x[,1]-x[,2])
  s <- xts(position,order.by=index(z))
  return (long*(s>0) + benchmark*(s<=0))
}

# How does the rule do for "standard" parameters?
R.def <- simpleRule (Cl(INTC), fast=12, slow=26, signal=9, long=Ra, benchmark=Rb)
chart.CumReturns(R.def, main="nFast=12 nSlow=26 nSig=9")

# calculate Sharpe ratio for these parameters
Dt <- na.omit(R.def - Rb)
sharpe <- mean(Dt)/sd(Dt)
print (paste("Ratio = ",sharpe))

# brute force: 77s
M <- 100
S <- matrix(0,M,M)
system.time(
for (j in 5:(M-1)) {
  for (k in min((j+2),M):M) {
    R <- simpleRule(Cl(INTC),j,k,9, Ra, Rb)
    Dt <- na.omit(R - Rb)
    S[j,k] <- mean(Dt)/sd(Dt)
  }
}
)

# find optimized values of j,k
print (paste("Best ratio is ",max(S)))
which (S==max(S), arr.ind=TRUE)

# 2-core parallel: 
s <- sleigh(workerCount=2)
registerDoNWS(s) # use NetWorkSpaces as the back-end engine
system.time (
SS <- foreach (j=5:(M-1), .combine=rbind, .packages=c('xts','TTR')) %dopar% {
    x <- rep(0,M)
    for (k in min((j+2),M):M) {
      R <- simpleRule(INTC$Close,j,k,9,Ra,Rb)
      Dt <- na.omit(R - Rb)
      x[k] <- mean(Dt)/sd(Dt)
    }
    return(x)
  }
)

stopSleigh(s) # Shutdown parallel workers

# plot optimized returns
j <- which(S==max(S), arr.ind=TRUE)
Ropt <- simpleRule(Cl(INTC),j[1],j[2],9,Ra, Rb)
chart.CumReturns(cbind(Ra,Rb,R.def,Ropt))
