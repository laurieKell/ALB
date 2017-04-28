# runs.R - DESC
# /runs.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(4)

data(oms)
data(indicators)

# --- SA

# REC RESIDUALS

# --- LONG TERM projection at F_bar=0

ltom <- fwd(fwdWindow(om, orp, end=2116), sr=osr,
  f=FLQuant(0.0001, dimnames=list(year=2015:2116)))

ltom <- fwd(fwdWindow(om, orp, end=2116), sr=osr,
  catch=FLQuant(5000, dimnames=list(year=2015:2116)))

plot(ltom) + geom_vline(aes(xintercept=as.numeric(ISOdate(2015,1,1))), colour="gray", linetype=2)

# --- RUNS

years <- seq(2014, 2034, by=2)
omp <- fwdWindow(om, orp, end=2038)
index <- window(propagate(cpue$index, dims(omp)$iter), end=2038)

# No ERROR

R0 <- mseIndex(omp=omp, sr=list(model='mean', params=FLPar(a=15000, iter=1:200)), cpue=index,
  hcrparams=FLPar(lambda=0.95, ny=5, dltac=0.20, dhtac=0.10),
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA, verbose=TRUE)

R0 <- mseIndex(omp=omp, sr=osr, cpue=index,
  hcrparams=FLPar(lambda=0.95, ny=5, dltac=0.20, dhtac=0.10),
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA, verbose=TRUE)

plot(R0$om) + geom_vline(aes(xintercept=as.numeric(ISOdate(years[1],1,1))))

ggplot(ssb(R0$om), aes(x=date, y=data, colour=iter)) + geom_line() + geom_vline(aes(xintercept=as.numeric(ISOdate(2014,1,1))))

plot(R1$omp) + geom_vline(aes(xintercept=as.numeric(ISOdate(2014,1,1))))

plot(FLQuants(cpue=R0$cpue, stock=stock(R0$om)))

plotOMR(om, FLStocks(R0=R0$om, R1=R1$om), rpts['SBMSY',])

performance(R0$om, indicators, rpts)

performance(FLStocks(R0=R0$om, R1=R1$om), indicators, rpts)


# ROBUSTNESS TEST


# TUNE

# library(doRNG)
# registerDoRNG(1234)

T0 <- tune(mseIndex,
  grid=list(lambda=seq(0.90, 1.10, length=5), dltac=c(0.15, 0.10), dhtac=c(0.15, 0.20), ny=5),
  indicators=indicators, refpts=rpts, omp=omp, sr=osr, cpue=index,
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA)

qT0 <- T0[, as.list(quantile(data, probs=c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=TRUE)),
        keyby=list(mp, run, indicator, name, year)]

# Find tuned params

# P(SB == SB_MSY) = 0.50

# P(kobe == 'green') = 75%
