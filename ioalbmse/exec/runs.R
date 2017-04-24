# runs.R - DESC
# /runs.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

data(oms)
data(indicators)


# LONG TERM projection at F_bar=0

ltom <- fwdWindow(om, orp, end=2116)
ltom <- fwd(ltom, sr=osr, f=FLQuant(0, dimnames=list(year=2015:2116)))

plot(ltom) + geom_vline(aes(xintercept=as.numeric(ISOdate(2015,1,1))), colour="gray", linetype=2)


# RUNS

years <- seq(2014, 2064, by=2)

omp <- fwdWindow(om, orp, end=2066)

index <- window(propagate(cpue$index,100), end=2066)

# SINGLE iter
omp <- omp[,,,,,1]
osr$params <- osr$params[,1]
index <- index[,,,,,1]

# No ERROR
R0 <- mseIndex(omp=omp, sr=osr, cpue=index,
  hcrparams=FLPar(lambda=0.85, ny=5, dtac=0.15),
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA, verbose=TRUE)

R0 <- mseIndex(omp=omp, sr=osr, cpue=index,
  hcrparams=FLPar(lambda=1.25, ny=5, dtac=0.15),
  years=years, oemparams=FLPar(b=0.71, sd=0.2), imparams=NA, verbose=TRUE)

plot(R0$omp) + geom_vline(aes(xintercept=as.numeric(ISOdate(2014,1,1))))

plot(FLQuants(cpue=R0$cpue, stock=stock(R0$om)))


plotOMR(om, R0$om, rpts['SBMSY',])



# ROBUSTNESS TEST


# TUNE

tune(mseIndex, grid=list(lambda=c(0.90, 0.80), dtac=c(0.15, 0.20)), indicators, rpts)
