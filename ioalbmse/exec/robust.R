# robust.R - DESC
# /robust.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# PACKAGES
library(ioalbmse)

# DATA
data(oms)
data(indicators)

years <- seq(2014, length=15, by=2)

# OMP
omp <- fwdWindow(om, end=tail(years, 1) + 2, rp)

# R0
r0 <- mseIndex(omp=omp, sr=sr, index=NA,
  hcrparams=FLPar(lambda=-1.25, ny=5, dtac=0.15),
  years=years, oemparams=NA, imparams=NA, verbose=TRUE)

r1 <- mseIndex(omp=omp, sr=sr, index=NA,
  hcrparams=FLPar(lambda=1.25, ny=5, dtac=0.10),
  years=years, oemparams=NA, imparams=NA, verbose=TRUE)

plot(r0$omp) + geom_vline(aes(xintercept=as.numeric(ISOdate(years[1],1,1))))

plotOMR(om, FLStocks(R0=r0$omp, R1=r1$omp, R2=r0$omp), rpts["SBMSY",])

perf <- performance(r0$omp, indicators=indicators, refpts=rpts, years=c(2024, 2034, 2064))

# BUG run and mp: in performance or plotTOs?
perf[, run:="r0"]
perf[, mp:="index"]

# quantiles
perq <- perf[, as.list(quantile(data, probs=c(0.1, 0.25, 0.50, 0.75, 0.9), na.rm=TRUE)),
  keyby=list(mp, run, indicator, name, year)]

# plots
plotTOs(perq, "S1", "T1", "2064")


# --- TEST w/ple4

data(ple4)
psr <- fmle(as.FLSR(ple4, model='bevholt'))
prp <- brp(FLBRP(ple4, psr))
pom <- fwdWindow(ple4, prp, end=2038)

pte <- fwd(pom, sr=psr, f=FLQuant(0, dimnames=list(year=2006:2036)))

pr0 <- mseIndex(omp=pom, sr=psr, index=NA,
  hcrparams=FLPar(lambda=1.05, ny=5, dtac=0.15),
  years=seq(2006, 2036, by=2), oemparams=NA, imparams=NA, verbose=FALSE)

plot(window(pr0$omp, end=2036))


# ---

srresiduals <- rlnoise(1, FLQuant(0, dimnames=list(year=seq(years[1],
  tail(years, 1) + 6))), sd=0.2, b=0.1)
