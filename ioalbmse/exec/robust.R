# robust.R - DESC
# /robust.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

library(ioalbmse)

data(oms)

# omp
years <- seq(2014, length=25, by=2)

omp <- fwdWindow(om, end=tail(years, 1) + 2, rp)

r0 <- mseIndex(omp=omp, sr=sr, index=NA,
  hcrparams=FLPar(lambda=1.25, ny=5, dtac=0.15),
  years=years, oemparams=NA, imparams=NA, verbose=TRUE)

plot(r0$omp) + geom_vline(aes(xintercept=as.numeric(ISOdate(years[1],1,1))))


# ---

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

rmet <- metrics(omp, list(catch=catch, ssb=ssb, vb=vb))

srresiduals=exp(FLife::rnoise(1, FLQuant(0, dimnames=list(year=seq(years[1],
  tail(years, 1) + 6))), sd=0.2, b=0.1))

