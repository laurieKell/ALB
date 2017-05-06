# runs.R - DESC
# /runs.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(6)

# --- SETUP

# data

data(indicators)
data(oms)

# --- SA

data(sa)

out <- r4ss::SS_output("/home/imosqueira/Dropbox/Backup/Desktop/IOTC_ALB_MSE/om/sa/run",
  verbose=FALSE, hidewarn=TRUE, warn=FALSE,
  printstats=FALSE, covar=FALSE, forecast=FALSE)

# GET biasadj
bias <- out$recruit[, c("year", "biasadj")]
names(bias)[2] <- "data"
bias <- as(bias, "FLQuant")
bias[is.na(bias)] <- 0

# SR model
sasr <- predictModel(bias=bias,
  model= rec ~ (4 * s * R0 * ssb)/(v * (1 - s) + ssb * (5 * s - 1)) * exp(-0.5 * bias * sd ^ 2),
  params=FLPar(R0=exp(9.82), s=0.8, v=146374, sd=0.3192))

sasr <- predictModel(bias=bias,
  model= rec ~ (4 * s * R0 * ssb)/(v * (1 - s) + ssb * (5 * s - 1)),
  params=FLPar(R0=exp(9.82), s=0.8, v=146374, sd=0.3192))

# COMPARE predictModel vs. out$recruit

plot(FLQuants(
  EXPRECR=predict(sasr, ssb=ssb(sa)[,,'F',4]) / out$recruit$exp_recr,
  ADJUSTED=predict(sasr, ssb=ssb(sa)[,,'F',4]) / out$recruit$adjusted,
  PRED_RECR=predict(sasr, ssb=ssb(sa)[,,'F',4]) / out$recruit$pred_recr))

tes <- fwd(sa, sr=sasr,
  control=fwdControl(quant='f', year=2010:2014, value=45000))

plot(tes)

plot(
log(
  unitSums(rec(sa)[,,,4])
    /
  unitSums(predict(sasr, ssb=ssb(sa)[,,,4]) / 2)
)
)

 mean(log(
   unitSums(rec(sa)[,,,4])     /
   unitSums(predict(sasr, ssb=ssb(sa)[,,,4]) / 2)
 )
 [,ac(1980:2014)], na.rm=TRUE)


 
ggplot(out$recruit[out$recruit$year > 1978,], aes(x=year, y=dev)) + geom_line()


# COMPARE rec

rec(sa)[,,,4]

ssb(sa)[,,'F',4] / out$recruit$spawn_bio

unitSums(rec(sa)[,,,4]) / out$recruit$pred_recr
unitSums(rec(sa)[,,,4]) / out$recruit$adjusted
unitSums(rec(sa)[,,,4]) / out$recruit$exp_recr


# Bias-adjustment in bevholtss3

# REC RESIDUALS
rdsr <- log(predict(osr, ssb=ssb(om) / 2) / (ssb(om) / 2))
plot(FLQuants(RESID=rdsr, REC=rec(om), PRED=predict(osr, ssb=ssb(om)/2)))


# --- LONG TERM projection at F_bar=0

ltom1 <- fwd(omp, sr=osr,
  control=fwdControl(quant='f', year=2015:2040, value=0.0001))

ltom1b <- fwd(omp, sr=osr,
  f=FLQuant(0.01, dimnames=list(year=2015:2040)))

ltom2 <- fwd(omp, sr=osr,
  control=fwdControl(quant='catch', year=2015:2040, value=25000))

ltom2b <- fwd(omp, sr=osr,
  catch=FLQuant(25000, dimnames=list(year=2015:2040)))

plot(FLStock(F=ltom1, C=ltom2)) + geom_vline(aes(xintercept=as.numeric(ISOdate(2015,1,1))), colour="gray", linetype=2)

# --- RUNS

years <- seq(2014, 2034, by=2)
index <- window(propagate(cpue$index, dims(omp)$iter), end=2040)

# NO ERROR

R0 <- mseIndex(omp=omp, sr=osr, cpue=index,
  hcrparams=FLPar(lambda=0.95, ny=5, dltac=0.20, dhtac=0.10),
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA, verbose=TRUE)

plot(R0$om) + geom_vline(aes(xintercept=as.numeric(ISOdate(years[1],1,1))))

ggplot(ssb(R0$om), aes(x=date, y=data, colour=iter)) + geom_line() + geom_vline(aes(xintercept=as.numeric(ISOdate(2014,1,1)))) + theme(legend.position='none')

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

# msePT

R0b <- msePT(omp, osr,
  hcrparams=FLPar(lambda=1, dltac=0.15, dhtac=0.15, Dlimit=0.10, Dtarget=0.40), 
  cpue=propagate(window(cpue$index, end=2038), 200),
  years=seq(2014, 2038, by=2), oemparams=FLPar(b=0, sd=0))

R1b <- msePT(omp, osr,
  hcrparams=FLPar(lambda=0.3, dltac=0.15, dhtac=0.15, Dlimit=0.20, Dtarget=0.30), 
  cpue=propagate(window(cpue$index, end=2038), 200),
  years=seq(2014, 2038, by=2), oemparams=FLPar(b=0, sd=0.3))


plot(R0b$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))
plot(R1b$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

ptgrid <- list(lambda=seq(0.50, 1.50, length=20),
  dltac=seq(0.05, 0.25, length=5), dhtac=seq(0.05, 0.25, length=5),
  Dlimit=seq(0.10, 0.20, length=5), Dtarget=seq(0.30, 0.50, length=5))

ptgrid <- lapply(ptgrid, '[', 1)

system.time(RG <- doRuns(msePT, grid=ptgrid, omp=omp, sr=osr,
  cpue=propagate(window(cpue$index, end=2038), 200),
  years=seq(2014, 2038, by=2), oemparams=FLPar(b=0, sd=0)))


# P(SB_2038 > SBMSY)
lapply(RG, function(x) sum(c(ssb(x)[,ac(2015:2038)]) > c(rpts$SBMSY)) / (200 * 24))


pRG <- performance(RG, indicators, rpts, years=2024)

pRG[indicator == 'S8']

plotOMR(om, RG)


