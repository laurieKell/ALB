# runs.R - DESC
# /runs.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(5)

# --- SETUP

# data
data(indicators)
data(oms)

# CPUE
cpue <- propagate(window(ocpue$index, end=2040), 200)
cpuesel <- ocpue$sel.pattern[-1,'2014']

# SR residuals: sd, b
resd <- c(sqrt(yearVars(ocpue$index.res))*4)
rerho <- c(apply(ocpue$index.res, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2]))
sres <- FLQuant(0, dimnames=dimnames(stock(omp)))
for(i in seq(dim(sres)[6]))
  sres[,,,,,i] <- rlnoise(1, sres[,,,,,i], sd=resd[1], b=0)
osr@.Data[['residuals']] <- sres

# ARGUMENTS
years <- seq(2014, 2034, by=2)


# --- LONG TERM projection at F_bar~0

Rltom <- fwd(omp, sr=osr, residuals=osr$residuals,
  control=fwdControl(quant='f', year=2015:2040, value=0.0001))

# --- CONSTANT catch

Rconc <- fwd(omp, sr=osr, residuals=osr$residuals,
  control=fwdControl(quant='catch', year=2015:2040, value=catch(omp)[,'2014']))

# --- CONSTANT F

Rconf <- fwd(omp, sr=osr, residuals=osr$residuals,
  control=fwdControl(quant='f', year=2015:2040, value=fbar(omp)[,'2014']))

plot(Rltom, Rconc, Rconf)


# --- msePT

# Robustness trial

R0pt <- fwd(omp, sr=osr,
  control=fwdControl(list(year=2014, quant="f", value=fbar(omp)[,'2014'] * 5)))

R0pt <- msePT(R0, osr, cpue=cpue, cpuesel=cpuesel, years=2015:2034,
  hcrparams=FLPar(lambda=1, dltac=0.15, dhtac=0.15, Dlimit=0.10, Dtarget=0.40), 
  oemparams=FLPar(sd=0.3, b=0.2))

plot(R0pt$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))


# Tuning

grid <- list(lambda=seq(0.50, 1.50, length=5),
  dltac=seq(0.05, 0.25, length=2), dhtac=seq(0.05, 0.25, length=2),
  Dlimit=seq(0.10, 0.20, length=2), Dtarget=seq(0.30, 0.50, length=2))

# TEST
grid <- list(lambda=seq(0.50, 1.50, length=5),
  dltac=0.10, dhtac=0.15, Dlimit=c(0.10, 0.20), Dtarget=c(0.40, 0.50))

system.time(RG <- doRuns(msePT, grid=grid, omp=omp, sr=osr, dlag=2,
  cpue=cpue, cpuesel=cpuesel, years=years, oemparams=FLPar(b=0, sd=0)))

plotOMR(om, RG, qname='fbar')
plotOMR(om, RG, qname='catch')

# P(SB_2038 > SBMSY)
t1 <- lapply(RG, function(x) sum(c(ssb(x)[,ac(2015:2034)]) > c(rpts$SBMSY)) / (200 * 24))

pRG <- performance(RG, indicators, rpts, years=2034, grid=grid)
qRG <- performance(RG, indicators, rpts, years=2034,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid)

qRG2 <- quantilePerf(pRG)

qRG[indicator == 'S8', `50%`>0.40 & `50%`<0.60]

# P(SB
qRG[indicator == 'S3', `25%`>1]

save(RG, pRG, qRG, file="msePT_grid.RData", compress="xz")




# --- mseIndex

# ROBUSTNESS TEST


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

# TUNE

T0 <- tune(mseIndex,
  grid=list(lambda=seq(0.90, 1.10, length=5), dltac=c(0.15, 0.10), dhtac=c(0.15, 0.20), ny=5),
  indicators=indicators, refpts=rpts, omp=omp, sr=osr, cpue=index,
  years=years, oemparams=FLPar(b=0, sd=0), imparams=NA)

qT0 <- T0[, as.list(quantile(data, probs=c(0.1, 0.25, 0.50, 0.75, 0.90), na.rm=TRUE)),
        keyby=list(mp, run, indicator, name, year)]

# Find tuned params
# P(SB == SB_MSY) = 0.50
# P(kobe == 'green') = 75%


# FINISH msePT
# CORRECT mseIndex
# ADD mseIndexTg

# DO RUNS

# CLARIFY tuning selection

# OUTPUT plots for all runs and tuned ones

# PREPARE presentation

# THINK about STORAGE of outputs data.table & FLSs
