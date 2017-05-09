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

cpue <- propagate(window(ocpue$index, end=2040), 200)
cpuesel <- ocpue$sel.pattern[-1,'2014']

# options

years <- seq(2014, 2034, by=2)


# --- LONG TERM projection at F_bar~0

ltom <- fwd(omp, sr=osr,
  control=fwdControl(quant='f', year=2015:2040, value=0.0001))

# --- msePT

# Robustness trial

R0b <- msePT(omp, osr, cpue=cpue, cpuesel=cpuesel, years=years,
  hcrparams=FLPar(lambda=1, dltac=0.15, dhtac=0.15, Dlimit=0.10, Dtarget=0.40), 
  oemparams=FLPar(b=0.2, sd=0.3))

plot(R0b$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

# Tuning

ptgrid <- list(lambda=seq(0.50, 1.50, length=3),
  dltac=seq(0.05, 0.25, length=2), dhtac=seq(0.05, 0.25, length=2),
  Dlimit=seq(0.10, 0.20, length=2), Dtarget=seq(0.30, 0.50, length=2))

system.time(RG <- doRuns(msePT, grid=ptgrid, omp=omp, sr=osr, dlag=2,
  cpue=cpue, cpuesel=cpuesel, years=years, oemparams=FLPar(b=0, sd=0)))

plotOMR(om, RG)

# P(SB_2038 > SBMSY)
t1 <- lapply(RG, function(x) sum(c(ssb(x)[,ac(2015:2034)]) > c(rpts$SBMSY)) / (200 * 24))

pRG <- performance(RG, indicators, rpts, years=2034, grid=ptgrid)
qRG <- performance(RG, indicators, rpts, years=2034,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=ptgrid)

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
