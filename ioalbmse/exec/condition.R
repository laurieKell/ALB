# condition.R - DESC
# ioalbmse/exec/condition.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(4)

# setwd('/media/mosquia/VB/grid/')

# -- SETUP scenarios {{{
sce <- list(
	# M
	M=c('0202', '0303', '0404', '0403', '0402'),
	# sigmaR
	sigmaR=c(0.4, 0.6),
	# steepness
	steepness=c(0.7, 0.8, 0.9),
	# CPUE CV
	cpuecv=c(0.2, 0.3, 0.4, 0.5),
	# ESS
	ess=c(20, 50, 100),
	# CPUE LL q
	# Yearly multiplier for cpuecv
	llq=c('1.0000', '1.0025'),
	# CPUE LL select
	# Logarithmic or DoubleNormal
	llsel=c("Log", "DoNorm")
) # }}}

# -- SETUP grid/SS3 folders

dir <- 'grid20170117'

grid <- setgrid(sce, dir=dir, base=system.file("sa/2016", package="ioalbmse"),
  name='abt')

save(grid, file=paste0(dir, "/grid.RData")

# -- RUN SS3 grid

rungrid(grid, options="", dir=dir)

# -- LOAD results

load(file=paste0(dir, "/grid.RData")

# res
res <- loadres(dir=dir, grid=grid)

# om
om <- loadom(dir=dir, grid=grid)

# index
ind <- readFLIBss3(paste0(dir, '/', grid$id[1]))

# -- HACK: combine() fails in server {{{

library(FLCore)
load('grid20170117/om.RData')
omf <- propagate(om[[1]], 720)
omf <- slimFLStock(omf)
for(i in 2:720) {
  cat("[", i, "]\n")

  for(s in c("catch", "catch.n", "catch.wt",
             "landings", "landings.n", "landings.wt",
             "stock", "stock.n",
             "m", "harvest"))
    slot(omf, s)[,,,,,i] <- slot(om[[i]], s)
} # }}}

# DROP iters in fixed slots and age=0
om <- slimFLStock(om)

# sr
sr <- list(model='shepherd',
  params=FLPar(a=grid$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
  iter=dim(res)[1]), formula=rec~(4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))

# brp
rp <- FLBRP(om)

# omf
save(om, rp, sr, ind, grid, res, file='omf.RData', compress='xz')

# oms
idx <- sample(1:720, 100)

om <- deSeason(deGender(iter(om, idx)))
rp <- FLBRP(om)
ind <- seasonMeans(areaMeans(ind))

res <- res[idx,]
grid <- grid[idx,]

sr <- list(model='shepherd',
  params=FLPar(a=grid$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
  iter=dim(res)[1]), formula=rec~(4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))

save(om, rp, sr, ind, grid, res, file='oms.RData', compress='xz')

