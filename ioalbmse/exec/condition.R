# condition.R - DESC
# ioalbmse/exec/condition.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(6)

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

dir <- "/media/mosquia/VB/repository/IO_ALB/grid20170306"
setwd(dir)

grid <- setgrid(sce, dir=dir, base=system.file("sa/2016", package="ioalbmse"),
  name="abt", write=FALSE)

save(grid, file=paste0(dir, "/grid.RData"))

# -- RUN SS3 grid

rungrid(grid, options="", dir=dir)

# -- LOAD results

# res
res <- cbind(grid[,-9], loadres(dirs=grid$id))

# rpts: MSY, SB_MSY, F_MSY, SB0
rpts <- FLPar(MSY=res$TotYield_MSY, SBMSY=2 * res$SSB_MSY, FMSY=res$Fstd_MSY,
  SB0=2 * res$SPB_1950, Ftarget=res$Fstd_MSY, SBlim=2 * 0.40*res$SSB_MSY)

# om
om <- loadom(dirs=grid$id)

# sr
osr <- list(model='shepherd',
  params=FLPar(a=res$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
  iter=dim(res)[1]), formula=rec ~ (4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))

# brp
orp <- FLBRP(om)

# index: cpue, sd, b
cpue <- foreach(i=seq(length(grid$id))) %dopar% {
  print(i)
  readFLIBss3(dir=grid$id[i], fleets=c(LLCPUE3=3))
}

index <- cpue[[1]]@index
sel.pattern <- Reduce('combine', lapply(cpue, function(x) x@sel.pattern))
index.res <- Reduce('combine', lapply(cpue, function(x) x@index.var))
index.q <- Reduce('combine', lapply(cpue, function(x) x@index.q))

save(cpue, index, sel.pattern, index.res, index.q, file='cpue.RData', compress='xz')

# cpue list
# TODO REVIEW deSeason, deGender
cpue <- list(index=seasonMeans(index),
  sel.pattern=seasonMeans(unitMeans(sel.pattern)),
  index.q=seasonMeans(index.q),
  index.res=seasonMeans(index.res)
) 

# --- OMF
# om orp rpts osr res ind
save(om, orp, rpts, osr, res, cpue, file='omf.RData', compress='xz')


# --- TRIM down om and res

# load("omf.RData")

# data.frame of K (t) vs. habitat size (km2) for all ALB stocks
prK <- data.frame(
	habitat=c(6073, 244, 3752, 7547, 3779, 7426),
	K=c(474828, NA, 3.576e+5, 3.982e5, 3.5e5, 307830))
dimnames(prK)[[1]] <- c("IO","MED","NAT","NPA","SAT","SPA")

# lm K ~ 0 + habitat
mod <- lm(K ~ 0 + habitat, prK)

# 99.9% CI for t/km2 ratio * IO ALB area (Arrizabalaga et al, 2015, doi: 10.1016/j.dsr2.2014.07.001)
cis <- confint(mod, level = 0.999) * 6073

# idx
idx <- res$TotBio_Unfished <= cis[2]


# --- OM SUBSET, deGender and deSeason
# om orp rpts osr res ind

om <- deSeason(deGender(FLCore::iter(om, seq(1, 1440)[idx])))
orp <- FLBRP(om)
res <- res[idx,]
rpts <- FLPar(MSY=res$TotYield_MSY, SBMSY=2 * res$SSB_MSY, FMSY=res$Fstd_MSY,
  SB0=2 * res$SPB_1950, Ftarget=res$Fstd_MSY, SBlim=2 * 0.40 * res$SSB_MSY)
osr <- list(model='shepherd',
  params=FLPar(a=res$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
  iter=dim(res)[1]), formula=rec ~ (4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))
cpue <- list(index=cpue$index,
  sel.pattern=cpue$sel.pattern[,,,,,idx],
  index.q=cpue$index.q[,,,,,idx],
  index.res=cpue$index.res[,,,,,idx]
) 

save(om, orp, res, rpts, osr, cpue, file='om.RData', compress='xz')

# --- OMS - For TESTING

idx <- sample(seq(dimnames(m(om))$iter), 100)

om <- FLCore::iter(om, idx)
orp <- FLBRP(om)
res <- res[idx, ]
rpts <- rpts[, idx]
osr <- list(model='shepherd',
  params=FLPar(a=res$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
  iter=dim(res)[1]), formula=rec~(4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))
cpue <- list(index=cpue$index,
  sel.pattern=cpue$sel.pattern[,,,,,idx],
  index.q=cpue$index.q[,,,,,idx],
  index.res=cpue$index.res[,,,,,idx]
) 

save(om, orp, res, rpts, osr, cpue, file='oms.RData', compress='xz')

# --- OM - SUBSET based on analysis
