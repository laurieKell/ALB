# tmp.R - DESC
# /tmp.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# ----

# HACK: Adding STD_SSB_MSY
vsmsy <- loadres(dir=dir, grid=grid, vars=list(SSB_MSY=4))
resF[, STD_SSB_MSY:=vsmsy$SSB_MSY]
setcolorder(resF, c(1:3,22,4:21))

save(resF, file=paste0(dir, '/resF.RData'), compress='xz')

# TRIM down om and res

# data.frame of K (t) vs. habitat size (km2) for all ALB stocks
prK <- data.frame(
	habitat=c(6073, 244, 3752, 7547, 3779, 7426),
	K=c(474828, NA, 3.576e+5, 3.982e5, 3.5e5, 307830))
dimnames(prK)[[1]] <- c("IO","MED","NAT","NPA","SAT","SPA")

# lm K ~ 0 + habitat
mod <- lm(K ~ 0 + habitat, prK)

# 99.9% CI for t/km2 ratio * IO ALB area (Arrizabalaga et al, 2015, doi: 10.1016/j.dsr2.2014.07.001)
cis <- confint(mod, level = 0.999) * 6073

idx <- resF$TotBio_Unfished <= cis[2]

# SUBSET res
res <- resF[idx,]

# LOAD OM[idx]
om <- loadom(dir=dir, grid=grid)

# DROP age 0
om <- om[-1,,,,,]

# SET range
range(om, c('minfbar', 'maxfbar', 'plusgroup')) <- c(5, 12, 14)

# SR
# HACKED sr FLPar using params from shepherd
srp <- FLPar(a=res$steepness, b=exp(res$`SR_LN(R0)`), c=res$SPB_1950,
	iter=dim(res)[1])

sro <- list(model='shepherd', params=srp,
  formula=rec~(4 * a * b * ssb) / (c * (1 - a) + ssb * (5 * a - 1)))

# BRP
br <- FLBRP(om)

# SR residuals from 1981
sresid <- residuals(window(om, start=1990, end=2008), sro[['formula']], sro[['params']])

om <- fwdWindow(om, end=2017, br)

# UPDATE OM for 2013, 2014, NC from IOTC (2016)
nc <- FLQuant(c(33719.8034529015, 40091.187972784),
  dimnames=list(age='all', year=2013:2014))

sr.residuals <- sresid[,sample(dimnames(sresid)$year, 2, replace=FALSE)]
dimnames(sr.residuals) <- list(year=2013:2014)

om <- fwd(om, asfwdControl(catch=nc), sr=sro, sr.residuals=sr.residuals)

# BRIDGE 'current' years with F=F_y-1
nf <- FLCore::expand(fbar(om)[  ,'2014'], year=2015:2016)
sr.residuals <- sresid[,sample(dimnames(sresid)$year, 2, replace=FALSE)]
dimnames(sr.residuals) <- list(year=2015:2016)

om <- fwd(om, asfwdControl(f=nf), sr=sro, sr.residuals=sr.residuals)

# CUT DOWN to 2016
om <- window(om, end=2016)

# HESSIAN
hess <- loadhessian(dir, grid[idx,])

# REFPTS
refpts <- FLPar(
	B0=res$TotBio_Unfished,
	SB0=res$SPB_1950,
	MSY=res$TotYield_MSY,
	SBMSY=res$SSB_MSY,
	FMSY=res$Fstd_MSY,
	SBtarget=res$SSB_MSY,
	Ftarget=res$Fstd_MSY,
  SBlim=res$SSB_MSY*0.40,
  Flim=res$Fstd_MSY*0.40,
	iter=dim(res)[1])
units(refpts) <- c("t", "t", "t", "t", "f", "t", "f", "t", "f")

# SAVE om
save(om, sro, refpts, res, br, hess, file=paste0(dir, '/om.RData'), compress='xz')

# save(om, sro, refpts, res, br, file='../data/om.RData', compress='xz')

# FULL for PCA

hessF <- loadhessian(dir, grid)

save(resF, hessF, file=paste0(dir, '/resF.RData'), compress='xz')

# EFFECT of final year rec on OM

omp <- fwdWindow(om, end=2033, br)

# NO resid
nf <- FLCore::expand(fbar(om)[  ,'2011'], year=2013:2033)
omr12 <- fwd(omp, asfwdControl(f=nf), sr=sro)

nf <- FLCore::expand(fbar(om)[  ,'2011'], year=2012:2033)
omr11 <- fwd(omp, asfwdControl(f=nf), sr=sro)

plot(omr11, omr12)

# resid
sr.residuals <- sresid[,sample(dimnames(sresid)$year, 22, replace=TRUE)]
dimnames(sr.residuals) <- list(year=2012:2033)

nf <- FLCore::expand(fbar(om)[  ,'2011'], year=2013:2033)
omrr12 <- fwd(omp, asfwdControl(f=nf), sr=sro, sr.residuals=sr.residuals[,-1])

nf <- FLCore::expand(fbar(om)[  ,'2011'], year=2012:2033)
omrr11 <- fwd(omp, asfwdControl(f=nf), sr=sro, sr.residuals=sr.residuals)

plot(omrr11, omrr12)

# CHECK convergence


 Small values of the maximum gradient (approximately 0.001 or less) indicate that model convergence is likely. Larger values (greater than 1) 

  # Check convergence
  Converged <- FALSE
  if("ss3.par" %in% list.files(File)){
    # Move PAR files
    file.rename(from=ParFile,
                to=file.path(File,paste("ss3_",Iteration,"-first.par",sep="")))
    # Read and check
    PAR <- scan(file.path(File,paste("ss3_",Iteration,"-first.par",sep="")),
                what="character", quiet=TRUE)
    if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
      Converged <- TRUE
    }else{
      write(paste("*** Optimization ",1," didn't converge ***",sep=""),
            file=OptRecord,append=TRUE)
    }
}
