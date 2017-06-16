# load.R - DESC
# ioalbmse/R/load.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# loadres(dirs, vars, progress=TRUE) {{{
loadres <- function(dirs,
  vars=list(TotBio_Unfished=3, SPB_1950=3, SSB_MSY=3, SPB_2015=3, F_2015=3,
  Fstd_MSY=3, TotYield_MSY=3, `SR_LN(R0)`=3, LIKELIHOOD=2, Convergence_Level=2,
  Survey=2, Length_comp=2, Catch_like=2, Recruitment=2), progress=TRUE) {

	# Loop over dirs
	out <- foreach(i=seq(length(dirs)), .errorhandling = "remove" ) %dopar% {

		if(progress)
			cat(paste0('[', i, ']\n'))
    
    # CONVERGED?
    if(!file.exists(paste0(dirs[i], "/covar.sso"))) {
      setNames(data.frame(matrix(NA, ncol = length(vars), nrow = 1)), names(vars))
    } else {
    # READ results
		readRPss3(paste(dirs[i], "Report.sso", sep="/"), vars)
    }
	}

  # rbind 
  res <- rbindlist(out)

  # names(res)[4] <- "STD_SSB_MSY"

  # res <- cbind(res, grid)

	# CHANGE SSB_MSY to both sexes
	# res$SSB_MSY <- res$SSB_MSY

	return(res)
} # }}}

# loadrec(dirs, progress=TRUE) {{{
loadrec <- function(dirs, progress=TRUE, object="resid") {

	# Loop over dirs
	out <- foreach(i=seq(length(dirs)), .errorhandling = "remove" ) %dopar% {

		if(progress)
			cat(paste0('[', i, ']\n'))

		readFLQsss3(dirs[i])
	}
	
  res <- foreach(i=object, .errorhandling = "remove" ) %dopar% {
    lapply(out, "[[", i)
  }
  
  res <- lapply(res, function(x) Reduce(combine, x))
  names(res) <- object

	return(res)
} # }}}

# loadom(dirs, progress=TRUE) {{{
loadom <- function(dirs, progress=TRUE, ...) {

	# LOOP over dirs
  om <- foreach(i=seq(length(dirs)), .combine=combine) %dopar% {
    if(progress)
      cat("[", i, "]\n", sep="")
    readFLSss3(dirs[i], ...)
  }

  # DROP undeeded extra iters
  # om <- slimFLStock(om)

	return(om)
} # }}}

# loadindex(dirs, progress=TRUE) {{{
loadindex <- function(dirs, progress=TRUE, fleets) {

	# LOOP over dirs
  ind <- foreach(i=seq(length(dirs))) %dopar% {

    if(progress)
      cat("[", i, "]\n", sep="")

    out <- r4ss::SS_output(dirs[i], verbose=FALSE, hidewarn=TRUE, warn=FALSE,
      printstats=FALSE, covar=FALSE, forecast=FALSE)
    
    # dfs from out
    cpue <- data.table(out[[c("cpue")]])
    selex <- data.table(out[["ageselex"]])

    # EXTRACT index, residuals and selectivity
    index <- ss3index(cpue, fleets)
    index.res <- ss3index.res(cpue, fleets)
    sel.pattern <- ss3sel.pattern(selex, unique(cpue$Yr), 3)
    
    # MERGE across fleets
    list(index=index,
      index.res=index.res,
      sel.pattern=sel.pattern)
  }

  # ind: iter - slot - index/flqs
  
  # out: index - slot - iter
  
  index <- Reduce(combine, lapply(ind, "[[", 'index'))
  index.q <- Reduce(combine, lapply(ind, "[[", 'index.q'))
  sel.pattern <- Reduce(combine, lapply(ind, "[[", 'sel.pattern'))

	return(FLQuants(index=index, index.q=index.q, sel.pattern=sel.pattern))
} # }}}

# loadhessian {{{
loadhessian <- function(dir, grid) {
	
  dirs <- paste(dir, grid$id, sep='/')

	res <- vector('list', length=nrow(grid))
		names(res) <- grid$number

	for(i in seq(length(grid$number))) {

    cat(paste0("[", i, "]\n"))

		filename <- file(paste(dir, grid$id[i], "admodel.hes", sep='/'), "rb")

  	num.pars <- readBin(filename, "integer", 1)
  	hes.vec <- readBin(filename, "numeric", num.pars^2)
  
		hes <- matrix(hes.vec, ncol=num.pars, nrow=num.pars)
  	hybrid_bounded_flag <- readBin(filename, "integer", 1)
  	scale <- readBin(filename, "numeric", num.pars)
  	
		res[[i]] <- list(num.pars = num.pars, hes = hes,
			hybrid_bounded_flag = hybrid_bounded_flag, scale = scale)

		close(filename)
	}

	return(res)

} # }}}

# getRange {{{
getRange <- function(x) {
	
  # empty range
	range <- rep(as.numeric(NA), 7)
	names(range) <- c("min", "max", "plusgroup", "minyear", "maxyear", "minfbar", "maxfbar")
	
  # age range from catage
	range[c("min", "max")] <- range(as.numeric(names(x)[-(1:10)]))
	
  # plusgroup = max
	range["plusgroup"] <- range["max"]
	
  # min/maxfbar = min/max
	range[c("minfbar", "maxfbar")] <- range[c("min", "max")]
	
  # year range from catage
	range[c("minyear", "maxyear")] <- range(x$Yr[x$Era == "TIME"])

  # set plusgroup to max age
	range["plusgroup"] <- range["maxyear"]
	
  return(range)
} # }}}
