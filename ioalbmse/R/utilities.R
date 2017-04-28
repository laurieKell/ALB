# utilities.R - DESC
# ioalbmse/R/utilities.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# window {{{
setMethod("window", signature(x="FLStocks"),
  function(x, ... ) {
    lapply(x, function(y) do.call("window", c(list(y), list(...))))
  }
) # }}}

# vb = vulnerable biomass {{{
setGeneric("vb", function(x, sel, ...) standardGeneric("vb"))

setMethod("vb", signature(x="FLStock", sel="missing"),
  function(x) {
    
    har <- harvest(x)
    minx <- apply(har, 2:6, min)
    maxx <- apply(har, 2:6, max)
    vn <- stock.n(x) * ((har %-% minx) %/% (maxx-minx))
    vb <- quantSums(vn * stock.wt(x))
    
    return(vb)
  }
)

setMethod("vb", signature(x="FLStock", sel="FLQuant"),
  function(x, sel) {
    
    vb <- quantSums(stock.n(x) * sel * stock.wt(x))
    
    return(vb)
  }
)

# }}}

# sel {{{
setGeneric("sel", function(x, ...) standardGeneric("sel"))

setMethod("sel", signature(x="FLStock"),
  function(x) {
    
    har <- harvest(x)
    minx <- apply(har, 2:6, min)
    maxx <- apply(har, 2:6, max)
    return((har %-% minx) %/% (maxx-minx))
  }
) # }}}

# plot(FLStock, FLStock) {{{
setMethod("plot", signature(x="FLStock", y="FLStock"),
  function(x, y, ...) {

    args <- list(...)

    # FIND FLStock objects in ...
    idx <- unlist(lapply(args, is, "FLStock"))

    # CREATE FLStocks list
    fls <- c(list(x, y))
    nms <- c(deparse(substitute(x)), deparse(substitute(y)))

    if(!is.null(idx)) {
      fls <- c(fls, args[idx])

      # DEPARSE args names
      anms <- as.list(substitute(list(...)))[-1][idx]
      names(anms)[names(anms)==""] <- as.character(anms)[names(anms)==""]
      nms <- c(nms, unlist(lapply(anms, as.character)))
    }
    
    names(fls) <- nms

    # TODO PASS on other args to 'plot', args[!idx]
    do.call("plot", list(x=FLStocks(fls)))
  }
) # }}}

# iterDrop {{{
iterDrop <- function(object) {

	foo <- function(x) {
  	if(sum(apply(x@.Data, 1:5, var, na.rm=TRUE), na.rm=TRUE) == 0)
    	x[,,,,,1]
    else
    x}

  return(qapply(object, foo))
} # }}}

# residuals(FLStock, model, params) {{{
setMethod("residuals", signature(object="FLStock"),
  function(object, model, params) {

    if(is(model, 'character') | is(model, 'function'))
      model <- do.call(model, list())$model

    dmo <- dims(object)

    env <- list(
      ssb = window(ssb(object), end=dmo$maxyear - dmo$min))


    env <- c(env, as(params, 'list'))

    rhat <- eval(model[[3]], env)
    rec <- window(rec(object), start=dmo$minyear + dmo$min)
    res <- rec / rhat

    dimnames(res) <- dimnames(rec)

    return(res)

  }
) # }}}

# deSS3 {{{
deSS3 <- function(object, spwnSeason=4, stockSeason=1) {

	# ADD catch/landings/discards.n across seasons
	catch.n <- unitSums(seasonSums(catch.n(object)))
	landings.n <- unitSums(seasonSums(landings.n(object)))
	discards.n <- unitSums(seasonSums(discards.n(object)))

  # CALCULATE stock.n and harvest
  survivors <- unitSums(stock.n(object)[,,,4] *
    exp(- harvest(object)[,,,4] - m(object)[,,,4]))
	stock.n <- unitSums(stock.n(object)[,,,stockSeason])
	dimnames(stock.n) <- list(season="all", unit="unique")
  harvest <- (catch.n * log(survivors / stock.n)) / (survivors - stock.n)
  units(harvest) <- "f"

	# AVERAGE catch/landings/discards.wt across seasons weighted by *.n
	catch.wt <- seasonSums(catch.wt(object) * catch.n(object)) / seasonSums(catch.n(object))
  catch.wt <- unitSums(catch.wt * seasonSums(catch.n(object))) / catch.n

	landings.wt <- seasonSums(landings.wt(object) * landings.n(object)) / seasonSums(landings.n(object))
  landings.wt <- unitSums(landings.wt * seasonSums(landings.n(object))) / landings.n

	discards.wt <- seasonSums(discards.wt(object) * discards.n(object)) / seasonSums(discards.n(object))
  discards.wt <- unitSums(discards.wt * seasonSums(discards.n(object))) / discards.n

  stock.wt <- unitSums(stock.wt(object)[,,,stockSeason] * stock.n(object)[,,,stockSeason]) / stock.n
	dimnames(stock.wt) <- list(season="all", unit="unique")

	# EXTRACT mat from spwnSeason
	mat <- mat(object)[,,1,spwnSeason]
	dimnames(mat) <- list(season="all", unit="unique")

  # m
  m <- seasonSums(m(object)[,,1])
	dimnames(m) <- list(season="all", unit="unique")

	# harvest.spwn & m.spwn
  harvest.spwn <- m.spwn <- m
  harvest.spwn[] <- 0.75
  m.spwn[] <- 0.75

	# CREATE FLStock
	res <- FLStock(catch.n=catch.n, landings.n=landings.n, discards.n=discards.n,
		catch.wt=catch.wt, landings.wt=landings.wt, discards.wt=discards.wt,
		stock.n=stock.n, stock.wt=stock.wt, harvest=harvest,
		m=m, m.spwn=m.spwn, harvest.spwn=harvest.spwn, mat=mat)

	landings(res) <- computeLandings(res)
	discards(res) <- computeDiscards(res)
	catch(res) <-  computeCatch(res)
	stock(res) <- computeStock(res)

	return(res)

} # }}}

# deSeason {{{
deSeason <- function(object, spwnSeason=1, stockSeason=1) {

	# ADD catch/landings/discards.n across seasons
	catch.n <- seasonSums(catch.n(object))
	landings.n <- seasonSums(landings.n(object))
	discards.n <- seasonSums(discards.n(object))

	# AVERAGE catch/landings/discards.wt across seasons weighted by *.n
	catch.wt <- seasonSums(catch.wt(object) * catch.n(object)) / seasonSums(catch.n(object))
	landings.wt <- seasonSums(landings.wt(object) * landings.n(object)) / seasonSums(landings.n(object))
	discards.wt <- seasonSums(discards.wt(object) * discards.n(object)) / seasonSums(discards.n(object))
  units(catch.wt) <- units(landings.wt) <- units(discards.wt) <- units(catch.wt(object))

	# EXTRACT stock.n/wt at start of year
	stock.n <- stock.n(object)[,,,stockSeason]
	dimnames(stock.n)$season <- 'all'
	# Set age-0 stock.n to be from spwnSeason
	stock.n[1,] <- stock.n(object)[1,,,spwnSeason]
	stock.wt <- stock.wt(object)[,,,stockSeason]
	dimnames(stock.wt)$season <- 'all'

	# EXTRACT mat, harvest.spwn, m.spwn from season 1
	mat <- mat(object)[,,,spwnSeason]
	dimnames(mat)$season <- 'all'
	# TODO % F before spwnSeason
	harvest.spwn <- harvest.spwn(object)[,,,1]
	harvest.spwn[] <- (spwnSeason - 1) / dims(object)$season
	dimnames(harvest.spwn)$season <- 'all'
	# TODO % M before spwnSeason
	m.spwn <- m.spwn(object)[,,,1]
	m.spwn[] <- (spwnSeason - 1) / dims(object)$season
	dimnames(m.spwn)$season <- 'all'

	# ADD harvest and m across seasons
	# TODO CHECK units == "f"
	harvest <- seasonSums(harvest(object))
	m <- seasonSums(m(object))
	# CORRECT m for age0 if spwnSeason > 1
	m[1,] <- m[1,] / spwnSeason

	# CREATE FLStock
	res <- FLStock(catch.n=catch.n, landings.n=landings.n, discards.n=discards.n,
		catch.wt=catch.wt, landings.wt=landings.wt, discards.wt=discards.wt,
		stock.n=stock.n, stock.wt=stock.wt, harvest=harvest,
		m=m, m.spwn=m.spwn, harvest.spwn=harvest.spwn, mat=mat)

	landings(res) <- computeLandings(res)
	discards(res) <- computeDiscards(res)
	catch(res) <-  computeCatch(res)
	stock(res) <- computeStock(res)

	return(res)

} # }}}

# deGender {{{
deGender <- function(object) {

	# ADD catch/landings/discards/stock.n across units
	catch.n <- unitSums(catch.n(object))
	landings.n <- unitSums(landings.n(object))
	discards.n <- unitSums(discards.n(object))
	stock.n <- unitSums(stock.n(object))

	# AVERAGE catch/landings/discards.wt across units weighted by *.n
	catch.wt <- unitSums(catch.wt(object) * catch.n(object)) / unitSums(catch.n(object))
	landings.wt <- unitSums(landings.wt(object) * landings.n(object)) / unitSums(landings.n(object))
	discards.wt <- unitSums(discards.wt(object) * discards.n(object)) / unitSums(discards.n(object))
	stock.wt <- unitSums(stock.wt(object) * stock.n(object)) / unitSums(stock.n(object))

	harvest <- unitSums(harvest(object) * stock.n(object)) / unitSums(stock.n(object))
  units(harvest) <- units(harvest(object))
	m <- unitSums(m(object) * stock.n(object)) / unitSums(stock.n(object))

	# EXTRACT mat, harvest.spwn, m.spwn from unit 'F'
	mat <- mat(object)[,,'F',]
	dimnames(mat)$unit <- 'unique'
	harvest.spwn <- harvest.spwn(object)[,,'F',]
	dimnames(harvest.spwn)$unit <- 'unique'
	m.spwn <- m.spwn(object)[,,'F',]
	dimnames(m.spwn)$unit <- 'unique'

	# CREATE FLStock
	res <- FLStock(catch.n=catch.n, landings.n=landings.n, discards.n=discards.n,
		catch.wt=catch.wt, landings.wt=landings.wt, discards.wt=discards.wt,
		stock.n=stock.n, stock.wt=stock.wt, harvest=harvest,
		m=m, m.spwn=m.spwn, harvest.spwn=harvest.spwn, mat=mat)

	landings(res) <- computeLandings(res)
	discards(res) <- computeDiscards(res)
	catch(res) <-  computeCatch(res)
	stock(res) <- computeStock(res)

	return(res)

} # }}}

# combine {{{
setMethod("combine", signature(x="FLStocks", y="missing"),
  function(x) {

    res <- propagate(x[[1]], length(x))
    for(i in seq(2, length(x)))
      res[,,,,,i] <- x[[i]]
    return(res)
  }
) # }}}

# HACK: slimFLStock, as FLash::fwd does not respect iter=1 slots
# TODO MOVE to combine as arg
slimFLStock <- function(x) {
  discards(x) <- discards(x)[,,,,,1]
  discards.n(x) <- discards.n(x)[,,,,,1]
  discards.wt(x) <- discards.wt(x)[,,,,,1]
  stock.wt(x) <- stock.wt(x)[,,,,,1]
  mat(x) <- mat(x)[,,,,,1]
  harvest.spwn(x) <- harvest.spwn(x)[,,,,,1]
  m.spwn(x) <- m.spwn(x)[,,,,,1]

  return(x)
}

# exec scripts
# edit(file=makeOM())
makeOM <- function() return(system.file("exec", "makeOM.R", package="ioalbmse"))

.onLoad <- function(libname, pkgname) {
	if (.Platform$OS.type == "unix") {
    Sys.chmod(system.file('bin/linux/ss3', package=pkgname),
      mode = "0755", use_umask = TRUE)
  }
}

# %++% {{{
"%++%" <- function(x, y) {
  
  dy <- dimnames(y)

  names(dy) <- c("i", "j", "k", "l", "m", "n")

  value <- do.call("[", c(list(x=x), dy)) + y
  res <- do.call("[<-", c(list(x=x, value=value), dy))

  return(res)
} # }}}
