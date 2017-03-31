# irate.R - Function implementing the irate MP
# ioalbmse/R/irate.R

# Copyright European Union, 2013-2016
# Authors: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# irate {{{

#' Run the irate Management Procedure
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' nunc euismod ante fringilla lobortis. Aliquam ullamcorper in diam non placerat. 
#'
#  -- INPUTS
#' @param omp An FLStock object with the extended OM
#' @param sr A list with SR params and model, to be passed to fwd()
#  -- PARAMS
#' @param responsiveness
#' @param hr_multiplier
#' @param biomass_threshold
#' @param biomass_limit
#  -- TIMING
#' @param years For which years is the simulation to be run?
#' @param maxTAC
#' @param DLAG
#' @param MLAG
#' @param SFREQ
#  -- ERROR
#' @param errorcpue
#' @param effcpue
#' @param errorcpue
#' @param srresiduals
#' @param errimp
#  -- EXTRA
#' @param verbose Should progress be reported on screen? Defaults to TRUE
#'
#' @return FLStock
#'
#' @name irate
#' @rdname irate
#' @aliases irate irate
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \code{\link{FLStock}}
#' @keywords utilities
#' @examples
#'
#' data(om)
#' omp <- fwdWindow(om, end=dims(om)$maxyear + 3, br)
#'
#' # Hindcast run
#' run <- irate(omp, sro, years=1990:2012, yref=c(1981:2010),
#'   responsiveness=0.5, DLAG=1, MLAG=1, SFREQ=2,
#'   hr_multiplier= 1.1, biomass_threshold=0.5,
#'   biomass_limit=0.2, maxTAC=500000,
#'   srresiduals=~rlnorm(dims(om)$iter, 0, 0.5), verbose=FALSE)
#'
#' plot(window(FLStocks(run=run, om=om), end=2012))
#' 

irate <- function(omp, sr, years, yref=seq(dims(omp)$minyear, years[1]-1),
  responsiveness, hr_multiplier, biomass_threshold, biomass_limit, maxTAC,
  DLAG=1, MLAG=1, SFREQ=1,
  errcpue=~0, effcpue=~0, srresiduals=~0, errimp=~0, 
  verbose=FALSE) {
  
  # Selectivity
  sel <- sel(omp)

  # GET historic levels
  rmet <- metrics(omp[,ac(yref)], catch=catch, ssb=ssb, vb=vb)
  hr_multiplier <- hr_multiplier * yearMeans(rmet$catch / (rmet$ssb / 1000))
  biomass_threshold <- biomass_threshold * (yearMeans(rmet$ssb) / 1000)
  biomass_limit <- biomass_limit * (yearMeans(rmet$ssb) / 1000)

  # PB
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3, title="Years:")

  # Initial smoother value is 0
  cpue_smooth <- NULL
  
  # Get CPUE
  # cpue <- vb(x=omp, sel=sel) / 1000
  cpue <- quantSums((stock.n(omp) * stock.wt(omp))[5:14,])
  
  for (y in years[seq.int(1L, length(years), SFREQ)]) {

    # Get catch data, no error
    tc <- catch(omp)[,ac(y - DLAG)]
    
    # TODO UPDATE cpue
    cpue[,ac(y - DLAG)] <- vb(x=omp[,ac(y - DLAG)], sel=sel[,ac(y - DLAG)]) / 1000
  
    # CPUE EFFICIENCY BIAS
    cpue[,ac(y - DLAG)] <- cpue[,ac(y - DLAG)] +
      cpue[,ac(y - DLAG)] * eval(effcpue[[2]]) ^ SFREQ

    # CPUE ERROR
    cpue[,ac(y - DLAG)] <- cpue[,ac(y - DLAG)] + eval(errcpue[[2]], list(cpue=cpue[, ac(y - DLAG)]))
    
    # Get smoothed CPUE using last available timestep
    if (is.null(cpue_smooth)){
      cpue_smooth <- cpue[,ac(y - DLAG)]
    } else {
      cpue_smooth <- responsiveness * cpue[,ac(y - DLAG)] +
        (1 - responsiveness) * cpue_smooth
    }

    # Calc recommended catch scalar
    rate <- cpue_smooth
    rate[] <- (cpue_smooth - biomass_limit) * hr_multiplier /
      (biomass_threshold - biomass_limit)
    rate[cpue_smooth < biomass_limit] <- 0
    rate[cpue_smooth > biomass_threshold] <- hr_multiplier[cpue_smooth > biomass_threshold]

    # Set catch for next SFREQ years, starting in y + MLAG
    tac <- FLCore::expand(tc, year=seq(y + MLAG, y + MLAG + SFREQ - 1))
    tac[] <- rep(pmin(c(cpue_smooth * rate), c(maxTAC)), each=SFREQ)

    # IMPLEMENTATION error
    nc <- tac + eval(errimp[[2]])

    # Parse srresiduals
    if(is(srresiduals, "formula")) {
      sr.residuals <- eval(srresiduals[[2]]) +
        FLQuant(0, dimnames=c(dimnames(nc)[-2],
        list(year=seq(y + MLAG, y + MLAG + SFREQ - 1))))
    }
    else {
      sr.residuals <- srresiduals[, ac(seq(y + MLAG, y + MLAG + SFREQ - 1))]
    }

    # Project om
    omp <- fwd(omp, asfwdControl(catch=nc),
      sr=sr, sr.residuals=sr.residuals, sr.residuals.mult=TRUE)

    # PRINT
    if(verbose)
      setTxtProgressBar(pb, y)
  }
  return(window(omp, start=years[1]-1, end=y))
} # }}}
