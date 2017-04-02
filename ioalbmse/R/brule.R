# brule.R - Function implementing the brule MP
# ioalbmse/R/brule.R

# Copyright European Union, 2013-2016
# Authors: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# TODO Parse FLQ args for tune, e.g. bthreshold & bmsy

# brule {{{

#' Run the brule Management Procedure
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
#  -- TIMING
#' @param years For which years is the simulation to be run?
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
#' @name brule
#' @rdname brule
#' @aliases brule brule
#'
#' @author Iago Mosqueira, EC JRC
#' @seealso \code{\link{FLStock}}
#' @keywords utilities
#' @examples
#'
#' omp <- fwdWindow(om, end=dims(om)$maxyear + 3, br)
#'
#' B0 <- brule(omp, sr=sro, years=years, bthreshold=1, blim=0.4*refpts['SBMSY'],
#'  ftarget=refpts['FMSY'], bmsy=refpts['SBMSY',],  
#'  DLAG=2, MLAG=2, SFREQ=2,
#'  errcpue=~rnorm(mean=0, sd=cpue * 0.20), effcpue=~0, errimp=~0,
#'  srresiduals=exp(rnoise(1, FLQuant(0, dimnames=list(year=seq(years[1],
#'    tail(years, 1) + 6))), sd=0.3, b=0.2)))

brule <- function(omp, sr, years, bthreshold, blim, ftarget, bmsy,
 DLAG=1, MLAG=1, SFREQ=1, errcpue=~0, effcpue=~0, srresiduals=~1, errimp=~0, 
 verbose=FALSE) {

  sel <- sel(omp)

  # Relative args
  bthreshold <- bthreshold * bmsy
  blim <- blim * bmsy

  # PB
  if(verbose)
    pb <- utils::txtProgressBar(min = years[1], max = years[length(years)],
      initial = 1, style=3, title="Years:")

  for (y in years[seq.int(1L, length(years), SFREQ)]) {

    # Get catch data, no error
    tc <- window(catch(omp), end=y - DLAG)

    # Get CPUE
    # TODO USE real cpue for historic period
    cpue <- vb(x=window(omp, end=y - DLAG), sel=window(sel, end= y - DLAG)) / 1000

    # CPUE ERROR
    cpue <- cpue + eval(errcpue[[2]])

    # CPUE EFFICIENCY BIAS
    cpue <- cpue * eval(effcpue[[2]])

    # RUN bd model
    eb <- ssb(omp)[,ac(y-DLAG)]
#    eb <- stock(omp)[,ac(y-DLAG)] + (stock(omp)[,ac(y-DLAG)] * rnorm(1, 0, sd=eval(effcpue[[2]])))
    
    # SET TAC for next SFREQ years, starting in y + MLAG
    taf <- FLCore::expand(fbar(omp)[,ac(y-1)], year=seq(y + MLAG, y + MLAG + SFREQ - 1))
    
    # IF EB < BLIM, F = 0
    taf[,,,,,c(eb) <= c(blim)] <- 0.02
    # IF EB >= BTHRESHOLD, F = Ftarget
    taf[,,,,,c(eb) > c(bthreshold)] <- c(ftarget[,c(eb) > c(bthreshold)])
    # ELSE F = f()
    idx <- c(eb) < c(bthreshold) & c(eb) > c(blim)
    taf[,,,,,idx] <- 
      fbar(omp)[,ac(y-DLAG),,,,idx] / (bthreshold[,idx] - blim[,idx]) * (eb[,,,,,idx] - blim[,idx])
    
    # IMPLEMENTATION error
    # nc <- tac + eval(errimp[[2]])

    # Parse srresiduals
    if(is(srresiduals, "formula")) {
      sr.residuals <- eval(srresiduals[[2]]) +
        FLQuant(0, dimnames=c(dimnames(taf)[-2],
          list(year=seq(y + MLAG, y + MLAG + SFREQ - 1))))
    }
    else {
      sr.residuals <- srresiduals[, ac(seq(y + MLAG, y + MLAG + SFREQ - 1))]
    }

    # Project om
    omp <- fwd(omp, asfwdControl(f=taf),
      sr=sr, sr.residuals=sr.residuals, sr.residuals.mult=TRUE)

    # PRINT
    if(verbose)
      setTxtProgressBar(pb, y)
  }
  return(window(omp, start=years[1]-1, end=y))
} # }}}
