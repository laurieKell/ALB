# grid.R - DESC
# ioalbmse/R/grid.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# setgrid {{{
setgrid <- function(sce, dir=paste0('grid_', format(Sys.time(), "%Y%m%d")),
  base=system.file("sa/2016", package="ioalbmse"), name='abt', from=1, write=TRUE) {
	
  # EXPAND grid from sce
	grid <- nameGrid(expand.grid(sce, stringsAsFactors=FALSE), from=from)

  if(!write)
    return(grid)

  # SET ctl, dat full paths
  ctlf <- paste0(base, "/", name, ".ctl")
  datf <- paste0(base, "/", name, ".dat")
 	
  # READ source files
  dats <- r4ss::SS_readdat(datf, verbose=FALSE)
  ctls <- r4ss::SS_readctl_3.24(file=ctlf, use_datlist=T, datlist=dats, verbose=FALSE)

  # NAMES in grid
  pars <- names(grid)[!names(grid) %in% c("number", "id")]

  # CREATE dir
  if(dir.exists(dir))
    stop(paste("folder", dir, "already exists. Delete first."))
	
  dir.create(dir)

	# SETUP grid
  foreach (i=grid$number, .errorhandling = "remove") %dopar% {

    dat <- dats
    ctl <- ctls

    row <- which(grid$number == i)
 
    # M
    if("M" %in% pars) {
      natM <- c(seq(from=as.numeric(substr(as.character(grid[row,'M']), 1, 2))/10,
        to=as.numeric(substr(as.character(grid[row,'M']), 3, 4))/10, length=6),
		  	rep(as.numeric(substr(as.character(grid[row,'M']), 3, 4))/10 , 9))
      ctl$natM[] <- rep(natM, each=ctl$Ngenders)
    }

    # sigmaR, #_SRparm3
    if("sigmaR" %in% pars) {
      ctl$SRparm["SR_sigmaR", c("INIT", "PRIOR")] <- grid[row, "sigmaR"]
    }

		# steepness
    if("steepness" %in% pars) {
      ctl$SRparm["SR_BH_steep", c("INIT", "PRIOR")] <- grid[row, "steepness"]
    }

    # llsel
    if("llsel" %in% pars) {
      if(grid[row, 'llsel'] == "DoNorm") {
        # CHANGE fleet 3 (S) 
        ctl$size_selex_parms[7:12, "INIT"] <- c(90, -0.50, 5.00, 6.00, -999, 0)
      }

    }

    # cpuecv
    if("cpuecv" %in% pars) {
      dat$CPUE$se_log <- grid[row, "cpuecv"]
    }

    # ESS obs
    if("ess" %in% pars) {
      dat$lencomp$Nsamp <- grid[row, "ess"]
    }
	
    # CPUE Q increases
    if("llq" %in% pars) {
      cpue <- data.table(dat$CPUE)
      dat$CPUE <- cpue[, obs:=obs / as.numeric(grid[row, "llq"]) ^ seq(0,
        length(obs) - 1), by=index]
    }

		# CREATE dir
		dirname <- paste(dir, grid[row, "id"], sep='/')
		dir.create(dirname)

		# COPY unaltered files
		# starter.ss
		file.copy(paste(base, "starter.ss", sep="/"),
			paste(dirname, "starter.ss", sep="/"))
		
		# forecast.ss
		file.copy(paste(base, "forecast.ss", sep="/"),
			paste(dirname, "forecast.ss", sep="/"))

		# WRITE modified files
		# ctl
    r4ss::SS_writectl_3.24(ctl, paste0(dirname, "/", name, ".ctl"), nseas=ctl$nseas)
		
    # dat
    r4ss::SS_writedat(dat, outfile=paste0(dirname, "/", name, ".dat"))
		}

	invisible(grid)
} # }}}
