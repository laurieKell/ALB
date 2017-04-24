# grid.R - DESC
# ioalbmse/R/grid.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# nameGrid {{{
#' nameGrid
#'
#' Creates folder names from a 'grid' df of scenarios
#'
nameGrid <- function(df, dir, from=1) {
	df$number <- seq(from=from, length=nrow(df))
	df$id <- paste(df$number, apply(df, 1, function(x)
		paste0(gsub(" ", "", paste0(names(x), as.character(x))), collapse="_")), sep="-")
	return(df)
}
# }}}

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

# rungrid {{{
rungrid <- function(grid, dir=paste0('grid', format(Sys.time(), "%Y%m%d")),
  logfile=paste0(dir, '/run_grid_log'),
	options="") {
	
	cat("START: ", date(), "\n", file=logfile)

  # FIND ss3 in pkg
	if (.Platform$OS.type == "unix") {
    ss3path <- system.file("bin/linux/", package="ioalbmse")
    sep <- ":"
	} else if (.Platform$OS.type == "windows") {
    ss3path <- system.file("bin/windows/", package="ioalbmse")
    sep <- ";"
  }

  # SET $PATH
  path <- paste0(ss3path, sep, Sys.getenv("PATH"))
  Sys.setenv(PATH=path)

	foreach (i=grid$number, .errorhandling = "remove") %dopar% {
    
    row <- which(grid$number == i)
		dirname <- paste(dir, grid[row, "id"], sep="/")
		
    cat("[", i, "]\n")
    cat(grid[row,'id'], ": ", date(), "\n", file=logfile, append=TRUE)

		# SS3!
		workdir <- getwd()
		setwd(dirname)
		system(paste("ss3", options), ignore.stdout = TRUE, ignore.stderr = TRUE)
		setwd(workdir)

		cat("DONE ", grid[row,'id'], ": ", date(), "\n", file=logfile, append=TRUE)
	}

	cat("END: ", date(), "\n", file=logfile, append=TRUE)

	invisible(0)
} # }}}
