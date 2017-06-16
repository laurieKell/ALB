# harvest.R - DESC
# ioalbmse/R/harvest.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# harvest {{{
# F_t = ln(N_t / N_t+1) - M_t
#
setMethod("harvest", signature(object="FLQuant", catch="FLQuant"),
  function(object, catch, m) {
 
    # EMPTY harvest FLQ   
    har <- m
    har[] <- NA

    # dims, ages and years - 1
    dm <- dim(har)
    aa <- seq(1, dm[1] - 2)
    yy <- seq(1, dm[2] - 1)
    
    # MINIMIZES diff in catch
    foo <- function(logf, n, c, m, ratio) {
      f <- exp(logf)
      ch <- (f / (f + m)) * (1 - exp(-f - m)) * n
      cr <- (c - ch)
      return(cr^2)
    }

    # YEARLY
    if(dm[4] == 1) {

      # a-1 ages and y-1 years
      n0 <- object[aa, yy]
      n1 <- object[aa+1, yy + 1]
      har[aa,yy] <- log(n0/n1) - m[aa,yy]

      # LOOP over ages for last year, by unit & area
      for(i in seq(dm[1])) {
        for(k in seq(dm[3])) {
          for(mm in seq(dm[5])) {
            res <- optimize(f=foo, interval = log(c(1e-8,3)),
              n=c(object[i,dm[2],k,,mm]),
              c=c(catch[i,dm[2],k,,mm]),
              m=c(m[i,dm[2],k,,mm]))$minimum
          har[i,dm[2],k,,mm] <- exp(res)
          }
        }
      }

      # LOOP over years for last 2 ages, by unit & area
      for(j in seq(dm[2])) {
        for(k in seq(dm[3])) {
          for(mm in seq(dm[5])) {
            for(i in c(dm[1]-1, dm[1])) {
              res <- optimize(f=foo, interval = log(c(1e-8,3)),
                n=c(object[i,j,k,,mm]),
                c=c(catch[i,j,k,,mm]),
                m=c(m[i,j,k,,mm]))$minimum
              har[i,j,k,,mm] <- exp(res)
            }
          }
        }
      }

    # SEASONS
    } else {

      # seasons s=1:n-1, log(s+1/s)
      ss <- seq(1, dm[4] - 1)
      n0 <- object[,,,ss]
      n1 <- object[,,,ss + 1]
      har[,,,ss] <- log(n0/n1) - m[,,,ss]

      # last season
      n0 <- object[aa,yy,,dm[4]]
      n1 <- object[aa + 1,yy + 1,,1]
      har[aa,yy,,dm[4]] <- log(n0/n1) - m[aa,yy,,dm[4]]

      # DIV/0 to 0
      har[har < 0] <- 0

      # Q & D, 1999 (Page 325)
      # C_y,a = N_y,a * (F_y,a / (F_y,a + M_y,a)) * (1 - exp((-F_y,a - M_y,a) * tau))
      # NOTE: IGNORING tau (no. years represented by pgroup)
      
      # LOOP over units
      for(u in seq(dm[3])) {
        # LOOP over years and last 2 ages
        for(y in seq(dm[2]-1)) {
          for(a in c(dm[1]-1, dm[1])) {
            res <- optimize(f=foo, interval = log(c(1e-8,3)),
              n=c(object[a,y,u,4]),
              c=c(catch[a,y,u,4]),
              m=c(m[a,y,u,4]))$minimum
            har[a,y,u,4] <- exp(res)
          }
        }
        # LOOP over ages for last year and season
        for(a in seq(dm[1])) {
          res <- optimize(f=foo, interval = log(c(1e-8,3)),
            n=c(object[a,dm[2],u,4]),
            c=c(catch[a,dm[2],u,4]),
            m=c(m[a,dm[2],u,4]))$minimum
          har[a,dm[2],u,4] <- exp(res)
        }
      }
    }

    har[is.na(har)] <- 0
    units(har) <- "f"

    return(har)
  }
) # }}}
