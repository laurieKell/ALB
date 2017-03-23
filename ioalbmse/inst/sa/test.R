# test.R - DESC
# /test.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

devtools::install_github('r4ss/r4ss')

library(r4ss)

dats <- r4ss::SS_readdat("2016/abt.dat", verbose=TRUE)

ctls <- r4ss::SS_readctl_3.24(file="2016/abt.ctl", use_datlist=TRUE, datlist=dats, verbose=TRUE)


