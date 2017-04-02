# om2016.R - DESC
# /om2016.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplotFL)
library(ioalbmse)

# FULL om
load('../../../om/omf.RData')

# GET metrics
omq <- FLQuants(SSB=unitSums(ssb(om)[,,,4]), C=seasonSums(unitSums(catch(om))),
  F=unitSums(seasonSums(fbar(om))), REC=unitSums(rec(om)[,,,4]))

# SUBSET SB0 < 9e+05
idx <- res$SPB_1950 < 9e+05
oms <- lapply(omq, function(x) x[,,,,,idx])

# PDF
pdf("om2016.pdf")

# M sigmaR steepness cpuecv ess llq llsel
ggplot(cbind(grid, res), aes(y=log(SPB_1950), x=llsel)) + geom_point() +
  facet_grid(M~steepness) + geom_hline(aes(yintercept=log(9e+05)))

# metrics
plot(omq)

# subset metrics
plot(oms)

dev.off()
