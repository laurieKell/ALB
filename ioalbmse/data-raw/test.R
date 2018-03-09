# test.R - DESC
# /test.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

data(indicators)
data(oms)

metrics=list(C=catch, F=fbar, SB=ssb)

out <- performance(om, refpts=rpts, mp="index",
  indicators=indicators, years=list(seq(2000,2014)), metrics=metrics)
