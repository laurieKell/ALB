# selectivity_by_iter.R - DESC
# /selectivity_by_iter.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)
library(r4ss)

data(oms)

# PLOT

ggplot(iter(harvest(om)[,'2014',1,1] %/% apply(harvest(om)[,'2014',1,1], 2:6, max),1),
  aes(x=age, y=data, group=iter)) + geom_line()

out <- SS_output('../../../selex/run')
sa <- SS_output('../../../sa/run')

# AGE SELEX
selex <- data.table(out$ageselex)
saselex <- data.table(sa$ageselex)

# factor fleet year seas gender morph label
# FLEETS: F1_LL1 F2_LL2 F3_LL3 F4_LL4 (1:4)

# SELECT year=2014, fleet=1:4
foo <- melt(selex[year==2014 & factor=="Asel2" & fleet %in% 1:4 & morph==3,],
  id.vars=c("fleet", "seas", "gender"),
  measure.vars=ac(0:14), variable.name="age")
safoo <- melt(saselex[year==2014 & factor=="Asel2" & fleet %in% 1:4 & morph==3,],
  id.vars=c("fleet", "seas", "gender"),
  measure.vars=ac(0:14), variable.name="age")

# COMPUTE mean by gender,
ggplot(foo, aes(x=age, y=value, group=fleet)) +
  geom_line(aes(colour=as.factor(fleet))) + facet_grid(seas~.)


all <- rbind(cbind(safoo, mod='sa'), cbind(foo, mod='sel'))

ggplot(all, aes(x=age, y=value, group=fleet)) +
  geom_line(aes(colour=as.factor(fleet))) + facet_grid(seas~mod, labeller = label_both)

# --
fli <- readFLIBss3('../../../selex/run')

plot(fli[,,,,-1])

# --- CPUE Q

# SUBSET from out
cpue <- data.table(out[[c("cpue")]])

# PLOT resid
ggplot(cpue[Yr > 2003], aes(x=ISOdate(Yr, (Seas * 3) - 2, 1), y=Exp-Obs)) +
  geom_line() + geom_hline(aes(yintercept=0)) + facet_grid(Name ~ .)

cpue[, .(mean=mean(Calc_Q), var=var(Calc_Q)), by=.(Name, Seas)]
cpue[Yr==2014, Calc_Q, by=.(Name, Seas)]
