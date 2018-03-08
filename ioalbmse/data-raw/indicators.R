# indicators.R - DESC
# ioalbmse/data-raw/indicators.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

indicators <- list(
  # S1
  S1 = list(~yearMeans(SB/SB0), name = "SB/SB[0]",
    desc = "Mean spawner biomass relative to unfished"),
  # S2
  S2 = list(~apply(SB/SB0, c(1, 3:6), min), name = "min(SB/SB[0]", 
    desc = "Minimum spawner biomass relative to unfished"),
  # S3
  S3 = list(~yearMeans(SB/SBMSY), name = "SB/SB[MSY]",
    desc = "Mean spawnwer biomass relative to BMSY"),
  # S4
  S4 = list(~yearMeans(F/Ftarget), name = "F/F[target]",
    desc = "Mean fishing mortality relative to target"),
  # S5
  S5 = list(~yearMeans(F/FMSY), name = "F/F[MSY]",
    desc = "Mean fishing mortality relative to FMSY"),
  # S6
  S6 = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # S7
  S7 = list(~yearSums(FLQuant((SB / SBMSY) < 1 & (F / FMSY) > 1)) / dim(SB)[2],
    name = "P(Red)", desc = "Probability of being in Kobe red quadrant"),
  # S8
  S8 = list(~yearMeans((SB / SBMSY) > 1),
    name = "P(SB > SB[MSY])",  desc = "Probability of SB greater than SBMSY"),
  # F1
  F1 = list(~yearSums((SB / (0.2 * SB0)) > 1) / dim(SB)[2],
    name = "P(SB > 0.20 %*% SB[0])",
    desc = "Probability that spawner biomass is above 20% SB_0"),
  # F2
  F2 = list(~yearSums((SB / SBlim) > 1) / dim(SB)[2], name = "P(SB > B[limit])", 
    desc = "Probability that spawner biomass is above Blim"),
  # Y1
  Y1 = list(~yearMeans(C), name = "Mean catch", desc = "Mean catch over years"),
  # Y3
  Y3 = list(~yearMeans(C/MSY), name = "C/MSY", desc = "Mean proportion of MSY"),
  # A1
  A1 = list(~yearMeans(SB/SB0), name = "Catch rate", desc = "Mean catch rate"),
  # T1
  T1 = list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name = "Catch variability",
    desc = "Mean absolute proportional change in catch"),
  # T2
  T2 = list(~yearVars(C), name = "Catch variance", desc = "Variance in catch"),
  # T3
  T3 = list(~yearVars(F), name = "F variance", desc = "Variance in fishing mortality"),
  # T4
  T4 = list(~yearSums(C < 0.1 * MSY) / dim(C)[2], name = "P(catch < 0.1 %*% MSY)", 
    desc = "Probability of fishery shutdown")
  )

save(indicators, file="../data/indicators.RData", compress="xz")
