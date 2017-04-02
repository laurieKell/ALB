
indicators <- list(
  # S1
  S1 = list(~yearMeans(SB/SB0), name = "mean(SB/SB_0)",
    desc = "Mean spawner biomass relative to unfished"),
  # S2
  S2 = list(~apply(SB/SB0, c(1, 3:6), min), name = "min(SB/SB_0)",
    desc = "Minimum spawner biomass relative to unfished"),
  # S3
  S3 = list(~yearMeans(SB/SBMSY), name = "mean(SB/SB_MSY)",
    desc = "Mean spawnwer biomass relative to BMSY"),
  # S4
  S4 = list(~yearMeans(F/Ftarget), name = "mean(F/F_target)",
    desc = "Mean fishing mortality relative to target"),
  # S5
  S5 = list(~yearMeans(F/FMSY), name = "mean(F/F_MSY)",
    desc = "Mean fishing mortality relative to FMSY"),
  # S6
  S6 = list(~yearSums((SB > SBMSY) + (F < FMSY))/length(SB), name = "P(Green)", 
    desc = "Probability of being in Kobe green quadrant"),
  # S7
  S7 = list(~yearSums((SB < SBMSY) + (F > FMSY))/length(SB), name = "P(Red)", 
    desc = "Probability of being in Kobe red quadrant"),
  # F1
  F1 = list(~yearSums(SB > 0.2 * SB0)/length(SB), name = "P(SB > 0.20 SB0)", 
    desc = "Probability that spawner biomass is above 20% SB_0"),
  # F2
  F2 = list(~yearSums(SB > SBlim)/length(SB), name = "P(B > Blim)", 
    desc = "Probability that spawner biomass is above Blim"),
  # Y1
  Y1 = list(~yearMeans(C), name = "mean(C)", desc = "Mean catch over years"),
  # Y3
  Y3 = list(~yearMeans(C/MSY), name = "mean(C/MSY)",
    desc = "Mean proportion of MSY"),
  # A1
  A1 = list(~yearMeans(SB/SB0), name = "mean(CR)", desc = "Mean catch rate"),
  # T1
  T1 = list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name = "mean(C_t / C_t-1)",
    desc = "Mean absolute proportional change in catch"),
  # T2
  T2 = list(~yearVars(C), name = "var(C)", desc = "Variance in catch"),
  # T3
  T3 = list(~yearVars(F), name = "var(F)",
    desc = "Variance in fishing mortality"),
  # T4
  T4 = list(~yearSums(C < 0.1 * MSY)/length(C), name = "P(C < 0.1 MSY)", 
    desc = "Probability of fishery shutdown")
  )

save(indicators, file="../data/indicators.RData", compress="xz")
