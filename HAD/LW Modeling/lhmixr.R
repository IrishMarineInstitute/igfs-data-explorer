library(lhmixr)

set.seed(1010)
sim.dat <- sim_vb_data(nfemale = 50, nmale = 50, mean_ageF = 4, mean_ageM = 4,
                       growth_parF = c(linf = 30, k = 0.5, t0 = -1, sigma = 0.1),
                       growth_parM = c(linf = 25, k = 0.5, t0 = -1, sigma = 0.1),
                       mat_parF = c(A50 = 5, MR = 2), mat_parM = c(A50 = 3, MR = 2),
                       distribution = "lognormal")
## Model fit with contrained Brody's growth coefficient
## Set up the constraint
binding <- matrix(c(1:2, rep(3, 2), 4:7), ncol = 2, byrow = TRUE)
rownames(binding) <- c("lnlinf", "lnk", "lnnt0", "lnsigma")
colnames(binding) <- c("female", "male")
## note: lnnt0 is the natural logarithm of the negative of t0 (t0 < 0)
## starting values
start.par <- c(c(log(30), log(25)), rep(log(0.3), 1), rep(log(1), 2), rep(log(.1), 2))
start.list <- list(par = list(mixprop = 0.5, growth.par = start.par))
vb.bind.fit <- vb_growth_mix(data = sim.dat, start.list = start.list,
                             binding = binding, distribution = "lognormal",
                             reltol = 1e-6, plot.fit=TRUE)


#starting parameters males/females
LengthWeightAgeF=filter(LengthWeightAge, fldFishSex=="F")
dim(LengthWeightAgeF)
LengthWeightAgeM=filter(LengthWeightAge, fldFishSex=="M" & fldResult1<10)
dim(LengthWeightAgeM)
svTypF = vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAgeF, plot=TRUE)
svTypM = vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAgeM, plot=TRUE)
#vbStartsDP(fldFishLength ~ fldResult1, data=LengthWeightAgeM, dynamicPlot=TRUE)


had.dat=LengthWeightAge[,c(6,2,3,10)]
names(had.dat)=c("age", "length", "obs.sex", "Year")
library(plyr)
had.dat$obs.sex=revalue(had.dat$obs.sex, c("F"="female", "M"="male", "U"="unclassified"))
dim(had.dat)
had.dat=filter(had.dat, !is.na(age))

binding <- matrix(c(1:2, rep(3, 2), 4:7), ncol = 2, byrow = TRUE)
rownames(binding) <- c("lnlinf", "lnk", "lnnt0", "lnsigma")
colnames(binding) <- c("female", "male")
start.par <- c(c(log(501.9334), log(463.6436)), rep(log(0.3), 1), rep(log(1), 2), 
               rep(log(.1), 2))
start.list <- list(par = list(mixprop = 0.5, growth.par = start.par))
vb.bind.fit <- vb_growth_mix(data = had.dat, start.list = start.list,
                             binding = binding, distribution = "lognormal",
                             reltol = 1e-6, plot.fit=TRUE)

library(tidyverse)
nest_by_year=had.dat %>%
  group_by(Year) %>%
  nest()
fitTyp <- function(df) {
  vb_growth_mix(data = df, start.list = start.list,
                binding = binding, distribution = "lognormal",
                reltol = 1e-6, plot.fit=TRUE)
}
models <- nest_by_year %>%
  mutate(
    model  = data %>% map(fitTyp)
  )

coeffs= models %>% 
  unnest(map(model, coefficients))
