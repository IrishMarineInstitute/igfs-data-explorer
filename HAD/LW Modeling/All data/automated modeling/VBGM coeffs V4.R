library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)
library(FSAsim)
library(tidyverse)
library(broom)
library(lhmixr)

setwd("H:\\IGFS\\IGFS3\\HAD\\LW Modeling\\All data\\automated modeling")

library(RODBC)
channel <- odbcDriverConnect("Driver=SQL Server; Server=VMINFORMDEV01; Database=InformaticsLoad;")
LengthWeightAge=sqlQuery(channel,"Select * from SurveyLenghtWeightAge")
close(channel)

LWA_missingremoved=filter(LengthWeightAge, !is.na(age) & age>=0 & ICESCODE !="VIIa")

vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))

table(LWA_missingremoved$fldMainSpeciesCode)



#Choose species
fish="HAD"
LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode==fish)

#All data
svTyp=vbStarts(length ~ age, data=LWA_fish)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish, start=svTyp)
summary(fitTyp)
#create vector with coeff for all data
coeff_all=c(coef(fitTyp)[1],coef(fitTyp)[2],coef(fitTyp)[3])
write_rds(coeff_all, paste0("Species/",fish,"/coeff_all.RDS"))

#By Year
dfSample = LWA_fish %>% group_by(Year) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=c()
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(dfSample$Year[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(dfSample$Year[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Year", "Linf", "K", "t0")
coeffs
write_rds(coeffs, paste0("Species/",fish,"/coeff_year.RDS"))


#By Sex
LWA_F=filter(LWA_fish, obs.sex=="female")
dim(LWA_F)
LWA_M=filter(LWA_fish, obs.sex=="male")# & age<9)
dim(LWA_M)
svTypF = vbStarts(length ~ age, data=LWA_F, plot=TRUE)
svTypM = vbStarts(length ~ age, data=LWA_M, plot=TRUE)

binding <- matrix(c(1:2, rep(3, 2), 4:7), ncol = 2, byrow = TRUE)
rownames(binding) <- c("lnlinf", "lnk", "lnnt0", "lnsigma")
colnames(binding) <- c("female", "male")
start.par <- c(c(log(svTypF[[1]]), log(svTypM[[1]])), rep(log(0.3), 1), rep(log(1), 2), 
               rep(log(.1), 2))
start.list <- list(par = list(mixprop = 0.5, growth.par = start.par))
vb.bind.fit <- vb_growth_mix(data = LWA_fish, start.list = start.list,
                             binding = binding, distribution = "lognormal",
                             reltol = 1e-6, plot.fit=TRUE, 
                             estimate.mixprop=TRUE)
coeff_sex=vb.bind.fit$coefficients
coeff_sex
write_rds(coeff_sex, paste0("Species/",fish,"/coeff_sex.RDS"))

dfSample = LWA_fish %>% group_by(Year) %>%
  do(vb.bind.fit = try(vb_growth_mix(data = ., start.list = start.list,
                                                  binding = binding, distribution = "lognormal",
                                                  reltol = 1e-6, plot.fit=TRUE, 
                                                  estimate.mixprop=TRUE)
  ))




#By Sex and by year
nest_by_year=LWA_fish %>%
  group_by(Year) %>%
  nest()
vb.bind.fit<- function(df) {
  vb_growth_mix(data = df, start.list = start.list, binding = binding, distribution = "lognormal", 
                reltol = 1e-6, plot.fit=TRUE, 
                estimate.mixprop=TRUE)
}
models <- nest_by_year %>%
  mutate(
    fitTypcoeffs  = data %>% map(vb.bind.fit)
  )
coeffs=NULL
for(i in 1:dim(models)[1]){
  year=cbind(models$Year[i], models$fitTypcoeffs[[i]]$coefficients)
  coeffs=rbind(coeffs, year)
}
#nam <- paste("coeff_sex_year_", fish, sep = "")
#assign(nam, coeffs)
coeff_sex_year=coeffs
write_rds(coeff_sex_year, paste0("Species/",fish,"/coeff_sex_year.RDS"))


#By Division
dfSample = LWA_fish %>% group_by(ICESCODE) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))

coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$ICESCODE[[i]]), NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$ICESCODE[[i]]),
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("ICESCODE", "Linf", "K", "t0")
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs
write_rds(coeffs, paste0("Species/",fish,"/coeff_div.RDS"))


#By Division and year
dfSample = LWA_fish %>% group_by(ICESCODE, Year) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$ICESCODE[[i]]), dfSample$Year[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$ICESCODE[[i]]), dfSample$Year[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("ICESCODE", "Year", "Linf", "K", "t0")
coeffs$Year=as.numeric(as.character(coeffs$Year))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs


write_rds(coeffs, paste0("Species/",fish,"/coeff_div_year.RDS"))

#By Cohort
dfSample = LWA_fish %>% group_by(cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
print(tbl_df(dfSample), n=30)

coeffs=c()
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(dfSample$cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(dfSample$cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("cohort", "Linf", "K", "t0")
coeffs
write_rds(coeffs, paste0("Species/",fish,"/coeff_cohort.RDS"))

#