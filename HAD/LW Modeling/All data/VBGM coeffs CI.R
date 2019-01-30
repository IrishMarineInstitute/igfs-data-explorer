library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)
library(FSAsim)
library(tidyverse)
library(broom)
library(lhmixr)
library(tidyverse)
library(RODBC)

channel <- odbcDriverConnect("Driver=SQL Server; Server=VMINFORMDEV01; Database=InformaticsLoad;")
LWA<- sqlQuery(channel,"SELECT * FROM SurveyLenghtWeightAge")
close(channel)



setwd("H:\\IGFS\\IGFS3\\HAD\\LW Modeling\\All data")

LWA_missingremoved=filter(LWA, !is.na(age) & age>=0 & ICESCODE !="VIIa")

vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))

table(LWA_missingremoved$fldMainSpeciesCode)



#Choose species
fish="HAD"
LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode==fish)# for POK remove age <7

#All data
svTyp=vbStarts(length ~ age, data=LWA_fish)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish, start=svTyp)
#create vector with coeff for all data
coeff_all=c(coef(fitTyp)[1],coef(fitTyp)[2],coef(fitTyp)[3])
write_rds(coeff_all, paste0("Species/",fish,"/coeff_all.RDS"))

predictedvalue=vbTyp(4, coeff_all[[1]], coeff_all[[2]], coeff_all[[3]])
#Bootstrap for CI
bootTyp=nlsBoot(fitTyp)

ests=bootTyp$coefboot
#Predicted values
#pv = ests[,"Linf"]*(1-exp(-ests[,"K"]*(4-ests[,"t0"])))
#quantile(pv, c(0.025, 0.975))

#write ests to rds and read back in
write_rds(ests, paste0("Species CI/",fish,"/coeff_CI.RDS"))

#By Year
dfSample = LWA_fish %>% group_by(Year) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
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
AllOptions=expand.grid(Year=c(2003:2017))
coeffs=left_join(AllOptions, coeffs, by=c("Year"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_year.RDS"))


#Confidence interval by year
boot_year= LWA_fish %>% 
  nest(-Year) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                           do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_year, paste0("Species CI/",fish,"/coeff_CI_year.RDS"))



#By Sex
binding <- matrix(c(1:2, rep(3, 2), 4:7), ncol = 2, byrow = TRUE)
rownames(binding) <- c("lnlinf", "lnk", "lnnt0", "lnsigma")
colnames(binding) <- c("female", "male")
start.par <- c(c(log(svTyp[[1]]), log(svTyp[[1]])), rep(log(0.3), 1), rep(log(1), 2), 
               rep(log(.1), 2))
start.list <- list(par = list(mixprop = 0.5, growth.par = start.par))
vb.bind.fit <- vb_growth_mix(data = LWA_fish, start.list = start.list,
                             binding = binding, distribution = "lognormal",
                             reltol = 1e-6, plot.fit=TRUE, 
                             estimate.mixprop=TRUE)
coeff_sex=vb.bind.fit$coefficients
write_rds(coeff_sex, paste0("Species/",fish,"/coeff_sex.RDS"))



#By Sex and by year
vb.bind.fit<- function(df) {
  vb_growth_mix(data = df, start.list = start.list, binding = binding, distribution = "lognormal", 
                reltol = 1e-6, plot.fit=TRUE, 
                estimate.mixprop=TRUE)
}
dfSample = LWA_fish %>% group_by(Year) %>%
  do(fitSample = try(vb.bind.fit(.)))
coeffs=NULL
coeffs$Year=as.numeric(as.character(coeffs$Year))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, cbind("Year"=rep(dfSample$Year[[i]],5),
                               "Parameter"=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"),
                               "Female"=rep(NA,5),
                               "Female.Std.Error"=rep(NA,5), "Male"=rep(NA,5), 
                               "Male.Std.Error"=rep(NA,5)))
  }else{
    coeffs=rbind(coeffs, cbind("Year"=rep(dfSample$Year[[i]],5),
                               dfSample$fitSample[[i]]$coefficients))
  }
}
coeffs$Year=as.numeric(as.character(coeffs$Year))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))
coeffs$Parameter=as.factor(as.character(coeffs$Parameter))
AllOptions=expand.grid(Year=c(2003:2017), Parameter=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"))
coeffs=left_join(AllOptions, coeffs, by=c("Year", "Parameter"))
coeffs
write_rds(coeffs, paste0("Species/",fish,"/coeff_sex_year.RDS"))

#Confidence interval by Sex and year
vb.bind.fit2<- function(df) {
  vb_growth_mix(data = df, start.list = start.list, binding = binding, distribution = "lognormal", 
                reltol = 1e-6, plot.fit=TRUE, 
                estimate.mixprop=TRUE)$coefficients
}

boot_test= LWA_fish %>% 
  nest(-Year) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(fitSample = try(vb.bind.fit2(.))))) %>% 
  unnest(nls_boot)
boot_test2=unnest(boot_test)
write_rds(boot_test2, paste0("Species CI/",fish,"/coeff_CI_year_sex.RDS"))

boot_sex= LWA_fish %>% 
  nest() %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 100) %>% 
                          do(fitSample = try(vb.bind.fit2(.))))) %>% 
  unnest(nls_boot)
boot_sex2=unnest(boot_sex)
write_rds(boot_test2, paste0("Species CI/",fish,"/coeff_CI_year_sex.RDS"))



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
AllOptions=expand.grid(ICESCODE=c("VIa", "VIIb", "VIIc", "VIIg", "VIIj", "VIIk"))
coeffs=left_join(AllOptions, coeffs, by=c("ICESCODE"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_div.RDS"))

LWA_fish2=filter(LWA_fish, ICESCODE !="VIIk")
LWA_fish2=filter(LWA_fish2, ICESCODE !="VIIc")
LWA_fish2$ICESCODE=factor(LWA_fish2$ICESCODE)
table(LWA_fish2$ICESCODE)

boot_div= LWA_fish2 %>% 
  #filter(ICESCODE != "VIIk") %>%
  nest(-ICESCODE) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 2) %>% 
                          do(try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) #%>% 
  #unnest(nls_boot)
write_rds(boot_div, paste0("Species CI/",fish,"/coeff_CI_div.RDS"))


boot_div= LWA_fish %>% 
  nest(-ICESCODE) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_div, paste0("Species CI/",fish,"/coeff_CI_div.RDS"))


boot_div= LWA_fish2 %>% 
  nest(-ICESCODE) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_div, paste0("Species CI/",fish,"/coeff_CI_div.RDS"))


boot_div_year= LWA_fish2 %>% 
  nest(-ICESCODE, -Year) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_div_year, paste0("Species CI/",fish,"/coeff_CI_div_year.RDS"))

LWA_fish_VIa=filter(LWA_fish, ICESCODE %in% c("VIa"))
boot_div_year= LWA_fish_VIa %>% 
  nest(-ICESCODE, -Year) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_div_year, paste0("Species CI/",fish,"/coeff_CI_div_year.RDS"))




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
AllOptions=expand.grid(Year=c(2003:2017), ICESCODE=c("VIa", "VIIb", "VIIc", "VIIg", "VIIj", "VIIk"))
coeffs=left_join(AllOptions, coeffs, by=c("Year", "ICESCODE"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_div_year.RDS"))


#By gear
dfSample = LWA_fish %>% group_by(fldGearDescription) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$fldGearDescription[[i]]), NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$fldGearDescription[[i]]),
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Gear", "Linf", "K", "t0")
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs
write_rds(coeffs, paste0("Species/",fish,"/coeff_gear.RDS"))

#By gear by year
dfSample = LWA_fish %>% group_by(fldGearDescription, Year) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$fldGearDescription[[i]]), dfSample$Year[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$fldGearDescription[[i]]), dfSample$Year[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Gear", "Year", "Linf", "K", "t0")
coeffs$Year=as.numeric(as.character(coeffs$Year))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs
AllOptions=expand.grid(Year=c(2003:2017), Gear=c("GOV 3647 Groundgear A", "GOV 3647 Groundgear D"))
coeffs=left_join(AllOptions, coeffs, by=c("Year", "Gear"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_gear_year.RDS"))

#Confidence intervals
boot_gear= LWA_fish %>% 
  nest(-fldGearDescription) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_gear, paste0("Species CI/",fish,"/coeff_CI_gear.RDS"))

boot_gear_year= LWA_fish %>% 
  nest(-fldGearDescription, -Year) %>% 
  mutate(nls_boot = map(data, ~ bootstrap(., 1000) %>% 
                          do(tidy(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp))))) %>% 
  unnest(nls_boot)
write_rds(boot_gear_year, paste0("Species CI/",fish,"/coeff_CI_gear_year.RDS"))

##################################################
##################################################
# Cohort
##################################################
##################################################

#By Year
dfSample = LWA_fish %>% group_by(Cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
print(tbl_df(dfSample), n=30)

coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(dfSample$Cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(dfSample$Cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Cohort", "Linf", "K", "t0")
coeffs=filter(coeffs, Cohort>2002)
AllOptions=expand.grid(Cohort=c(2003:2017))
coeffs=left_join(AllOptions, coeffs, by=c("Cohort"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_cohort.RDS"))

#cohort by division
dfSample = LWA_fish %>% group_by(ICESCODE, Cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$ICESCODE[[i]]), dfSample$Cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$ICESCODE[[i]]), dfSample$Cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("ICESCODE", "Cohort", "Linf", "K", "t0")
coeffs$Cohort=as.numeric(as.character(coeffs$Cohort))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs=filter(coeffs, Cohort>2002)
AllOptions=expand.grid(Cohort=c(2003:2017), ICESCODE=c("VIa", "VIIb", "VIIc", "VIIg", "VIIj", "VIIk"))
coeffs=left_join(AllOptions, coeffs, by=c("Cohort", "ICESCODE"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_cohort_div.RDS"))


#cohort by gear
dfSample = LWA_fish %>% group_by(fldGearDescription, Cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$fldGearDescription[[i]]), dfSample$Cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$fldGearDescription[[i]]), dfSample$Cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Gear", "Cohort", "Linf", "K", "t0")
coeffs$Cohort=as.numeric(as.character(coeffs$Cohort))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs
coeffs=filter(coeffs, Cohort>2002)
AllOptions=expand.grid(Cohort=c(2003:2017), Gear=c("GOV 3647 Groundgear A", "GOV 3647 Groundgear D"))
coeffs=left_join(AllOptions, coeffs, by=c("Cohort", "Gear"))
write_rds(coeffs, paste0("Species/",fish,"/coeff_gear_cohort.RDS"))


#By sex
binding <- matrix(c(1:2, rep(3, 2), 4:7), ncol = 2, byrow = TRUE)
rownames(binding) <- c("lnlinf", "lnk", "lnnt0", "lnsigma")
colnames(binding) <- c("female", "male")
start.par <- c(c(log(svTyp[[1]]), log(svTyp[[1]])), rep(log(0.3), 1), rep(log(1), 2), 
               rep(log(.1), 2))
start.list <- list(par = list(mixprop = 0.5, growth.par = start.par))

vb.bind.fit<- function(df) {
  vb_growth_mix(data = df, start.list = start.list, binding = binding, distribution = "lognormal", 
                reltol = 1e-6, plot.fit=TRUE, 
                estimate.mixprop=TRUE)
}

dfSample = LWA_fish %>% group_by(Cohort) %>%
  filter(Cohort>2002) %>%
  do(fitSample = try(vb.bind.fit(.)))
coeffs=NULL
coeffs$Cohort=as.numeric(as.character(coeffs$Cohort))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, cbind("Cohort"=rep(dfSample$Cohort[[i]],5),
                               "Parameter"=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"),
                               "Female"=rep(NA,5),
                               "Female.Std.Error"=rep(NA,5), "Male"=rep(NA,5), 
                               "Male.Std.Error"=rep(NA,5)))
  }else{
    coeffs=rbind(coeffs, cbind("Cohort"=rep(dfSample$Cohort[[i]],5),
                               dfSample$fitSample[[i]]$coefficients))
  }
}
coeffs$Cohort=as.numeric(as.character(coeffs$Cohort))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))
AllOptions=expand.grid(Cohort=c(2003:2017), Parameter=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"))
coeffs=left_join(AllOptions, coeffs, by=c("Cohort", "Parameter"))

write_rds(coeffs, paste0("Species/",fish,"/coeff_sex_cohort.RDS"))


