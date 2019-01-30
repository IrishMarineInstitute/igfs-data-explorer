library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)
library(FSAsim)
library(tidyverse)
library(broom)
library(lhmixr)

setwd("H:\\IGFS\\IGFS3\\HAD\\LW Modeling\\All data")

LengthWeightAge=readRDS("LengthWeightAge.RDS")

LWA_missingremoved=filter(LengthWeightAge, !is.na(fldResult1) & fldResult1>=0 & ICESCODE !="VIIa")
names(LWA_missingremoved)[2]="length"
names(LWA_missingremoved)[6]="age"
names(LWA_missingremoved)[3]="obs.sex"
library(plyr)
LWA_missingremoved$obs.sex=revalue(LWA_missingremoved$obs.sex, c("F"="female", "M"="male", "U"="unclassified"))
LWA_missingremoved$length=LWA_missingremoved$length/10+0.5
LWA_missingremoved$cohort=LWA_missingremoved$Year-LWA_missingremoved$age

vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))

table(LWA_missingremoved$fldMainSpeciesCode)



#Choose species
fish="HAD"
LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode==fish)

#All data
svTyp=vbStarts(length ~ age, data=LWA_fish)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish, start=svTyp)
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
#nam <- paste("coeff_sex_", fish, sep = "")
#assign(nam, vb.bind.fit$coefficients)

#source("VB_fn.R")
vb.bind.fit <-VB_fn(data = LWA_fish, start.list = start.list,
              binding = binding, distribution = "lognormal",
              reltol = 1e-6, plot.fit=TRUE, 
              estimate.mixprop=TRUE)

coeff_sex=vb.bind.fit$coefficients
write_rds(coeff_sex, paste0("Species/",fish,"/coeff_sex.RDS"))

alldataF=data.frame(Data= paste("All Female data"), 
                    Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Female"]),
                    K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Female"]), 
                    t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Female"]))
alldataM=data.frame(Data= paste("All Male data"), 
                    Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Male"]),
                    K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Male"]), 
                    t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Male"]))

x=seq(0, max(LWA_fish$age), length.out = 199)
predicted=data.frame(age=x, 
                     predlengthF= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Female"]),
                                        K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Female"]), 
                                        t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Female"])),
                     predlengthM= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Male"]),
                                        K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Male"]), 
                                        t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Male"])))
                     
p=plot_ly() %>% 
  add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
            color = ~obs.sex, colors=c('unclassified'='#999999','male'='#6699ff','female'='#ff66cc'),
            text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Sex:", obs.sex),
            hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
  layout(hovermode="closest", title=paste("Length vs Age (points coloured by sex)"),
         xaxis = list(title = 'Age (years)', zeroline=FALSE,
                      range= c(min(LWA_fish$age)-1,max(LWA_fish$age)+1)),
         yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                      range = c(0, max(LWA_fish$length, na.rm = T)*1.05)),
         margin=(list(t=70)), showlegend = TRUE)%>%
  add_trace(data=predicted, x = ~age, y = ~predlengthF, type="scatter", mode = "lines",
            line = list(shape="spline", color="#ff66cc"), name=paste("Female data fit"), hoverinfo="none")%>%
  add_trace(data=predicted, x = ~age, y = ~predlengthM, type="scatter", mode = "lines",
            line = list(shape="spline", color="#6699ff"), name=paste("Male data fit"), hoverinfo="none")


fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_F, start=svTypF)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_M, start=svTypM)


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
coeffs=c()
for(i in 1:dim(models)[1]){
  year=cbind(models$Year[i], models$fitTypcoeffs[[i]]$coefficients)
  coeffs=rbind(coeffs, year)
}
#nam <- paste("coeff_sex_year_", fish, sep = "")
#assign(nam, coeffs)
coeff_sex_year=coeffs
write_rds(coeff_sex_year, paste0("Species/",fish,"/coeff_sex_year.RDS"))

dfSample = LWA_fish %>% group_by(Year) %>%
  do(fitSample = try(vb.bind.fit(.)))
coeffs=c()
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, cbind("models$Year[i]"=rep(dfSample$Year[[i]],5),
                               "Parameter"=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"),
                               "Female"=rep(NA,5),
                               "Female.Std.Error"=rep(NA,5), "Male"=rep(NA,5), 
                               "Male.Std.Error"=rep(NA,5)))
  }else{
    coeffs=rbind(coeffs, cbind("models$Year[i]"=rep(dfSample$Year[[i]],5),
                               dfSample$fitSample[[i]]$coefficients))
  }
}
coeffs$'models$Year[i]'=as.numeric(as.character(coeffs$'models$Year[i]'))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))

write_rds(coeffs, paste0("Species/",fish,"/coeff_sex_year.RDS"))

#By Division
dfSample = LWA_fish %>% group_by(ICESCODE) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))

coeffs=c()
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
coeffs=c()
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


#By gear
dfSample = LWA_fish %>% group_by(fldGearDescription) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))

coeffs=c()
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

#By gear by year
dfSample = LWA_fish %>% group_by(fldGearDescription, Year) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=c()
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

write_rds(coeffs, paste0("Species/",fish,"/coeff_gear_year.RDS"))


##################################################
##################################################
# Cohort
##################################################
##################################################

#By Year

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

#cohort by division
dfSample = LWA_fish %>% group_by(ICESCODE, cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$ICESCODE[[i]]), dfSample$cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$ICESCODE[[i]]), dfSample$cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("ICESCODE", "cohort", "Linf", "K", "t0")
coeffs$cohort=as.numeric(as.character(coeffs$cohort))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs


#cohort by gear
dfSample = LWA_fish %>% group_by(fldGearDescription, cohort) %>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=., start=svTyp)))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(as.character(dfSample$fldGearDescription[[i]]), dfSample$cohort[[i]], NA, NA, NA))
  }else{
    coeffs=rbind(coeffs, cbind(as.character(dfSample$fldGearDescription[[i]]), dfSample$cohort[[i]],
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]]))
  }
}
coeffs=as.data.frame(coeffs)
colnames(coeffs)=c("Gear", "cohort", "Linf", "K", "t0")
coeffs$cohort=as.numeric(as.character(coeffs$cohort))
coeffs$Linf=as.numeric(as.character(coeffs$Linf))
coeffs$K=as.numeric(as.character(coeffs$K))
coeffs$t0=as.numeric(as.character(coeffs$t0))
coeffs

write_rds(coeffs, paste0("Species/",fish,"/coeff_gear_cohort.RDS"))


#By sex
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

vb.bind.fit<- function(df) {
  vb_growth_mix(data = df, start.list = start.list, binding = binding, distribution = "lognormal", 
                reltol = 1e-6, plot.fit=TRUE, 
                estimate.mixprop=TRUE)
}


dfSample = LWA_fish %>% group_by(cohort) %>%
  do(fitSample = try(vb.bind.fit(.)))
coeffs=c()
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, cbind("cohort"=rep(dfSample$cohort[[i]],5),
                               "Parameter"=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"),
                               "Female"=rep(NA,5),
                               "Female.Std.Error"=rep(NA,5), "Male"=rep(NA,5), 
                               "Male.Std.Error"=rep(NA,5)))
  }else{
    coeffs=rbind(coeffs, cbind("cohort"=rep(dfSample$cohort[[i]],5),
                               dfSample$fitSample[[i]]$coefficients))
  }
}
coeffs$cohort=as.numeric(as.character(coeffs$cohort))
coeffs$Female=as.numeric(as.character(coeffs$Female))
coeffs$Male=as.numeric(as.character(coeffs$Male))

write_rds(coeffs, paste0("Species/",fish,"/coeff_sex_cohort.RDS"))


