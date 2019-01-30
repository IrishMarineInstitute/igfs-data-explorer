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

#Weights
calweight=table(LWA_fish$age)/dim(LWA_fish)[1]

LWA_fish$weights=NA
for(i in 1:dim(LWA_fish)[1]){
  LWA_fish$weights[i]=calweight[which(names(calweight)==LWA_fish$age[i])][[1]]
}

#LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode==fish & age<7) #POK filter
#Cod filter
#LWA_fish=LWA_fish[-which(LWA_fish$Year==2009 & LWA_fish$age==4),]

#All data
svTyp=vbStarts(length ~ age, data=LWA_fish)

LWA_fish_year=filter(LWA_fish, Year==2017 & ICESCODE=="VIIj")
calweight=table(LWA_fish_year$age)/dim(LWA_fish_year)[1]

LWA_fish_year$weights=NA
for(i in 1:dim(LWA_fish_year)[1]){
  LWA_fish_year$weights[i]=calweight[which(names(calweight)==LWA_fish_year$age[i])][[1]]
}

fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish_year, start=svTyp)
coef(fitTyp)
fitTypweights=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish_year, start=svTyp, weights=weights)
coef(fitTypweights)
GAMfit=gam(length ~ s(age, bs="cr", k=5), data=LWA_fish_year)

anova(fitTyp, GAMfit)
anova(fitTypweights, GAMfit)

x=seq(0, max(LWA_fish_year$age), length.out = 199)
predicted=data.frame(age=x, 
                     fitpred=vbTyp(x, Linf= coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]),
                     fitweightpred=vbTyp(x, Linf= coef(fitTypweights)[[1]], K=coef(fitTypweights)[[2]], 
                                         t0=coef(fitTypweights)[[3]]))

fits = predict(GAMfit, newdata=LWA_fish_year, type='response', se=T)
predicts = data.frame(LWA_fish_year, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

plot_mod_gam2_response = ggplot(predicts, aes(x=age, y=length))+geom_point()+
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(aes(x=age,y=fit),color='#1e90ff') +
  theme_bw()+
  geom_line(color='red',data = predicted, aes(x=age, y=fitpred), size=1)+
  geom_line(color='green',data = predicted, aes(x=age, y=fitweightpred), size=1)
plot_mod_gam2_response




#create vector with coeff for all data
nam <- paste("coeff_all_", fish, sep = "")
assign(nam, c(coef(fitTyp)[1],coef(fitTyp)[2],coef(fitTyp)[3]))
write_rds(coeff_all_HAD, paste0("Species/",fish,"/coeff_all.RDS"))

AIC(fitTyp)
library(mgcv)
mod_lm <- gam(length ~ age, data=LWA_fish)
summary(mod_lm)
mod_gam1 <- gam(length ~ s(age, bs="cr"), data=LWA_fish)
summary(mod_gam1)
plot(mod_gam1)
AIC(mod_gam1)
anova(fitTyp, mod_gam1, test="Chisq")

nest_by_year=LWA_fish %>%
  group_by(Year) %>%
  nest()
fitTyp <- function(df) {
  nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp,control = nls.control(maxiter = 1000))
}
fitGam =function(df){
  gam(length ~ s(age, bs="cr", k=5), data=df)
}
models <- nest_by_year %>%
  mutate(
    fitTypcoeffs = data %>% map(fitTyp),
    fitGamcoeffs = data %>% map(fitGam)#,
#    AICcomp = anova(fitTypcoeffs, fitGamcoeffs, test="Chisq")
  )

GAMvsVBGM=function(df){
  anova(nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp,control = nls.control(maxiter = 1000)),
        gam(length ~ s(age, bs="cr", k=5), data=df), test="Chisq")
}
modelsAICComp <- nest_by_year %>%
  mutate(
    AICcomp = data %>% map(GAMvsVBGM)
  )


fitTyp <- function(df) {
  AIC(nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp,control = nls.control(maxiter = 1000)))
}
fitGam =function(df){
  AIC(gam(length ~ s(age, bs="cr", k=5), data=df))
}


glance <- models %>% 
  mutate(glanceTyp = map(fitTypcoeffs, broom::glance)) %>% 
  mutate(glanceGam = map(fitGamcoeffs, broom::glance))# %>%
  #unnest(glance, .drop = TRUE)
glance

test=LWA_fish %>%
  nest(-Year) %>% 
  mutate(fit = map(data, ~ nls(length ~ vbTyp(age, Linf, K, t0), data=., 
                               start=svTyp,control = nls.control(maxiter = 1000))),
         results = map(fit, broom::glance),
         GAMfit = map(data, ~gam(length ~ s(age, bs="cr", k=5), data=.)),
         resultsGAM = map(GAMfit, broom::glance),
         AICfit = map(fit, AIC),
         AICGAMfit= map(GAMfit, AIC),
         AICcomp = map(data, GAMvsVBGM)#,
         #resultsComp = map(AICcomp, broom::glance)
         )%>% 
  unnest(AICfit, AICGAMfit, AICcomp, .drop = TRUE)
  unnest(AICcomp)#, .drop = TRUE)


mtcars %>% 
  nest(-cyl) %>% 
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, broom::glance)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = factor(cyl), y = r.squared)) +
  geom_bar(stat = "identity") +
  labs(x = "Cylinders", y = expression(R^{2}))


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
LWA_M=filter(LWA_fish, obs.sex=="male"& age<10) # remove for haddock
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
nam <- paste("coeff_sex_", fish, sep = "")
assign(nam, vb.bind.fit$coefficients)
write_rds(coeff_sex_HAD, paste0("Species/",fish,"/coeff_sex.RDS"))


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
nam <- paste("coeff_sex_year_", fish, sep = "")
assign(nam, coeffs)
write_rds(coeff_sex_year_WHG, paste0("Species/",fish,"/coeff_sex_year.RDS"))


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

