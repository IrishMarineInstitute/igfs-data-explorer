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
#LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode==fish & age<7) #POK filter
#Cod filter
#LWA_fish=LWA_fish[-which(LWA_fish$Year==2009 & LWA_fish$age==4),]

#All data
svTyp=vbStarts(length ~ age, data=LWA_fish)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish, start=svTyp)
#create vector with coeff for all data
nam <- paste("coeff_all_", fish, sep = "")
assign(nam, c(coef(fitTyp)[1],coef(fitTyp)[2],coef(fitTyp)[3]))
write_rds(coeff_all_HAD, paste0("Species/",fish,"/coeff_all.RDS"))

#By Year
nest_by_year=LWA_fish %>%
  #filter(Year !=2004) %>% # Meg filter
  #filter(Year !=2003) %>% # POK filter
  group_by(Year) %>%
  nest()
fitTyp <- function(df) {
  nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp,control = nls.control(maxiter = 1000))
}
models <- nest_by_year %>%
  mutate(
    fitTypcoeffs  = data %>% map(fitTyp)
  )
coeffs=models %>% 
  unnest(map(fitTypcoeffs, tidy))
nam <- paste("coeff_year_", fish, sep = "")
assign(nam, coeffs)
write_rds(coeff_year_HAD, paste0("Species/",fish,"/coeff_year.RDS"))

#YearCheck for Cod
LWA_fish_Year=filter(LWA_fish, Year!=2009)
#dim(LWA_fish_Year)
nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish_Year, start=svTyp,control = nls.control(maxiter = 1000))




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
  #filter(!(Year %in% c(2007, 2010, 2011, 2015, 2016))) %>% #filter for Herring
  #filter(Year != 2004) %>% # filter for POK
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
LWA_fish_div= filter(LWA_fish, ICESCODE=="VIa")
nest_by_division=LWA_fish %>%
  group_by(ICESCODE) %>%
  nest()
m=c()
for(i in 1:dim(nest_by_division)[1]){
  if(dim(nest_by_division$data[[i]])[1]<21){
    m=c(m,i)
  }
}

nest_by_division=nest_by_division[-m,]

fitTyp <- function(df) {
  nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp)
}
models <- nest_by_division %>%
  mutate(
    fitTypcoeffs  = data %>% map(fitTyp)
  )
coeffs=models %>% 
  unnest(map(fitTypcoeffs, tidy))
nam <- paste("coeff_div_", fish, sep = "")
assign(nam, coeffs)
write_rds(coeff_div_WHG, paste0("Species/",fish,"/coeff_div.RDS"))

#By Divs by Year
nest_by_division=LWA_fish %>%
  #filter(!(Year %in% c(2007, 2010, 2011, 2015, 2016))) %>% # filter for HER
  #filter(!(ICESCODE %in% c("VIIc", "VIIk"))) %>% # filter for HOM
  #filter(!(ICESCODE %in% c("VIIc", "VIIj", "VIIg")) & age<12) %>% # filter for PLE
  #filter(Year >2003) %>% # filter for MEG
  filter(ICESCODE !="VIIc") %>% # filter for HAD
  group_by(ICESCODE, Year) %>%
  nest()
#Check sample Sizes
print(tbl_df(nest_by_division), n=76)
#Remove small samples in 
nest_by_division_year=c(0)
m=c()
for(i in 1:dim(nest_by_division)[1]){
  if(dim(nest_by_division$data[[i]])[1]<20){
    m=c(m,i)
    }
}
nest_by_division_year=nest_by_division[-m,]
print(tbl_df(nest_by_division_year), n=72)


fitTyp <- function(df) {
  nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp)
}
models <- nest_by_division_year %>%
  mutate(
    fitTypcoeffs  = data %>% map(fitTyp)
  )
coeffs=models %>% 
  unnest(map(fitTypcoeffs, tidy))
nam <- paste("coeff_div_year_", fish, sep = "")
assign(nam, coeffs)
write_rds(coeff_div_year_HAD, paste0("Species/",fish,"/coeff_div_year.RDS"))


dfSample = LWA_fish %>% group_by(ICESCODE, Year) %>% nest() %>%
  do(fitSample = try(fitTyp(df)))


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

substring(dfSample$fitSample[[9]][1],1,5)

test=dfSample[which(dfSample$ICESCODE=="VIIc"),]

coeffs=dfSample %>% 
  unnest(map(fitSample, tidy))
#print(tbl_df(coeffs), n=200)
coeffs[which(coeffs$Year==2017),]
coeff_div_year_HAD[which(coeff_div_year_HAD$Year==2017),]
coeff_div_year=readRDS("Species/HAD/coeff_div_year.RDS")
coeff_div_year[which(coeff_div_year$Year==2017),]


elim = "Error in nlsModel"
dfSamplei = subset(dfSample, !grepl(paste(elim), dfSample$fitSample))

dfSampleCoef = tidy(dfSamplei %>% rowwise(), fitSample)
#tidy the outcome
dfSampleCoef = tidy(dfSamplei, fitSample)


coeff=c(0, 0, 0)


#Which i is failing
for(i in 36:dim(nest_by_division_year)[1]){
  Model=fitTyp(nest_by_division_year$data[[i]])
  coeff=c(coef(Model)[1],coef(Model)[2],coef(Model)[3])
}
i
#8  VIa and 2005
#31 VIIc and 2009
#39 VIIj and 2011
nest_by_division_year=nest_by_division_year[-c(8,35),]

33

c(coef(fitTyp)[1],coef(fitTyp)[2],coef(fitTyp)[3])

#Plots
x=seq(0, max(LWA_fish$age), length.out = 199)
predicted=data.frame(age=x, predlengthF= vbTyp(x, Linf=exp(3.90481537), K=exp(-0.86816554), t0=-exp(-0.06363577)),
                     predlengthM= vbTyp(x, Linf=exp(3.6828925), K=exp(-0.8681655), t0=-exp(0.3920670)),
                     predlengthAll= vbTyp(x, Linf=46.8569055, K=0.3869193, t0=-1.1829791))

ggplot(LWA_fish, aes(x=age, y=length, color=factor(obs.sex)))+geom_point()+
  facet_wrap(~Year)
  scale_x_discrete(limits = c(-0.1, max(LWA_fish$age)))+
  scale_y_continuous(limits = c(min(LWA_fish$length), 
                                max(LWA_fish$length)+1))+
  labs(x = "Age", y="Length")+ theme_bw()+
  geom_line(color='red',data = predicted, aes(x=age, y=predlengthF), size=1)+
  geom_line(color='blue',data = predicted, aes(x=age, y=predlengthM), size=1)+
  geom_line(color='green',data = predicted, aes(x=age, y=predlengthAll), size=1)

ggplot(LWA_fish, aes(x=age, y=length))+geom_point()+facet_grid(Year ~ICESCODE)
ggplot(LWA_fish, aes(x=age, y=length))+geom_point()+facet_wrap(~Year)
ggplot(LWA_fish, aes(x=age, y=length, color=obs.sex))+geom_point() +geom_jitter()+facet_wrap(~Year)

ggplot(LWA_fish, aes(x=age, y=length))+geom_point()+facet_grid(Year ~ obs.sex)

library(plotly)
plot_ly() %>% 
  add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
            color = ~obs.sex, colors=c('unclassified'='#999999','male'='#6699ff','female'='#ff66cc'),
            text=~paste("length:",length,"cm","<br>age:",age),
            hoverinfo = 'text',marker =list(opacity = 0.5)) %>% 
  layout(hovermode="closest", title=paste("Length vs Weight (points coloured by sex)"),
         xaxis = list(title = 'Age (years)', zeroline=FALSE,
                      range= c(min(LWA_fish$age)-1,max(LWA_fish$age)+1)),
         yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                      range = c(0, max(LWA_fish$length, na.rm = T)*1.05)),
         margin=(list(t=70)), showlegend = TRUE) %>%
  add_trace(data=predicted, x = ~age, y = ~predlengthAll, type="scatter", mode = "lines",
            line = list(shape="spline", color="grey"), name="All data fit", hoverinfo="none") %>%
  add_trace(data=predicted, x = ~age, y = ~predlengthM, type="scatter", mode = "lines",
            line = list(shape="spline", color="#6699ff"), name="Male data fit", hoverinfo="none") %>%
  add_trace(data=predicted, x = ~age, y = ~predlengthF, type="scatter", mode = "lines",
            line = list(shape="spline", color="#ff66cc"), name="Female data fit", hoverinfo="none")

#######################
#### Cohort models ####
#######################
svTyp=vbStarts(length ~ age, data=LWA_fish)

ggplot(LWA_fish, aes(x=age, y=length))+geom_point()+facet_wrap(~Year)

LWA_fish_year=filter(LWA_fish, cohort==2015)
fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish_year, start=svTyp)
overview(fitTyp)
bootTypical <- nlsBoot(fitTyp,niter=200) 
confint(bootTypical,plot=TRUE)

nest_by_cohort=LWA_fish %>%
  filter(cohort %in% c(2003:2015))%>%
  group_by(cohort) %>%
  nest()
print(tbl_df(nest_by_cohort), n=76)

#nest_by_cohort=c(0)
m=c()
for(i in 1:dim(nest_by_cohort)[1]){
  if(dim(nest_by_cohort$data[[i]])[1]<254){
    m=c(m,i)
  }
}
nest_by_cohort=nest_by_cohort[-m,]
print(tbl_df(nest_by_cohort), n=72)

fitTyp <- function(df) {
  nls(length ~ vbTyp(age, Linf, K, t0), data=df, start=svTyp,control = nls.control(maxiter = 1000))
}
models <- nest_by_cohort %>%
  mutate(
    fitTypcoeffs  = data %>% map(fitTyp)#,
    #bootTypical = fitTypcoeffs %>% nlsBoot(niter=200) 
  )
coeffs=models %>% 
  unnest(map(fitTypcoeffs, tidy))
nam <- paste("coeff_year_", fish, sep = "")
assign(nam, coeffs)
write_rds(coeff_year_HAD, paste0("Species/",fish,"/coeff_year.RDS"))




##################
#### No Data #####
coeff_all=c(NA,NA,NA)
names(coeff_all)=c("Linf", "K", "t0")
write_rds(coeff_all, paste0("Species/Missing/coeff_all.RDS"))

#By Year
coeff_year=expand.grid(Year=c(2003:2017), term=c("Linf", "K", "t0"), estimate=NA, std.error=NA, statistic=NA, p.value=NA)
write_rds(coeff_year, paste0("Species/Missing/coeff_year.RDS"))

#By Sex
coeff_sex=expand.grid(Parameter=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"), 
                       Female=NA, Female.Std.Error=NA, Male=NA, Male.Std.Error=NA)
write_rds(coeff_sex, paste0("Species/Missing/coeff_sex.RDS"))

#By Sex and year
coeff_sex_year=expand.grid('models$Year[i]'=c(2003:2017), Parameter=c("lnlinf", "lnk", "lnnt0", "lnsigma", "logitpi"), 
                      Female=NA, Female.Std.Error=NA, Male=NA, Male.Std.Error=NA)
write_rds(coeff_sex_year, paste0("Species/Missing/coeff_sex_year.RDS"))

#By Division
coeff_div=expand.grid(ICESCODE=c("VIa", "VIIb", "VIIc", "VIIg", "VIIj", "VIIk"), 
                      term=c("Linf", "K", "t0"), estimate=NA, std.error=NA, statistic=NA, p.value=NA)
saveRDS(coeff_div, paste0("Species/Missing/coeff_div.RDS"))

#By Divsion and year
coeff_div_year=expand.grid(ICESCODE=c("VIa", "VIIb", "VIIc", "VIIg", "VIIj", "VIIk"), Year=c(2003:2017),
                      term=c("Linf", "K", "t0"), estimate=NA, std.error=NA, statistic=NA, p.value=NA)
saveRDS(coeff_div_year, paste0("Species/Missing/coeff_div_year.RDS"))

#By cohort
coeff_cohort=expand.grid(cohort=c(2003:2017), term=c("Linf", "K", "t0"), estimate=NA, std.error=NA, statistic=NA, p.value=NA)
saveRDS(coeff_cohort, paste0("Species/Missing/coeff_cohort.RDS"))



####################
#### Haddock VIIj specific
LWA_fish_year=filter(LWA_fish, Year==2017 & ICESCODE=="VIa")
ggplot(LWA_fish_year, aes(x=age, y=length))+geom_point()

fitTyp=nls(length ~ vbTyp(age, Linf, K, t0), data=LWA_fish_year, start=svTyp)
overview(fitTyp)


bootTypical <- nlsBoot(fitTyp,niter=200)
new <- data.frame(age=4)
predict(fitTyp, new)
ests <- bootTypical$coefboot
pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(8-ests[,"t0"])))
quantile(pv,c(0.025,0.975))

ages2plot=0:9
fitPlot(fitTyp,xlab="Age",ylab="Total Length (mm)",xlim=range(ages2plot),main="")
LCI <- UCI <- numeric(length(ages2plot))
for (i in 1:length(ages2plot)) {
  pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(ages2plot[i]-ests[,"t0"])))
  LCI[i] <- quantile(pv,0.025)
  UCI[i] <- quantile(pv,0.975)
}
lines(UCI~ages2plot,type="l",col="blue",lwd=2,lty=2)
lines(LCI~ages2plot,type="l",col="blue",lwd=2,lty=2)
