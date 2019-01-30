LengthWeightAge=readRDS("LengthWeightAge.RDS")

if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github('droglenc/FSAsim')

library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)
library(FSAsim)


svTyp = vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge, plot=TRUE)

vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge, dynamicPlot=TRUE)
vbStartsDP(fldFishLength ~ fldResult1, data=LengthWeightAge, dynamicPlot=TRUE)

svTyp = list(Linf=max(LengthWeightAge$fldFishLength, na.rm=TRUE), K=0, t0=0)


#Function to predict the mean length of a fish at age
vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))

#values from book
vbTyp(3, Linf=1200, K=0.13, t0=-2.0)

vbTyp(3, Linf=500, K=0.36, t0=-1.17)
vbTyp(2, Linf=500, K=0.36, t0=-1.17)


fitTyp=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAge, start=svTyp)
coef(fitTyp)
confint(fitTyp)

bootTyp = nlsBoot(fitTyp)
headtail(bootTyp, n=2)
confint(bootTyp, plot=TRUE)


x=seq(0, 15, length.out = 199)
pTyp=vbTyp(x, Linf=coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]])
xlmts=range(c(x, LengthWeightAge$fldResult1),na.rm=TRUE)
ylmts=range(c(pTyp, LengthWeightAge$fldFishLength))
plot(fldFishLength ~ fldResult1, data=LengthWeightAge, xlim=xlmts, ylim=ylmts)
lines(pTyp~x, lwd=2)

residPlot(fitTyp)

LengthWeightAge %<>% mutate(logTL=log(fldFishLength))
fitTypM =nls(logTL ~ log(vbTyp(fldResult1, Linf, K, t0)), data=LengthWeightAge, start=svTyp)
residPlot(fitTypM)

#compareAdditive and multiplicative arror structures
 

#nested

library(tidyverse)
LengthWeightAge_missingremoved=filter(LengthWeightAge, !is.na(fldResult1) & fldResult1>=0)

#distribution of ages
species=unique(factor(LengthWeightAge_missingremoved$fldMainSpeciesCode))
LengthWeightAge_speciesage=filter(LengthWeightAge, fldMainSpeciesCode %in% species & fldResult1>=0)
table(factor(LengthWeightAge_speciesage$fldMainSpeciesCode), LengthWeightAge_speciesage$fldResult1, useNA = "always")

library(ggridges)
ggplot(LengthWeightAge_speciesage, aes(x = fldResult1, y = fldMainSpeciesCode))+
  geom_density_ridges(alpha = .5)



#numbers across years
table(factor(LengthWeightAge_speciesage$fldMainSpeciesCode), LengthWeightAge_speciesage$Year, useNA = "always")



nest_by_species=LengthWeightAge_missingremoved %>%
  group_by(fldMainSpeciesCode) %>%
  nest()

svTypfit <- function(df) {
  vbStarts(fldFishLength ~ fldResult1, data=df, plot=TRUE)
}

startingvalues <- nest_by_species %>%
  mutate(
    model  = data %>% map(svTypfit)
  )

#Examine monkfish
LengthWeightAge_fish=filter(LengthWeightAge_missingremoved, fldMainSpeciesCode=="MON" &fldResult1<11)
svTyp=vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge_fish, plot=TRUE)
fitTyp=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAge_fish, start=svTyp)
coef(fitTyp)
confint(fitTyp)
bootTyp=nlsBoot(fitTyp)
headtail(bootTyp$coefboot, n=2)
confint(bootTyp, plot=TRUE)
summary(fitTyp, correlation=TRUE)

x=seq(0, max(LengthWeightAge_fish$fldResult1) ,length.out = 199)
#pTyp=vbTyp(x, Linf = svTyp[[1]], K=svTyp[[2]], t0=svTyp[[3]])
pTyp=vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]])
plot(fldFishLength ~ fldResult1, data=LengthWeightAge_fish)
lines(pTyp~x, lwd=2)

predicted_df <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]), 
                           x=seq(0, max(LengthWeightAge_fish$fldResult1),length.out = 199))

ggplot(LengthWeightAge_fish, aes(x=fldResult1, y=fldFishLength))+
  geom_point() +
  geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))+
  facet_wrap(~Year)+
  labs(x = "Age", y="Length")
#Examine saithe
LengthWeightAge_pok=filter(LengthWeightAge_missingremoved, fldMainSpeciesCode=="POK"&fldResult1<7)
vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge_pok, plot=TRUE)
plot(fldFishLength ~ fldResult1, data=LengthWeightAge_mon)



#Examine cod
LengthWeightAge_fish=filter(LengthWeightAge_missingremoved, fldMainSpeciesCode=="COD")
svTyp=vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge_fish, plot=TRUE)
vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
fitTyp=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAge_fish, start=svTyp)
coef(fitTyp)
confint(fitTyp)
bootTyp=nlsBoot(fitTyp)
headtail(bootTyp$coefboot, n=2)
confint(bootTyp, plot=TRUE)
summary(fitTyp, correlation=TRUE)

x=seq(0, 7 ,length.out = 199)
#pTyp=vbTyp(x, Linf = svTyp[[1]], K=svTyp[[2]], t0=svTyp[[3]])
pTyp=vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]])
plot(fldFishLength ~ fldResult1, data=LengthWeightAge_fish)
lines(pTyp~x, lwd=2)

predicted_df <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]), 
                           x=seq(0, 7,length.out = 199))

ggplot(LengthWeightAge_fish, aes(x=fldResult1, y=fldFishLength))+
  geom_point() +
  geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))+
  facet_wrap(~Year)+
  labs(x = "Age", y="Length")


################

#Examine Haddock
LengthWeightAge_fish=filter(LengthWeightAge_missingremoved, fldMainSpeciesCode=="HAD")
svTyp=vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAge_fish, plot=TRUE)
vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
fitTyp=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAge_fish, start=svTyp)
coef(fitTyp)
confint(fitTyp)
bootTyp=nlsBoot(fitTyp)
headtail(bootTyp$coefboot, n=2)
confint(bootTyp, plot=TRUE)
summary(fitTyp, correlation=TRUE)

x=seq(0, 13 ,length.out = 199)
#pTyp=vbTyp(x, Linf = svTyp[[1]], K=svTyp[[2]], t0=svTyp[[3]])
pTyp=vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]])
plot(fldFishLength ~ fldResult1, data=LengthWeightAge_fish)
lines(pTyp~x, lwd=2)

predicted_df <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]), 
                           x=seq(0, 13,length.out = 199))

ggplot(LengthWeightAge_fish, aes(x=fldResult1, y=fldFishLength))+
  geom_point() +
  geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))+
  facet_wrap(~Year)+
  labs(x = "Age", y="Length")

nest_by_year=LengthWeightAge_fish %>%
  group_by(Year) %>%
  nest()
fitTyp <- function(df) {
  nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=df, start=svTyp)
}
models <- nest_by_year %>%
  mutate(
    model  = data %>% map(fitTyp),
    predicted_df = model %>% map(vbTyp(x, Linf = coef(model)[[1]], K=coef(model)[[2]], t0=coef(model)[[3]])) 
  )


coef(models$model[[1]])[[1]]

predicted_df <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]), 
                           x=seq(0, 13,length.out = 199))

ggplot(LengthWeightAge_fish, aes(x=fldResult1, y=fldFishLength))+
  geom_point() +
  geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))+
  facet_wrap(~Year)+
  labs(x = "Age", y="Length")



plot_ly(LengthWeightAge_fish, y = ~(fldFishLength), x = ~fldResult1, type = 'scatter', 
        text=~paste("length:",(LengthWeightAge_fish$fldFishLength+5)/10,"cm","<br>age:",
                    LengthWeightAge_fish$fldResult1),
        hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
  layout(hovermode="closest", title=paste(species,"Age vs Length"),
        margin=(list(t=70)), showlegend = FALSE) %>%
  add_trace(x=x, y=length_pred, data=predicted_df, mode='lines+markers',
                      line = list(color = '#6699ff', opacity=0.5),
                      marker = list(color = 'grey', opacity=0))

  
    geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))
  
  

################
#Plots of data by year and species
for(i in species){
    LengthWeightAge_fish=filter(LengthWeightAge_missingremoved, fldMainSpeciesCode==i)
    ggplot(LengthWeightAge_fish, aes(x=fldResult1, y=fldFishLength))+
      geom_point() +
      facet_wrap(~Year)+
      labs(x = "Age", y="Length")
    ggsave(paste("Plots/", i, ".png"))
}

    
    
    
################    



fitTyp <- function(df) {
  nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=df, start=svTyp)
}


models <- nest_by_year %>%
  mutate(
    model  = data %>% map(fitTyp)
  )
  
models
coef(models$model[[]])


models <- models %>%
  mutate(
    glance  = model %>% map(broom::glance),
    aic     = glance %>% map_dbl("AIC"),
    tidy    = model %>% map(broom::tidy), #extracts coefficents, SE, p-value
    augment = model %>% map(broom::augment) # predicted values for data and residual calculated
  )
models


models %>%
  unnest(tidy) %>%
  select(Year, term, estimate, aic) %>%
  spread(term, estimate) %>%
  ggplot(aes(Linf, t0)) +
  geom_point(aes(colour = Year, size = aic)) +
  geom_smooth(se = FALSE) +
  xlab("Life Expectancy (1950)") +
  ylab("Yearly improvement") +
  scale_size_area()


models %>%
  unnest(augment) %>%
  ggplot(aes(fldFishLength, .resid)) +
  geom_line(aes(group = Year), alpha = 1/3) +
  geom_hline(yintercept = 0, colour = 'white', size = 2) +
  geom_smooth(se = FALSE)




data2004=filter(LengthWeightAge, Year==2004)
vbStarts(fldFishLength ~ fldResult1, data=data2004, plot=TRUE)



library(tidyverse)
LengthWeightAgeNoU=filter(LengthWeightAge, fldFishSex!="U")

nest_by_year=LengthWeightAgeNoU %>%
  group_by(Year, fldFishSex) %>%
  nest()


year_model <- function(df) {
  vbStarts(fldFishLength ~ fldResult1, data=df, plot=TRUE)
}

par(mfrow=c(3,2))
models <- nest_by_year %>%
  mutate(
    model  = data %>% map(year_model)
  )

models$model

