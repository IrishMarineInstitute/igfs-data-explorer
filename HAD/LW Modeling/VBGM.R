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
nest_by_year=LengthWeightAge %>%
  group_by(Year) %>%
  nest()


fitTyp <- function(df) {
  nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=df, start=svTyp)
}


models <- nest_by_year %>%
  mutate(
    model  = data %>% map(fitTyp)
  )
  
coeffs=models %>% 
  unnest(map(model, tidy))


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

