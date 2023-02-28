
library(leaflet)
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(htmltools)
library(shinycssloaders)
library(reshape2)
library(dplyr)
library(ggridges)
library(plotly)
library(tidyr)
library(tidyverse)
library(DT)
library(rgdal)


#############define colours#################


def<-c("#F8766D","#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")

########### Read in shapefiles########


div <- geojsonio::geojson_read("Data/shapefiles/div_simple.geojson", what = "sp")
cont <- geojsonio::geojson_read("Data/shapefiles/cont1_simple.geojson", what = "sp")

FU <- rgdal::readOGR("Data/shapefiles","Nephrops_Functional_Unit_Cut")
centers <-readRDS("Data/shapefiles/centers.RDS")


#####Data##########


#Species selectInput
sp_names<-read.csv("Data/Sp_names.csv", stringsAsFactors = TRUE)


stn=readRDS("Data/dataApp/stn.RDS")
data1=readRDS("Data/dataApp/data1.RDS")
dat=readRDS("Data/dataApp/dat.RDS")
dat1=readRDS("Data/dataApp/dat1.RDS")
mapdata <- readRDS("Data/dataApp/mapdata.RDS")
sp_data_gp <- readRDS("Data/dataApp/sp_data_gp.RDS")
LengthWeightAge <- readRDS("Data/dataApp/LengthWeightAge.RDS")
LengthData <- readRDS("Data/dataApp/LengthData.RDS")
mapdataS <- readRDS("Data/dataApp/mapdataS.RDS")
TotalNumbersMap <- readRDS("Data/dataApp/TotalNumbersMap.RDS")
AdultNumbersMap <- readRDS("Data/dataApp/AdultNumbersMap.RDS")
JuvNumbersMap <- readRDS("Data/dataApp/JuvNumbersMap.RDS")
indLW <- readRDS("Data/dataApp/indLW.RDS")
dat_raised <- readRDS("Data/dataApp/dat_raised.RDS")
datN <- readRDS("Data/dataApp/datN.RDS")
AdultNumbersMapN <- readRDS("Data/dataApp/AdultNumbersMapN.RDS")
JuvNumbersMapN <- readRDS("Data/dataApp/JuvNumbersMapN.RDS")

###max Year indicator
maxyear=as.numeric(max(LengthData$Year))

############################################
vbTyp = function(age, Linf, K, t0)Linf*(1-exp(-K*(age-t0)))
################################################

coeff_L_A<-read.csv("Data/dataApp/coeff_L_A.csv", stringsAsFactors = TRUE)




