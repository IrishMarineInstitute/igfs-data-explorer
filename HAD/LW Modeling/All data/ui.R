#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggridges)
library(plotly)
library(FSA)
library(car)
library(magrittr)
library(plyr)
detach(package:plyr)
library(dplyr)

#library(rgdal)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Modelling", id="nav",
  #           fluidPage(
  #theme=themeSelector(),
    theme = shinytheme("cerulean"),
    # Application title
    #titlePanel("Haddock"),
  
    tabPanel("Plots", 
             fluidPage(
               fluidRow(
                 #tags$style(".span12 {background-color: black;}"),
                 column(3,#style = "background-color:#E7E7E7;", # text-colour:#FFFFFF;
                        uiOutput("speciesfilter"),
                        #selectInput("species", h3("Select Species"),
                        #            choices = as.list(specieslist), selected = "HAD"),
                        uiOutput("yearfilter"),
                        selectInput("parameter", h3("Select Parameter"),
                                    choices = c("None", "Gear", "Sex", "Division"), selected = "None"),
                        uiOutput("divfilter")
                        ),
                 column(9,
                        tabsetPanel(id="tabselected", type="pills",
                          tabPanel("Length/Weight", value="lw", plotlyOutput("lwplot"),
                                   "*Filtering also available through the legend on the RHS when a parameter is chosen"),
                          tabPanel("Length/Age", value="la",uiOutput("latab"),#plotlyOutput("laplot"),
                                   textOutput("startingvaluesall"),
                                   textOutput("startingvaluesyear"),
                                   textOutput("fittedvaluesall"),
                                   textOutput("fittedvaluesyear"),
                                   textOutput("fittedvaluesall2"),
                                   "*Filtering also available through the legend on the RHS when a parameter is chosen")
                        )
                        
                 )
               )
             )
    )
))
