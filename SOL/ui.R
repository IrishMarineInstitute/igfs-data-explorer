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
#library(ggplot2)
#library(plyr)
library(dplyr)
library(ggridges)
library(plotly)
#library(sf)
library(tidyr)
library(tidyverse)



# Define UI for application
shinyUI(
  navbarPage("Sole", id="nav",
        
             theme = shinytheme("flatly"),
             tabPanel("Mapping", value="map",
             tags$head(includeScript("google-analytics.js")),
             fluidPage(
               
               fluidRow(column(3, uiOutput("yearfilter1")
                               
                               ,
                               downloadButton("downloadData_map", "Download Map data"),
                               br(),
                               br(),
                               downloadButton("downloadstation_data", "Download Station data")),
                        column(9, leafletOutput('mymap', height = 580))
               ))
             ),
    tabPanel("Plots", 
             fluidPage(
               fluidRow(
                 column(3,
                        uiOutput("yearfilter"),
                        selectInput("parameter", h3("Select Parameter"),
                                    choices = c("None", "Gear", "Sex", "Division"), selected = "None"),
                        uiOutput("divfilter"),
                        uiOutput("LWAdownload")
                        ),
                 column(9,
                        tabsetPanel(id="tabselected", type="pills",
                          tabPanel("CPUE", value="cpue",
                                   fluidRow(column(5, plotlyOutput("cpueplotall", width="100%")),
                                            column(7, plotlyOutput("cpueplotparam", width="100%"))),
                                   "* The line is the mean CPUE by Year"),
                          tabPanel("Abundance", value="abundance",
                                   fluidRow(column(5, plotlyOutput("abundanceplotall", width="100%")),
                                            column(7, plotlyOutput("abundanceplotparam", width="100%"))),
                                   "* The line is the mean Abundance by Year"),
                          tabPanel("Length Frequency", value="lf",
                                   fluidRow(column(5, plotOutput("lfplotall")),
                                            column(7, plotOutput("lfplotparam"))),
                                   "Vertical line is the length cut off for Juvenile/Adult classification ( If available) "),
                          tabPanel("Length/Weight", value="lw", uiOutput("LengthWeightUI")),
                          tabPanel("Length/Age", value="la",uiOutput("latab")),
                          tabPanel("Cohort Length/Age", value="co", 
                                   uiOutput("cohorttab"))#"test",
                                   #plotlyOutput("cohortplot"),
                                   #tableOutput("coeff_table_cohort"))
                        )
                 )
               )
             )
    ),
    tabPanel("Prediction",
             
             fluidPage( 
             #  list(setBackgroundColor(
               #color = c("#F7FBFF", "#2171B5")
            
            # ),
          
               
               fluidRow(column(3, 
                               uiOutput("yearfilter2"),
                                           # animate = animationOptions(interval = 2000)),
                       
                        selectInput("parameterP", h3("Select Parameter"),
                                           choices = c("None", "Gear", "Sex","Area"), selected = "None")),
                               column(9,uiOutput("pred"))
               )))
   # )
))
