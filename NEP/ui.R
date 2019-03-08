#Libraries
library(leaflet)
library(shinythemes)
library(dplyr)
library(ggridges)
library(plotly)
library(tidyverse)
library(tidyr)

# Define UI for application
shinyUI(
  navbarPage("Nephrops", id="nav",
             theme = shinytheme("flatly"),
tabPanel("Mapping", value="map",
                      tags$head(includeScript("google-analytics.js")),
                      fluidPage(
                        fluidRow(column(3, 
                                        uiOutput("yearfiltermap"),
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
                                 selectInput("parameter", h3("Select Functional Unit "),
                                             choices = c("16", "17", "19", "20-21","22","Outside FU"), selected = "16"),
                                 uiOutput("FUfilter"),uiOutput("FUyearfilter"),
                                 uiOutput("LWAdownload")
                          ),
                          column(9,
                                 tabsetPanel(id="tabselected", type="pills",
                                             tabPanel("CPUE", value="cpue",
                                                      fluidRow(column(5, plotlyOutput("cpueplotall", width="100%")),
                                                               column(7, plotlyOutput("cpueplotparam", width="85%"))),
                                                      "*Each circle is the CPUE and the line is the mean CPUE by Year"),
                                             tabPanel("Abundance", value="abundance",
                                                      fluidRow(column(5, plotlyOutput("abundanceplotall", width="100%")),
                                                               column(7, plotlyOutput("abundanceplotparam", width="85%"))),
                                                      "*Each circle is the Abundance  and the line is the mean Abundance by Year"),
                                             tabPanel("Length Frequency", value="lf",
                                                      fluidRow(column(5, plotOutput("lfplotall")),
                                                               column(7, plotOutput("lfplotparam"))),
                                                      "Vertical dashed line is the length cut off for Juvenile/Adult classification"),
                                             tabPanel("Length/Weight", value="lw", uiOutput("LengthWeightUI"))
                                 )
                          )
                        )
                      )
             )
#,
#tabPanel(HTML("</a></li><li><a href=\"https://shiny.marine.ie/igfs/\" target=_blank > Home Botton"))
  ))


