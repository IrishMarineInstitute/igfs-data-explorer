##require(devtools)
##install_version("plotly",version = "4.7.1")


library(leaflet)
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(plotly)
library(htmltools)
library(shinycssloaders)
sp_data_gp <- readRDS("Data/sp_data_gp_20190306.rds")

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      includeCSS(path = "adminLTE.css"), 
      includeScript(path = "app.js"),
      tags$head(includeScript("google-analytics.js")),
      fluidRow(valueBoxOutput("box1"),
               valueBoxOutput("box2"),
               valueBoxOutput("box3")
      ),
      fluidRow(
        box(h4("Interactive Map"),style = "margin-top:-1.5em",
            column(8,
            leafletOutput("map", height=650)),
            column(4,
            id="controls", fixed=FALSE, draggable = TRUE,
                          fluidRow(column(11,selectInput("catch_measure",label=NULL, choices = c("Biomass (Kg)","Number of fish (per hour)")),
                                             sliderInput("slideryear", "Year:", min = 2003, max = max(sp_data_gp$Year), value = 2018,
                                                        step = 1, sep = "", animate = TRUE))),
                          fluidRow(column(6,gaugeOutput("gauge1"),style = "margin-top:-0em"),
                                   column(6,gaugeOutput("gauge2"),style = "margin-top:-0em")),
                          fluidRow(column(12,
                                          plotlyOutput("sp_piechart")
                                          %>% withSpinner(color="#0dc5c1"),
                                          style = "margin-top:-9em") 
                          )
          ),
            solidHeader =FALSE, collapsible = T,width= "auto", background = "navy")
        ),
      fluidRow(
        box(title = "Individual species", width = 3, status = "success",
            selectInput("spSelect","Select a species", 
                        choices = list(Haddock = "https://shiny.marine.ie/igfshad/",
                                       Whiting ="https://shiny.marine.ie/igfswhg/",
                                       Cod = "https://shiny.marine.ie/igfscod/",
                                       Hake = "https://shiny.marine.ie/igfshke/",
                                       Mackerel = "https://shiny.marine.ie/igfsmac/",
                                       "Horse Mackerel" = "https://shiny.marine.ie/igfshom/",
                                       "Blue Whiting"= "https://shiny.marine.ie/igfswhb/", 
                                       Herring = "https://shiny.marine.ie/igfsher/",
                                       "Norway Pout" = "https://shiny.marine.ie/igfsnop/",
                                       Sprat ="https://shiny.marine.ie/igfsspr/",
                                       Boarfish = "https://shiny.marine.ie/igfsbof/",
                                       "Poor Cod" = "https://shiny.marine.ie/igfspod/",
                                       "Grey Gurnard" = "https://shiny.marine.ie/igfsgug/",
                                       Dab = "https://shiny.marine.ie/igfsdab/",
                                       "Lesser Spotted Dogfish" = "https://shiny.marine.ie/igfslsd/",
                                       Plaice = "https://shiny.marine.ie/igfsple/",
                                       Megrim = "https://shiny.marine.ie/igfsmeg/",
                                       "Spotted Ray" = "https://shiny.marine.ie/igfssdr/",
                                       "John Dory" = "https://shiny.marine.ie/igfsjod/",
                                       "Thornback Ray" = "https://shiny.marine.ie/igfsthr/",
                                       Sole = "https://shiny.marine.ie/igfssol/",
                                       "Cuckoo Ray" = "https://shiny.marine.ie/igfscur/",
                                       Pollack = "https://shiny.marine.ie/igfspol/",
                                       "Common Skate" = "https://shiny.marine.ie/igfsskt/",
                                       Saithe = "https://shiny.marine.ie/igfspok/",
                                       Spurdog = "https://shiny.marine.ie/igfsdgs/",
                                       Seabass = "https://shiny.marine.ie/igfsesb/")
            ),
            htmlOutput("spSite"),
            br()),
        box(title = "Indices",width = 3, background = "green",
            uiOutput("sp_indices"),
            uiOutput("stock_indices"),
            downloadButton("indiDownload", "Download")
        ),
        box(title = "Further information", width = 3, background = "green",
            textOutput("moreInfo"),
            br(),
            actionButton("modal1", "Introduction"),
            actionButton("modal2", "Survey Objectives"),
            br(),
            br(),
            actionButton("modal3", "Survey Design"),
            actionButton("modal4", "Methodology"),
            br(),
            br()),
        box(title ="Useful links",width = 3, status = "success",
            a(href=paste0("https://shiny.marine.ie/speciesdash/"),
              "The Species Dashboard",target="_blank"),
            p(),
            a(href=paste0("https://shiny.marine.ie/stockbook/"),
              "The Digital Stockbook",target="_blank"),
            p(),
            a(href=paste0("https://www.marine.ie"),
              "The Marine Institute webpage",target="_blank"),
            br(),
            br(),
            br(),
            br())
      ),
      hr(),
      fluidRow(width =12,style = "margin-top:-4em",
               box(width =12,
                   img(src="Niamh.png", width = "900px", height = "75px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
                   )
               )
      )
    ))



