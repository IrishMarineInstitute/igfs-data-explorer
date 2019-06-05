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


myUrl <- "https://shiny.marine.ie"


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
                        choices = list(Haddock = paste(myUrl,"/igfshad/",sep=""),
                                       Whiting =paste(myUrl,"/igfswhg/",sep=""),
                                       Cod = paste(myUrl,"/igfscod/",sep=""),
                                       Hake = paste(myUrl,"/igfshke/",sep=""),
                                       Mackerel = paste(myUrl,"/igfsmac/",sep=""),
                                       "Horse Mackerel" = paste(myUrl,"/igfshom/",sep=""),
                                       "Blue Whiting"= paste(myUrl,"/igfswhb/",sep=""), 
                                       Herring = paste(myUrl,"/igfsher/",sep=""),
                                       "Norway Pout" = paste(myUrl,"/igfsnop/",sep=""),
                                       Sprat = paste(myUrl,"/igfsspr/",sep=""),
                                       Boarfish = paste(myUrl,"/igfsbof/",sep=""),
                                       "Poor Cod" = paste(myUrl,"/igfspod/",sep=""),
                                       "Grey Gurnard" = paste(myUrl,"/igfsgug/",sep=""),
                                       Dab = paste(myUrl,"/igfsdab/",sep=""),
                                       "Lesser Spotted Dogfish" = paste(myUrl,"/igfslsd/",sep=""),
                                       Plaice = paste(myUrl,"/igfsple/",sep=""),
                                       Megrim = paste(myUrl,"/igfsmeg/",sep=""),
                                       "Spotted Ray" = paste(myUrl,"/igfssdr/",sep=""),
                                       "John Dory" = paste(myUrl,"/igfsjod/",sep=""),
                                       "Thornback Ray" = paste(myUrl,"/igfsthr/",sep=""),
                                       Sole = paste(myUrl,"/igfssol/",sep=""),
                                       "Cuckoo Ray" = paste(myUrl,"/igfscur/",sep=""),
                                       Pollack = paste(myUrl,"/igfspol/",sep=""),
                                       "Common Skate" = paste(myUrl,"/igfsskt/",sep=""),
                                       Saithe = paste(myUrl,"/igfspok/",sep=""),
                                       Spurdog = paste(myUrl,"/igfsdgs/",sep=""),
                                       Seabass = paste(myUrl,"/igfsesb/",sep=""))
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



