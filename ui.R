



dashboardPage(
  dashboardHeader(title = "IGFS"),
  dashboardSidebar( sidebarMenu(id="menu1",
    menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Species specific pages", tabName = "sp", icon = icon("th"))
  ),
  conditionalPanel(
    condition = "input.menu1 == 'sp'",
    selectInput("sp1", "Select a species:", 
                choices = levels(sp_names$species),selectize = TRUE,selected = "Haddock"))),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      tabItem("dashboard",
    fluidPage(
      #includeCSS(path = "adminLTE.css"), 
      #includeScript(path = "app.js"),
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
                                             sliderInput("slideryear", "Year:", min = 2003, max = max(sp_data_gp$Year), value = max(sp_data_gp$Year),
                                                        step = 1, sep = "", animate = TRUE))),
                          fluidRow(column(6,gaugeOutput("gauge1"),style = "margin-top:-0em"),
                                   column(6,gaugeOutput("gauge2"),style = "margin-top:-0em")),
                          fluidRow(column(12,
                                          plotlyOutput("sp_piechart")
                                          %>% withSpinner(color="#0dc5c1"),
                                          style = "margin-top:-10em")
                                   
                          ),
            fluidRow(column(6,offset=4,downloadButton("downloadstation_data", "Download Station data")))
          ),
            solidHeader =FALSE, collapsible = T,width= "auto", background = "navy")
        ),
      fluidRow(
       
        box(title = "Indices",width = 4, background = "green",
            uiOutput("sp_indices"),
            uiOutput("stock_indices"),
            downloadButton("indiDownload", "Download")
        ),
        box(title = "Further information", width = 4, background = "green",
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
        box(title ="Useful links",width = 4, status = "success",
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
      )
      )),
    tabItem("sp", uiOutput("Sp_name"), fluidRow(
      tabBox(id="pages",width=12,height = "650px",
        tabPanel("Mapping",value = "map",
                 box(width=3, status = "primary",uiOutput("yearfilter1")
                    ,downloadButton("downloadData_map", "Download Map data")),
                 
                  box(title="Map", solidHeader = TRUE,width=9, status = "primary"
                      ,
                 leafletOutput('mymap', height=600)
                 )
        ),
        tabPanel("Plots",value="plots",
                 box(width=3,status = "primary",uiOutput("yearfilter"),
                     uiOutput("paramselector"),
                     # selectInput("parameter", h3("Select Parameter"),
                     #             choices = c("None", "Gear", "Sex", "Division"), selected = "None"),
                     uiOutput("divfilter"),
                     uiOutput("Ldownload"),
                     uiOutput("LWAdownload"),br(),
                     uiOutput("LPdownload")),
                 tabBox(id="tabselected",width=9,tabPanel("CPUE", value="cpue",
                                 fluidRow(column(5, plotlyOutput("cpueplotall", width="100%")),
                                          column(7, plotlyOutput("cpueplotparam", width="100%"))),
                                 "* The line is the mean CPUE by Year for stations with positive catches"),
                        tabPanel("Abundance", value="abundance",
                                 fluidRow(column(5, plotlyOutput("abundanceplotall", width="100%")),
                                          column(7, plotlyOutput("abundanceplotparam", width="100%"))),
                                 "* The line is the mean Abundance by Year"),
                        tabPanel("Length Frequency", value="lf",uiOutput("lengthfrequi")),
                        tabPanel("Length/Weight", value="lw", uiOutput("LengthWeightUI")),
                        tabPanel("Length/Age", value="la",uiOutput("latab")),
                        tabPanel("Cohort Length/Age", value="co", 
                                 uiOutput("cohorttab")))
                 
                 )
      )
    ))
    ),hr(),
    fluidRow(width =12,style = "margin-top:-4em",
             box(width =12,
                 img(src="Niamh.png", width = "900px", height = "75px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
             )
    )))



