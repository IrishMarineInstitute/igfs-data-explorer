mapdata <- readRDS("Data/mapdata26022019.rds")
sp_data_gp <- readRDS("Data/sp_data_gp_26022019.RDS")
SpAggdata <- readRDS("Data/SpAggdata26022019.rds")

function(input, output, session) {
  
  ##### Interactive Map 1 - Timeseries #####
  # Create the map - leaflet 
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      setView(lng = -8.2124, lat = 53.2734, zoom = 6)
  })
  
  # Filtering for Mapping 
  mapdata1 <- reactive({
    subset(mapdata, Year %in% input$slideryear)
  })
  
  ##### Observer D #####
  # This observer is responsible for maintaining the circles and legend,histogram and gauges
  # according to the variables the user has chosen to map/display in the "station explorer"   
  observe({
    colorBy <-  input$catch_measure
    if(colorBy=="Number of fish (per hour)") {
      colorData <-  mapdata1()$RaisedNo
      pal <- colorNumeric("viridis", colorData)
      radius <- (mapdata1()$RaisedNo / max(mapdata1()$RaisedNo)) * 80000
    }else if(colorBy=="Biomass (Kg)"){
      colorData <-   mapdata1()$CatchKg
      pal <-  colorNumeric("magma", colorData)
      radius <- (mapdata1()$CatchKg/ max(mapdata1()$CatchKg)) *60000
    }

    leafletProxy("map", data = mapdata1()) %>% 
      clearShapes() %>% 
      addCircles(~Longitude,~Latitude, radius= radius, layerId=~PrimeStation,                  
                 stroke=FALSE,fillOpacity=0.7, fillColor=pal(colorData)) %>% 
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showStnPopup <- function(PrimeStation, Latitude, Longitude) { #Popups,
    selectedStn <- mapdata1()[mapdata1()$PrimeStation == PrimeStation,]
    content <- as.character(tagList(
      tags$strong("Survey Station:", selectedStn$PrimeStation), 
      tags$br(),
      tags$strong("Survey year:", selectedStn$Year),
      tags$br(),
      tags$strong("Catch:", as.integer(selectedStn$CatchKg), "kg"),
      tags$br(),
      tags$strong("Number of fish per hour:", as.integer(selectedStn$RaisedNo)),
      tags$br()
    ))    
    leafletProxy("map") %>% addPopups(lng=selectedStn$Longitude, lat=selectedStn$Latitude, 
                                      popup= content, layerId= PrimeStation, options = popupOptions()) 
  }
  
  # When map is clicked, show a popup with info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return() 
    isolate({
      showStnPopup(event$id, event$Latitude, event$Longitude)
    })
  }) 
  
  # A reactive expression that returns the set of stations
  # that are in the bounds of the map now 
  stnInView <- reactive({
    if (is.null(input$map_bounds))
      return(mapdata1()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north,bounds$south)
    lngRng <- range(bounds$east,bounds$west)
    
    subset(mapdata1(),
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })

  #####  Gagues #####  
  NumStn <- reactive({
    nrow(stnInView())
  })
  PerCat_bio <- reactive({(sum(stnInView()$CatchKg))/sum(mapdata1()$CatchKg)*100})#create a percentage 
  PerCat_bioformat <- reactive({formatC(PerCat_bio(),digits=0, format = "f")})
  PerCat <- reactive({(sum(stnInView()$RaisedNo))/sum(mapdata1()$RaisedNo)*100})#create a percentage 
  PerCatformat <- reactive({formatC(PerCat(),digits=0, format = "f")})
  
  # These observers are responsible for maintaining the gauges
  ##### Observer B #####
  observe({
    G1 <- input$catch_measure
    if(G1=="Number of fish (per hour)"){
      output$gauge1 <- flexdashboard::renderGauge({
        value_max <- max(nrow(mapdata1()))
        gauge(NumStn()[[1]], min = 0, max= value_max,
              sectors = gaugeSectors(danger = c(0,(value_max/3)),
                                     warning = c(((value_max/3)+1),((value_max/3)*2)),  
                                     success= c((((value_max/3)*2)+1),value_max)),  
              label= "Stations in view")  
      })
    }else if (G1=="Biomass (Kg)"){
      output$gauge1  <- flexdashboard::renderGauge({
        value_max <- max(nrow(mapdata1()))
        gauge(NumStn()[[1]], min = 0, max= value_max,
              sectors = gaugeSectors(danger = c(0,(value_max/3)),
                                     warning = c(((value_max/3)+1),((value_max/3)*2)),
                                     success= c((((value_max/3)*2)+1),value_max)),
              label= "Stations in view")
      })
    }
  })
  ##### Observer C #####
  observe({
    G2 <- input$catch_measure
    if(G2=="Number of fish (per hour)") { 
      output$gauge2 <- flexdashboard::renderGauge({
        gauge(PerCatformat()[[1]], min = 0, max = 100, symbol= "%", 
              sectors = gaugeSectors(danger = c(0,33), 
                                     warning = c(34,66), 
                                     success = c(67,100)),  
              label= "% of total catch in view")
      })
    }else if(G2=="Biomass (Kg)") {
      output$gauge2 <- flexdashboard::renderGauge({
        gauge(PerCat_bioformat()[[1]], min = 0,max = 100, symbol= "%",
              sectors = gaugeSectors(danger = c(0,33),
                                     warning = c(34,66),
                                     success = c(67,100)),
              label= "% of total catch in view")
      })
    }
  })
  ##### Pie chart  #####
  sp_data_gp1 <- reactive({
    subset(sp_data_gp, Year %in% input$slideryear)
  })
  sp_data_gp1_inView <- reactive({
    subset(sp_data_gp1(), sp_data_gp1()$PrimeStation %in% stnInView()$PrimeStation)
  })
  sp_data_gp2_kg <- reactive({
    sp_data_gp1_inView() %>%
    group_by(PrimeStation, Group) %>% 
    summarise(Totalkg = sum(CatchKg))
  })
  sp_data_gp2_no <- reactive({
    sp_data_gp1_inView() %>%
      group_by(PrimeStation, Group) %>% 
      summarise(Totalno = sum(RaisedNo))
  })
  m <- list(
    l = 30,
    r = 40,
    t = 160,
    b = 20
  )

observe({
    P1 <- input$catch_measure
    if(P1== "Number of fish (per hour)") { 

     output$sp_piechart <- renderPlotly({   
          sp_data_gp2_no()  %>%
            plot_ly(labels = ~Group, values = ~Totalno, 
                    textinfo ='label', textposition = "auto",  
                    type = 'pie',
                    marker = list(colors = c('#0c8e98','#06651f', '#F8D51A','#742416','#b8170f', '#f15f06'))) %>% ##8B2E62  
            layout(title = "Catch Composition (Raised numbers)",  showlegend = FALSE,
                   autosize = T, margin = m,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent',
                   font = list(color='#FFFFFF'))
        })
    }else if (P1 == "Biomass (Kg)") { 
      ##### Loading gif #####
      output$sp_piechart <- renderPlotly({   
        Sys.sleep(1)
        sp_data_gp2_kg() %>%
          plot_ly(labels = ~Group, values = ~Totalkg, 
                  textinfo ='label', textposition = "auto",
                  type = 'pie',
                  marker = list(colors = c('#0c8e98','#06651f', '#F8D51A','#742416','#b8170f', '#f15f06'))) %>% 
          layout(title = "Catch Composition (Catch Kg)",  showlegend = FALSE,
                 autosize = T, margin = m,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 font = list(color='#FFFFFF'))
         })
    }
})
 
##### Infographics  #####
## Box1
totalfish <- reactive({
  sum(stnInView()$RaisedNo,na.rm=TRUE)
  })
catch <- reactive({
  formatC(totalfish(), digits = 1, format= "d",mode = "integer", big.mark = ",")
  })

## Box2
biggestkg <-  reactive({
  max(stnInView()$CatchKg, na.rm=TRUE)
  })
maxKG <- reactive({
  formatC(biggestkg(),digits = 1, format = "d", mode = "integer", big.mark = ",")
  })

## Box3
totalswept <- reactive({
  formatC(sum(stnInView()$AreaKmSq),format="d", big.mark=",")
})
kmtext <- HTML("<p>Swept area (km <sup>2</sup> ) </p>")
output$box1 <- renderValueBox({valueBox(value = catch(),subtitle="Total Number of fish surveyed", icon = icon("fas fa-anchor"))})#,  "fas fa-calculator"
output$box2 <- renderValueBox({valueBox("largest survey catch (kg)", value = maxKG(),icon =icon("fas fa-trophy"))})#fas fa-trophy"
output$box3 <- renderValueBox({valueBox( value = totalswept(), subtitle = kmtext,icon = icon("fas fa-ship"))})

#####  Modal boxes #####
observeEvent(input$modal1,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="IGFS Introduction.pdf"),
    title= "Introduction to the Irish Groundfish Survey",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal2,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="IGFS Objectives.pdf"),
    title= "Survey Objectives",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal3,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="IGFS Design.pdf"),
    title= "Survey Design",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal4,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="IGFS Methodology.pdf"),
    title= "Methodology",
    easyClose = TRUE,
    footer = NULL))
})
output$moreInfo <- renderText("Click the buttons below for more
            information on the Irish Groundfish Survey")

##### Indices Downloader #####
IndicesTable=read.csv('Indices/IndicesRef.csv', header=TRUE)
IndicesTable$Species=as.character(IndicesTable$Species)
IndicesTable$Stock=as.character(IndicesTable$Stock)
Spfilter <- unique(IndicesTable$Species)

output$sp_indices <- renderUI({
  selectInput("speciesfilter", "Select Species", as.list(Spfilter), selected = "Cod") 
})
SpeciesStock<-  reactive({
  filter(IndicesTable, Species %in% c(input$speciesfilter))})
output$stock_indices <- renderUI({
  stocks <- unique(SpeciesStock()$Stock)
  selectInput("stockfilter", "Select Stock", as.list(stocks), selected = stocks[1])
})
filetable<- reactive({ 
  filter(IndicesTable, Species %in% c(input$speciesfilter) & Stock %in% c(input$stockfilter))
  })
indices_file<-  reactive({ 
  paste0(filetable()$NameofFile)
 })
x <- reactive({
   paste0("Indices/",indices_file(),sep="")
})
output$indiDownload <- downloadHandler(
  filename = function() {
    #indices_file
    paste0(filetable()$NameofFile)
  },
  content = function(file) {
    file.copy(x(), file) #x
  },
  contentType = ("text/csv")
)

##### Species specific pages #####
output$spSite <- renderUI({
  tags$a(href=input$spSelect,"Go", target="_blank")
}) 
}
