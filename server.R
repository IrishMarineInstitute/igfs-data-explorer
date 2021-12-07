
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
    b = 40
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

##########################################################################################
###########################Species page###################################################
########################################################################################

species=reactive({input$sp1})
speciesFAO=reactive({filter(sp_names,species==input$sp1)$speciesFAO })

####Species Title###
output$Sp_name=renderUI({list(h3(paste("Species Name:",species())),h3(paste("FAO Code:", speciesFAO())))})
#####for map#####
output$yearfilter1=renderUI({
  sliderInput("slideryearS", "Choose Year:", min = 2003, max = maxyear, value = maxyear, step = NULL, 
              sep = "", animate = TRUE)
  
})

output$paramselector=renderUI({
  if(input$sp1=="Nephrops"){
  selectInput("parameterN", h3("Select Functional Unit "),
              choices = c("16", "17", "19", "20-21","22","Outside FU"), selected = "16")}
else{selectInput("parameter", h3("Select Parameter"),
                        choices = c("None", "Gear", "Sex", "Division"), selected = "None")}
})
#For Length/Weight and Length/Age plots
output$yearfilter=renderUI({
  if(input$tabselected=="lw"  | input$tabselected=="la" | input$tabselected=="co"){
    sliderInput("slideryearS1", "Choose Year:", min = 2003, max = maxyear, value = maxyear, step = NULL, 
                sep = "", animate = TRUE)
  }
})


#Divfilter only appears if Division parameter selected
output$divfilter=renderUI({
  if(input$sp1!="Nephrops" & input$parameter=="Division" & input$tabselected %in% c("cpue","abundance","lf","lw")){
   divlist= factor(as.character(unique(dat$ICESCODE)))
     checkboxGroupInput("division1", h3("Select Division"),choices=sort(divlist))}
})





######################
### Filtering data ###
######################
juv_length_split=reactive({
  if(input$sp1=="Nephrops"){
    17}
else{  
  dplyr::filter(LengthData,Species%in%speciesFAO())$preRecruitLength[1]}
  })
########Map#####
cat=reactive({
  if(input$sp1=="Nephrops"){
    dplyr::filter(dat_raised, Survey_Code %in% paste0('IGFS', input$slideryearS))}
  else{
    dplyr::filter(dat1, Cruise %in% paste0('IGFS', input$slideryearS),Species%in%speciesFAO())
   }
})

haul = reactive({
  dplyr::filter(stn, fldCruiseName==paste0('IGFS',  input$slideryearS))
})

 mapdataSS=reactive({
   subset(mapdataS, Year %in% input$slideryearS & No_km2>0 & Species%in%speciesFAO())
 })
 


 JuvNumbers=reactive({
   if(input$sp1=="Nephrops"){
     dplyr::filter(JuvNumbersMapN, Year %in% input$slideryearS & No_30min>0)
   }
   else {dplyr::filter(JuvNumbersMap, Year %in% input$slideryearS & CatchNos30minHaul>0,Species%in%speciesFAO())}
 })
 
 AdultNumbers=reactive({
   if(input$sp1=="Nephrops"){
     dplyr::filter(AdultNumbersMapN, Year %in% input$slideryearS & No_30min>0)
   }
   else{dplyr::filter(AdultNumbersMap, Year %in% input$slideryearS & CatchNos30minHaul>0,Species%in%speciesFAO())}
 })


 
 TotalNumbers=reactive({
  dplyr::filter(TotalNumbersMap, Year %in% input$slideryearS & CatchNos30minHaul>0,Species%in%speciesFAO())
 })
 
 
 ####Plots####
 N_FU<-reactive({
   if(input$tabselected=="lf"){dplyr::filter(datN,Functional_Unit==input$parameterN)}
   else {dplyr::filter(dat_raised,Functional_Unit==input$parameterN)}
 })
 
datS=reactive({
 
   dplyr::filter(dat,Species%in%speciesFAO())
 })
 
 data1S=reactive({
   
   dplyr::filter(data1,Species%in%speciesFAO())
 })
 
 LengthDataS=reactive({
  
   dplyr::filter(LengthData,Species%in%speciesFAO())
 })

output$mymap <- renderLeaflet({
  if(input$sp1=="Nephrops"){
    
    mymap <-leaflet() %>%
      setView(lng = -9, lat = 53, zoom = 5.5) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      
      addPolylines(color = "grey",data= FU, group = "Functional Units", weight = 2)%>%
      addLabelOnlyMarkers(data = centers,
                          lng = ~x, lat = ~y, label = ~paste("", region),
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
      addLegend("bottomright",colors = c("blue","purple", "black","yellow", "green",""), 
                labels = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>", 
                           "Total No of Nephrops per 30 min Haul",
                           "No of Juvenile Nephrops per 30 min Haul", "No of Adult Nephrops per 30 min Haul",paste0("Length cut off for Juvenile/Adult = ",juv_length_split())))%>%
      hideGroup("Stations Surveyed")
    
    return(mymap)
  }
  else if (is.na(juv_length_split())==TRUE){
    mymap <-leaflet() %>%
      setView(lng = -9, lat = 53, zoom = 6) %>%
      #addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
      addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)%>%
      #addControl(html = html_legend, position = "bottomright")%>%
      addLegend("bottomright",colors = c("blue","purple", "black"), 
                labels = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>",  "Total No of Fish per 30 min Haul"))%>%
      hideGroup("Stations Surveyed")
    
    return(mymap)
    
  }
  else {mymap <-leaflet() %>%
    setView(lng = -9, lat = 53, zoom = 6) %>%
    #addTiles() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
    addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)%>%
    #addControl(html = html_legend, position = "bottomright")%>%
    addLegend("bottomright",colors = c("blue","purple", "black", "yellow", "green",""), 
              labels = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>",  "Total No of Fish per 30 min Haul", 
                         "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul",paste0("Length cut off for Juvenile/Adult = ",juv_length_split())))%>%
   hideGroup("Stations Surveyed")
   
  return(mymap)}
})
icon.ship <- makeIcon(iconUrl  = 'www/x-mark-16.png', iconHeight = 7, iconWidth = 7)
observe({
 req(input$pages=="map")
 new_zoom <- input$mymap_zoom
  if(is.na(juv_length_split())==FALSE){
    if(input$sp1=="Nephrops"){
      leafletProxy('mymap') %>%
        clearGroup(group =  c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>", 
                              "Total No of Nephrops per 30 min Haul", "No of Juvenile Nephrops per 30 min Haul", 
                              "No of Adult Nephrops per 30 min Haul"))%>%
        addMarkers(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, icon =icon.ship, 
                   group="Stations Surveyed", 
                   popup=paste("<b>Haul:</b> ",haul()$fldCruiseStationNumber, "<br />","<b>Station:</b> ",haul()$fldPrimeStation, "<br />", "<b>Gear Type:</b> ",haul()$Gear_Type,"<br />", 
                               "<b>Tow Duration:</b> ",haul()$TowDurationMin, "mins", "<br />", "<b>Door Spread:</b> ",haul()$DoorSpread, "<br />",  
                               "<b>Wing Spread:</b> ",haul()$fldWingSpread, "<br />", "<b>Headline Height:</b> ",haul()$fldHeadlineHeight, "<br />",  
                               "<b>Distance KM:</b> ",round(haul()$Dist_Km,2),"<br />", "<b>Stratum:</b> ", haul()$fldStratum)) %>%
        addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=30000*cat()$symbSize/new_zoom, group="Catch Rate kg/hr", color= "blue",
                   popup=paste("<b>Cruise:</b> ",cat()$Survey_Code, "<br />", "<b>Haul</b>: ",cat()$Haul, "<br />",
                               "<b>Catch Kg/Hr:</b> ", round(cat()$Kg_Hr,2))) %>%
        addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=10000*cat()$symbSize2/new_zoom, group="Distribution No/km<sup>2</sup>",color= "purple", 
                   popup=paste("<b>Haul</b>: ",cat()$Haul, "<br />",
                               "<b>No per km<sup>2</sup>:</b> ", round(cat()$No_Km2,0))) %>%
        addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=30000*cat()$symbSize3/new_zoom, group="Total No of Nephrops per 30 min Haul", 
                   color= "black", popup=paste("<b>Haul</b>: ",cat()$Haul, "<br />",
                                               "<b> Total No of Nephrops per 30 min Haul:</b> ",
                                               round(cat()$No_30min,0))) %>%
        addCircles(lng=JuvNumbers()$Lon, lat=JuvNumbers()$Lat, radius=50000*JuvNumbers()$symbSize/new_zoom, 
                   color = "yellow",  group="No of Juvenile Nephrops per 30 min Haul", 
                   popup=paste("<b>Haul</b>:  ",JuvNumbers()$Haul, "<br />", "<b>No of Juvenile Nephrops per 30 min Haul:</b> ",
                               round(JuvNumbers()$No_30min,0)))  %>%
        addCircles(lng=AdultNumbers()$Lon, lat=AdultNumbers()$Lat, radius=10000*AdultNumbers()$symbSize/new_zoom, 
                   color = "green",  group="No of Adult Nephrops per 30 min Haul", 
                   popup=paste("<b>Haul</b>:  ",AdultNumbers()$Haul, "<br />", "<b>No of Adult Nephrops per 30 min Haul:</b> ",
                               round(AdultNumbers()$No_30min,0)))  %>%
        
        addLayersControl(
          baseGroups = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>","Total No of Nephrops per 30 min Haul", 
                         "No of Juvenile Nephrops per 30 min Haul", "No of Adult Nephrops per 30 min Haul"),
          overlayGroups = c("Stations Surveyed"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
  else{
    
    
    leafletProxy('mymap') %>%
    clearGroup(group =  c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>", "Stations Surveyed", 
                          "Total No of Fish per 30 min Haul", "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"))%>%
    addMarkers(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, icon =icon.ship, 
               group="Stations Surveyed", 
               popup=paste("<b>Haul:</b> ",haul()$fldCruiseStationNumber, "<br />","<b>Station:</b> ",haul()$fldPrimeStation, "<br />", "<b>Gear Type:</b> ",haul()$Gear_Type,"<br />", 
                           "<b>Tow Duration:</b> ",haul()$TowDurationMin, "mins", "<br />", "<b>Door Spread:</b> ",haul()$DoorSpread, "<br />",  
                           "<b>Wing Spread:</b> ",haul()$fldWingSpread, "<br />", "<b>Headline Height:</b> ",haul()$fldHeadlineHeight, "<br />",  
                           "<b>Distance KM:</b> ",round(haul()$Dist_Km,2),"<br />", "<b>Stratum:</b> ", haul()$fldStratum))%>%
   addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=10000*cat()$symbSize/new_zoom, group="Catch Rate kg/hr", 
      
               popup=paste("<b>Species:</b> ",species(), "<br />","<b>Cruise:</b> ",cat()$Cruise, "<br />", "<b>Haul</b>: ",cat()$Haul, "<br />", 
                           "<b>Station:</b> ",cat()$Prime_Stn, "<br />", "<b>Catch Kg/Hr:</b> ", round(cat()$Kg_Per_Hr,2)))%>%
    addCircles(lng=mapdataSS()$LonDec, lat=mapdataSS()$LatDec, radius=2000*mapdataSS()$symbSize/new_zoom, color = "purple",  
               group="Distribution No/km<sup>2</sup>", popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",mapdataSS()$Haul, "<br />", 
                                                                   "<b>No per km<sup>2</sup>:</b> ", round(mapdataSS()$No_km2,0)))%>%
    addCircles(lng=TotalNumbers()$LonDec, lat=TotalNumbers()$LatDec, radius=5000*TotalNumbers()$symbSize/new_zoom, 
               color = "black",  group="Total No of Fish per 30 min Haul", 
               popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",TotalNumbers()$Haul, "<br />", "<b>No of Fish per 30 min Haul:</b> ",
                           round(TotalNumbers()$CatchNos30minHaul,0)))  %>%
    addCircles(lng=JuvNumbers()$LonDec, lat=JuvNumbers()$LatDec, radius=5000*JuvNumbers()$symbSize/new_zoom, 
               color = "yellow",  group="No of Juvenile Fish per 30 min Haul", 
               popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",JuvNumbers()$Haul, "<br />", "<b>No of Juvenile Fish per 30 min Haul:</b> ",
                           round(JuvNumbers()$CatchNos30minHaul,0)))  %>%
    addCircles(lng=AdultNumbers()$LonDec, lat=AdultNumbers()$LatDec, radius=5000*AdultNumbers()$symbSize/new_zoom, 
               color = "green",  group="No of Adult Fish per 30 min Haul", 
               popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",AdultNumbers()$Haul, "<br />", "<b>No of Adult Fish per 30 min Haul:</b> ",
                           round(AdultNumbers()$CatchNos30minHaul,0)))%>%
  addLayersControl(
    baseGroups = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>","Total No of Fish per 30 min Haul", 
                   "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"),
    overlayGroups = c("Stations Surveyed"),
    options = layersControlOptions(collapsed = FALSE)
  )}}
  else if (is.na(juv_length_split())==TRUE){
  leafletProxy('mymap') %>%
      clearGroup(group =  c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>", "Stations Surveyed", 
                            "Total No of Fish per 30 min Haul", "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"))%>%
      addMarkers(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, icon =icon.ship, 
                 group="Stations Surveyed", 
                 popup=paste("<b>Station:</b> ",haul()$fldPrimeStation, "<br />", "<b>Gear Type:</b> ",haul()$Gear_Type,"<br />", 
                             "<b>Tow Duration:</b> ",haul()$TowDurationMin, "mins", "<br />", "<b>Door Spread:</b> ",haul()$DoorSpread, "<br />",  
                             "<b>Wing Spread:</b> ",haul()$fldWingSpread, "<br />", "<b>Headline Height:</b> ",haul()$fldHeadlineHeight, "<br />",  
                             "<b>Distance KM:</b> ",round(haul()$Dist_Km,2),"<br />", "<b>Stratum:</b> ", haul()$fldStratum))%>%
      addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=10000*cat()$symbSize/new_zoom, group="Catch Rate kg/hr", 
                 popup=paste("<b>Species:</b> ",species(), "<br />","<b>Cruise:</b> ",cat()$Cruise, "<br />", "<b>Haul</b>: ",cat()$Haul, "<br />", 
                             "<b>Station:</b> ",cat()$Prime_Stn, "<br />", "<b>Catch Kg/Hr:</b> ", round(cat()$Kg_Per_Hr,2)))%>%
      addCircles(lng=mapdataSS()$LonDec, lat=mapdataSS()$LatDec, radius=2000*mapdataSS()$symbSize/new_zoom, color = "purple",  
                 group="Distribution No/km<sup>2</sup>", popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",mapdataSS()$Haul, "<br />", 
                                                                     "<b>No per km<sup>2</sup>:</b> ", round(mapdataSS()$No_km2,0)))%>%
      addCircles(lng=TotalNumbers()$LonDec, lat=TotalNumbers()$LatDec, radius=5000*TotalNumbers()$symbSize/new_zoom, 
                 color = "black",  group="Total No of Fish per 30 min Haul", 
                 popup=paste("<b>Species:</b> ",species(), "<br />","<b>Haul:</b> ",TotalNumbers()$Haul, "<br />", "<b>No of Fish per 30 min Haul:</b> ",
                             round(TotalNumbers()$CatchNos30minHaul,0)))  %>%
      
      addLayersControl(
        baseGroups = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>","Total No of Fish per 30 min Haul"),
        overlayGroups = c("Stations Surveyed"),
        options = layersControlOptions(collapsed = FALSE)
      )}
})



############
### CPUE ###
############
output$cpueplotall=renderPlotly({
  if(input$sp1=="Nephrops"){
    catchAll <- aggregate(list(KgHr=N_FU()$Kg_Hr), list(Cruise=N_FU()$Survey_Code, Year= N_FU()$Year),mean, na.rm=TRUE)
    p=ggplot(N_FU(), aes(x=Year, y=Kg_Hr)) + 
      geom_jitter(width = 0.05, colour="grey",aes(text=sprintf("Station: %s", Haul))) + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=0.5)+ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw()  + theme(legend.position = "none")+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+
      scale_x_continuous(breaks=datN$Year) + theme(axis.title.x=element_blank())
    ggplotly(p,width=300)
   # plotly_build(p)
  }
 else{
    
   
  #  catchAll<-aggregate(data1S()[,c("Kg_Per_Hr")],by=list(data1S()$Yr,data1S()$Haul),FUN=sum,  na.rm=TRUE) 
  #  names(catchAll)=c("Year", "Haul", "KgHr")
  #  
  #  catchmean<-aggregate(data1S()[,c("Kg_Per_Hr")],by=list(data1S()$Yr),FUN=mean,  na.rm=TRUE) 
  #  names(catchmean)=c("Year", "KgHr")
  #  
  # p=ggplot(catchAll, aes(x=Year, y=KgHr)) + 
  #   geom_point(width = 0.05,colour="grey",aes(text=sprintf("Station: %s", Haul)))+ 
  #   geom_line(data=catchmean, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +
  #   theme_bw() + theme(legend.position = "none")
  # ggplotly(p)
   
   catchAll <- aggregate(list(KgHr=datS()$Kg_Per_Hr), list(Cruise=datS()$Cruise, Year= datS()$Year),mean, na.rm=TRUE)
   p=ggplot(datS(), aes(x=Year, y=Kg_Per_Hr)) + 
     geom_point(width = 0.05,colour="grey",aes(text=sprintf("Station: %s", Haul)))+ 
     geom_line(data=catchAll, aes(x=Year, y =KgHr), size=0.5)+ ylab("KG/Hour") +
     theme_bw() + theme(legend.position = "none")
   ggplotly(p)
   
   
   }
})

output$cpueplotparam=renderPlotly({
  if(input$sp1=="Nephrops"){ 
    catchsex <- aggregate(list(KgHr=N_FU()$Kg_Hr), list(Cruise=N_FU()$Survey_Code, Year= N_FU()$Year,Sex=N_FU()$Sex),mean, na.rm=TRUE)
  p=ggplot(N_FU(), aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05,aes(text=sprintf("Station: %s", Haul))) + 
    geom_line(data=catchsex, aes(x=Year, y =KgHr), size=0.5,colour="black")+ ylab("KG/Hour") + facet_wrap(~Sex)+
    theme_bw() + theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+
    scale_x_continuous(breaks=datN$Year)+ theme(axis.title.x=element_blank())
  ggplotly(p)}
  else{
  if(input$parameter=="None"){
    p=NULL
  }else if(input$parameter=="Sex"){
    catchSex <- aggregate(list(KgHr=datS()$Kg_Per_Hr), 
                          list(Cruise=datS()$Cruise, Year=datS()$Year, fldSex=datS()$fldSex), mean, na.rm=TRUE)
    p=ggplot(datS(), aes(x=Year, y=Kg_Per_Hr, colour=fldSex)) + geom_jitter(width = 0.05,aes(text=sprintf("Station: %s", Haul))) + 
      geom_line(data=catchSex, aes(x=Year, y =KgHr), size=0.5,colour="black")+ ylab("KG/Hour") +  scale_color_manual(values=c("U"="#F8766D","F"="#00BFC4","M"="#B79F00"))+
      facet_wrap(~fldSex) + theme_bw() + theme(legend.position = "none")
  }else if(input$parameter=="Gear"){
    catchGear <- aggregate(list(KgHr=datS()$Kg_Per_Hr), 
                           list(Cruise=datS()$Cruise, Year= datS()$Year, GearDescription=datS()$GearDescription), mean, na.rm=TRUE)
    p=ggplot(datS(), aes(x=Year, y=Kg_Per_Hr, colour=GearDescription)) + geom_jitter(width = 0.05,aes(text=sprintf("Station: %s", Haul))) + 
      geom_line(data=catchGear, aes(x=Year, y =KgHr), size=0.5,colour="black")+ ylab("KG/Hour") +
      facet_wrap(~GearDescription) + theme_bw() + theme(legend.position = "none")
  }else if(input$parameter=="Division"){
    if(is.null(input$division1)){
      p=NULL
    }else{
      cpuebydiv=filter(datS(), ICESCODE %in% c(input$division1))
      catchArea <- aggregate(list(KgHr=cpuebydiv$Kg_Per_Hr), 
                             list(Cruise=cpuebydiv$Cruise, Year= cpuebydiv$Year,  
                                  ICESCODE=cpuebydiv$ICESCODE), mean, na.rm=TRUE)
      p=ggplot(cpuebydiv, aes(x=Year, y=Kg_Per_Hr, colour=ICESCODE)) + geom_jitter(width = 0.05,aes(text=sprintf("Station: %s", Haul))) + 
        geom_line(data=catchArea, aes(x=Year, y =KgHr), size=0.5,colour="black")+ ylab("KG/Hour") +
        facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")+ scale_colour_manual(values=def)
    }
  }
  if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else ggplotly(p)}
})


#################
### Abundance ###
#################
output$abundanceplotall=renderPlotly({
  if(input$sp1=="Nephrops"){
    meanAll <- aggregate(N_FU()[,c("No_Km2")],by=list(N_FU()$Year),FUN=mean,  na.rm=TRUE)
    names(meanAll)=c("Year", "No_Km2")
    p=ggplot(N_FU(), aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=0.5)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
      theme_bw()  + theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+
      scale_x_continuous(breaks=datN$Year) + theme(axis.title.x=element_blank())
    ggplotly(p,width=300)
  }
  #Order the data
  else{meanAll<-aggregate(data1S()[,c("catch_km2","No_km2")],by=list(data1S()$Yr),FUN=mean,  na.rm=TRUE) 
  names(meanAll)=c("Year", "catch_km2", "No_km2")
  p=ggplot(data1S(), aes(x=Yr, y=No_km2)) + geom_jitter(width = 0.05, colour="grey") + 
    geom_line(data=meanAll, aes(x=Year, y =No_km2), size=0.5)+ 
    ylab("No/KM<sup>2</sup>")+ xlab("Year") +
    theme_bw() + theme(legend.position = "none")
  ggplotly(p)}
})

output$abundanceplotparam=renderPlotly({
  if(input$sp1=="Nephrops"){
    meansex <- aggregate(N_FU()[,c("No_Km2")],by=list(N_FU()$Year,N_FU()$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(N_FU(), aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2),size=0.5,colour="black")+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+
      scale_x_continuous(breaks=datN$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)
  }
  else{if(input$parameter=="None"){
    p=NULL
  }else if(input$parameter=="Sex"){
    meanSex<-aggregate(data1S()[,c("catch_km2","No_km2")], by=list(data1S()$Yr, data1S()$Sex),FUN=mean,  na.rm=TRUE)
    names(meanSex)=c("Year", "Sex", "catch_km2", "No_km2")
    p=ggplot(data1S(),aes(x=Yr, y=No_km2, colour=Sex)) +geom_jitter(width=0.05) + 
      geom_line(data=meanSex, aes(x=Year, y =No_km2), size=0.5,colour="black")+   scale_color_manual(values=c("U"="#F8766D","F"="#00BFC4","M"="#B79F00"))+
      labs(y = "No/KM<sup>2</sup>", x="Year") +
      facet_wrap(~Sex) + theme_bw() + theme(legend.position = "none")
  }else if(input$parameter=="Gear"){
    meanGear<-aggregate(data1S()[,c("catch_km2","No_km2")],
                        by=list(data1S()$Yr, data1S()$fldGearDescription),FUN=mean,  na.rm=TRUE)
    names(meanGear)=c("Year", "fldGearDescription", "catch_km2", "No_km2")
    p=ggplot(data1S(),aes(x=Yr, y=No_km2, colour=fldGearDescription)) +geom_jitter(width=0.05) + 
      geom_line(data=meanGear, aes(x=Year, y =No_km2),size=0.5,colour="black" )+
      labs(y = "No/KM<sup>2</sup>", x="Year") +
      facet_wrap(~fldGearDescription) + theme_bw() + theme(legend.position = "none")
  }else if(input$parameter=="Division"){
    if(is.null(input$division1)){
      ## "Select a Division"
      p=NULL
    }else{
      abundancebydiv=filter(data1S(), ICESCODE %in% c(input$division1))
      meanDiv<-aggregate(abundancebydiv[,c("catch_km2","No_km2")], 
                         by=list(abundancebydiv$Yr,abundancebydiv$ICESCODE), FUN=mean, na.rm=TRUE)
      names(meanDiv)=c("Year", "ICESCODE", "catch_km2", "No_km2")
      p=ggplot(abundancebydiv, aes(x=Yr, y=No_km2, colour=ICESCODE)) + geom_jitter(width = 0.05) + 
        geom_line(data=meanDiv, aes(x=Year, y =No_km2),size=0.5,colour="black" )+
        labs(y = "No/KM<sup>2</sup>", x="Year") +
        facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")+ scale_colour_manual(values=def)
    }
  }
  if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else ggplotly(p)}
})


########################
### Length Frequency ###
########################
# Get recent data for current species
output$lfplotall=renderPlot({
  if(input$sp1=="Nephrops"){
    lfAll <- aggregate(N_FU()[,c("NepCount")],by=list(N_FU()$Year,N_FU()$CLmm,N_FU()$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +
      geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split(),lty=2)+scale_y_continuous(breaks=lfAll$Year)
  }
  else {lftotal<-aggregate(LengthDataS()[,c("CatchNos", "CatchNos30minHaul")],
                     by=list(LengthDataS()$Year,LengthDataS()$LengthClass),FUN=sum,  na.rm=TRUE)
  names(lftotal)=c("Year", "LengthClass", "CatchNos", "CatchNos30minHaul")
  ggplot(lftotal, aes(LengthClass, Year, height = CatchNos, group = Year, alpha=.5)) + 
    geom_density_ridges(stat = "identity", scale = 2.6) + 
    labs(y = "Year", x="Length Class") +scale_y_continuous(breaks=lftotal$Year)+ 
    theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split())}
})
 
 output$lfplotparam=renderPlot({
   if(input$sp1=="Nephrops"){
    lfAll <- aggregate(N_FU()[,c("NepCount")],by=list(N_FU()$Year,N_FU()$CLmm,N_FU()$Functional_Unit,N_FU()$Sex),FUN=sum,  na.rm=TRUE)
   names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
   ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +
     geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
     theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split(),lty=2)+scale_y_continuous(breaks=lfAll$Year) }
 else {if(input$parameter=="None"){
   }else if(input$parameter=="Sex"){
     lfsex<-aggregate(LengthDataS()[,c("CatchNos", "CatchNos30minHaul")],
                     by=list(LengthDataS()$Year,LengthDataS()$LengthClass, LengthDataS()$fldSex),FUN=sum,  na.rm=TRUE)
    names(lfsex)=c("Year", "LengthClass", "fldSex", "CatchNos", "CatchNos30minHaul")
     ggplot(lfsex, aes(LengthClass, Year, height = CatchNos, fill=fldSex, group = Year, alpha=.5)) + 
     geom_density_ridges(stat = "identity", scale = 2.6) +  
      labs(y = "Year", x="Length Class") +
      facet_wrap(~fldSex) + theme_bw()+scale_fill_manual(values=c("U"="#F8766D","F"="#00BFC4","M"="#B79F00"))+
    theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split())+scale_y_continuous(breaks=lfsex$Year) 
   }
    else if(input$parameter=="Gear"){
    lfgear<-aggregate(LengthDataS()[,c("CatchNos", "CatchNos30minHaul")],
                      by=list(LengthDataS()$Year,LengthDataS()$LengthClass, LengthDataS()$fldGearDescription),FUN=sum,  na.rm=TRUE)
     names(lfgear)=c("Year", "LengthClass", "fldGearDescription", "CatchNos", "CatchNos30minHaul")
     AllOptions=expand(lfgear, Year, fldGearDescription, LengthClass)
    MakingZeros=left_join(AllOptions, lfgear, by=c("Year", "fldGearDescription", "LengthClass"))
    MakingZeros$CatchNos[is.na(MakingZeros$CatchNos)] <- 0
     MakingZeros$CatchNos30minHaul[is.na(MakingZeros$CatchNos30minHaul)] <- 0
     ggplot(MakingZeros, aes(LengthClass, Year, height = CatchNos, fill=fldGearDescription,  group = Year, alpha=.5)) + 
       geom_density_ridges(stat = "identity", scale = 2.6) + labs(y = "Year", x="Length Class") +scale_y_continuous(breaks=lfgear$Year) +
       facet_wrap(~fldGearDescription) + theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split())
   }else if(input$parameter=="Division"){
     if(is.null(input$division1)){
       ## "Select a Division"
    }else{
      lfdiv<-aggregate(LengthDataS()[,c("CatchNos", "CatchNos30minHaul")],
                       by=list(LengthDataS()$Year,LengthDataS()$LengthClass, LengthDataS()$ICESCODE),FUN=sum,  na.rm=TRUE)
       names(lfdiv)=c("Year", "LengthClass", "ICESCODE", "CatchNos", "CatchNos30minHaul")
       
       nest_by_year=lfdiv %>%
         group_by(ICESCODE, Year) %>%
         nest()
       
       List_LC=function(df) {
         (min(df$LengthClass, na.rm = TRUE)-1):(max(df$LengthClass, na.rm = TRUE)+1)}
       models <- nest_by_year %>%
        mutate(LengthClass  = data %>% map(List_LC))
       testing=unnest(models, LengthClass)
       
       MakingZeros=left_join(testing, lfdiv, by = c("ICESCODE", "Year", "LengthClass"))
       MakingZeros$CatchNos[is.na(MakingZeros$CatchNos)] <- 0
       MakingZeros$CatchNos30minHaul[is.na(MakingZeros$CatchNos30minHaul)] <- 0
       
       lfbydiv=filter(MakingZeros, ICESCODE %in% c(input$division1))
       ggplot(lfbydiv, aes(LengthClass, Year, height = CatchNos, fill=ICESCODE, group = Year, alpha=.5)) + 
         geom_density_ridges(stat = "identity", scale = 2.6) + 
         labs(y = "Year", x="Length Class") + scale_fill_manual(values=def)+scale_y_continuous(breaks=lfdiv$Year) +
         facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split())
     }
   }
 }
})
 
 #Length/Freq UI
 output$lengthfrequi= renderUI({
  list( "Total number in the survey",
   if(input$sp1=="Nephrops"){ 
     fluidRow(column(5,list(plotOutput("lfplotall"),paste("Vertical line is the length cut off for",input$sp1, "Juvenile/Adult(",juv_length_split(),"cm)"))),
                                       column(7, plotOutput("lfplotparam"))
   )}
   else{if(dim(LengthDataS())[1]==0){
     h3(paste("No Length data available for",species()))
   }else if(dim(LengthDataS())[1]>0){
     if(is.na(juv_length_split())==FALSE){
     fluidRow(column(5,list(plotOutput("lfplotall"),paste("Vertical line is the length cut off for",input$sp1, "Juvenile/Adult(",juv_length_split(),"cm)"))),
                   column(7, plotOutput("lfplotparam"))
     )}
     else{ fluidRow(column(5,plotOutput("lfplotall")),
                    column(7, plotOutput("lfplotparam"))
     )}
   }})
 })
 
 ##########################
 ### Length/Weight Plot ###
 ##########################
 LengthWeightAgeSp=reactive({
   if(input$sp1=="Nephrops"){
     if(is.null(input$slideryearS1)){
       filter(indLW, Weight_g!="NA" & Year == maxyear)
     }else{
       subset(indLW, Weight_g!="NA" & Year == input$slideryearS1 & Functional_Unit==input$parameterN)
     }
   }
  else{if(is.null(input$slideryearS1)){
     subset(LengthWeightAge,fldFishWholeWeight!="NA" & Year == maxyear & fldMainSpeciesCode%in%speciesFAO())
   }else{
     subset(LengthWeightAge,fldFishWholeWeight!="NA" & Year == input$slideryearS1 & fldMainSpeciesCode%in%speciesFAO())}}
 })
 
 LengthWeightAgeSp1=reactive({
   if(input$sp1=="Nephrops"){filter(indLW, Weight_g!="NA")}
 else {filter(LengthWeightAge, fldFishWholeWeight!="NA",fldMainSpeciesCode%in%speciesFAO())}
 })
 

 
 
 
 output$lwplot=renderPlotly({
   if(input$sp1=="Nephrops"){
     p <- plot_ly(LengthWeightAgeSp(), x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                  text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                              "<br>Sex:",Sex),
                  hoverinfo = 'text',color= ~Sex,
                  marker =list(opacity = 1)) %>%  
       layout(hovermode="closest", title=paste(species(),"Length vs Weight (points coloured by Sex)"),
              xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                           max(LengthWeightAgeSp1()$CLmm)+1)),
              yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = TRUE)
     p
   }
 else {if(input$parameter=="None"){
     LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$length),]
     fit1=lm(log10(fldFishWholeWeight) ~ log10(length), data = LengthWeightAgeSp)
     p=plot_ly(LengthWeightAgeSp, x = ~length, y = ~fldFishWholeWeight, type = 'scatter', marker=list(color='#bdbdbd'),
               text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Weight:",fldFishWholeWeight),
               hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
       layout(hovermode="closest", title=paste("Length vs Weight"),
              xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                           max(LengthWeightAgeSp1()$length)+1)),
              yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = FALSE) %>%
       add_trace(x=~length, y=10^(fitted(fit1)), type="scatter", 
                 mode='lines+markers',
                 line = list(color = 'black', shape="spline"),
                 marker = list(color = 'blue', opacity=0))
     p$elementId <- NULL
   }else if(input$parameter=="Sex"){
     LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$length),]
     fit2=lm(log10(fldFishWholeWeight) ~ log10(length) + obs.sex, data = LengthWeightAgeSp)
     p <- plot_ly(LengthWeightAgeSp, x = ~length, y = ~fldFishWholeWeight, type = 'scatter', 
                  text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Weight:",fldFishWholeWeight, "<br>Sex:",obs.sex),
                  color = ~obs.sex, colors=c('unclassified'='#F8766D','female'='#00BFC4','male'='#B79F00'),
                  hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
       layout(hovermode="closest", title=paste("Length vs Weight (points coloured by sex)"),
              xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                           max(LengthWeightAgeSp1()$length)+1)),
              yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = TRUE) %>%
       add_trace(x=~length, y=10^(fitted(fit2)), type="scatter", 
                 mode='lines+markers',
                 line = list(shape="spline"),
                 marker = list(color = 'grey', opacity=0), showlegend = FALSE)
     p$elementId <- NULL
   }else if(input$parameter=="Gear"){
     p <- plot_ly(LengthWeightAgeSp(), x = ~length, y = ~fldFishWholeWeight, type = 'scatter', mode = 'markers',
                  text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Weight:",fldFishWholeWeight,
                              "<br>Gear type:",fldGearDescription),
                  hoverinfo = 'text',color= ~fldGearDescription, colors=c("#F8766D","#00BFC4"),
                  marker =list(opacity = 0.5)) %>%  
       layout(hovermode="closest", title=paste(species(),"Length vs Weight (points coloured by gear)"),
              xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                           max(LengthWeightAgeSp1()$length)+1)),
              yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = TRUE)
     p$elementId <- NULL
   }else if(input$parameter=="Division"){
     if(is.null(input$division1)){
       ## "Select a Division"
       p=NULL
     }else{
       grspnew.w2 <- filter(LengthWeightAgeSp(), ICESCODE %in% c(input$division1))
       p <- plot_ly(grspnew.w2, x = ~length, y = ~fldFishWholeWeight, type = 'scatter', mode = 'markers',
                    colors=c('VIa'='#F8766D','VIIb'='#00BFC4','VIIc'='#B79F00','VIIg'='#619CFF','VIIj'='#00BA38','VIIk'='#F564E3'),
                    text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Weight:",fldFishWholeWeight, "<br>Division:",ICESCODE),
                    hoverinfo = 'text',color= ~ICESCODE, marker =list(opacity = 0.5)) %>%  
         layout(hovermode="closest", title=paste(species(),"Length vs Weight (points coloured by divisions)"),
                xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                             max(LengthWeightAgeSp1()$length)+1)),
                yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
                margin=(list(t=70)), showlegend = TRUE)
       p$elementId <- NULL
     }
     }
   
   if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p}
   
 })

 
 output$LengthWeightUI=renderUI({if(dim(LengthWeightAgeSp1())[1]==0){
   h3(paste("No Weight data available for", species(), sep=" "))
 }else if(dim(LengthWeightAgeSp())[1]==0){
   h3(paste("No Weight data available for",species(), "for", input$slideryearS1, ".Try another year", sep= " "))
 }else{
   list(plotlyOutput("lwplot"),
        "*Filtering also available on the RHS by clicking on the legend entry when a parameter is chosen")
 }})
 

   
 
 ##########################
 ### Length/Age Plot ###
 ########################## 
 # 
coeffTab=reactive({
   dplyr::filter(coeff_L_A,Cohort=="No",Species%in%speciesFAO(),Parameter==input$parameter,
                 Year==input$slideryearS1)
  
  })
 
output$coeff_table<-DT::renderDT({
    DT::datatable(coeffTab()[c(2,6,7,8)],options =list(dom = 't'))%>%
      formatRound(c("Linf","K","t0"),digits=2)
   })


LengthWeightAgeSpA=reactive({
 if(is.null(input$slideryearS1)){
    subset(LengthWeightAge,age!="NA" & Year == maxyear & fldMainSpeciesCode%in%speciesFAO())
  }else{
    subset(LengthWeightAge,age!="NA" & Year == input$slideryearS1 & fldMainSpeciesCode%in%speciesFAO())}
 })


LengthWeightAgeSpA1=reactive({
  filter(LengthWeightAge, age!="NA",fldMainSpeciesCode%in%speciesFAO())
})
  
  
 Agepred=reactive({
 dplyr::filter(coeff_L_A,Cohort=="No",Species%in%speciesFAO(),Parameter==input$parameter,
                 Year==input$slideryearS1)
 })


  
output$laplot=renderPlotly({
    LWA_fish=LengthWeightAgeSpA()
    a=seq(0,max(LengthWeightAgeSpA1()$age),length.out = 199)
   if(dim(LWA_fish)[1]==0){
      p=NULL
    }else if(input$parameter=="None"){
      pred<-vbTyp(age=a,Linf=Agepred()[["Linf"]],K=Agepred()[["K"]],t0=Agepred()[["t0"]])
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = FALSE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
       if(is.na(Agepred()[["Linf"]]==T)){
        p  
       }
      else{ p = p %>%
        add_trace(x = ~a, y = ~pred, type="scatter", mode = "lines",
                  line = list(shape="spline", color="grey"),name=paste(Agepred()$Data), hoverinfo="none") }
    
     
    }
    else if(input$parameter=="Sex"){
      fem<-filter(Agepred(),Level=="female")
      male<-filter(Agepred(),Level=="male")
      predF<-vbTyp(age=a,Linf=fem[["Linf"]],K=fem[["K"]],t0=fem[["t0"]])
      predM<-vbTyp(age=a,Linf=male[["Linf"]],K=male[["K"]],t0=male[["t0"]])
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~obs.sex, colors=c('unclassified'='#F8766D','female'='#00BFC4','male'='#B79F00'),
                  text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Sex:", obs.sex),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by sex)"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)))
      if(is.na(fem[["Linf"]]==T)){
        p  
      }
      else{ p = p %>%
        add_trace(x = ~a, y = ~predF, type="scatter", mode = "lines",
                  line = list(shape="spline", color='#00BFC4'),name=paste(fem$Data), hoverinfo="none") }
      
      if(is.na(male[["Linf"]]==T)){
        p  
      }
      else{ p = p %>%
        add_trace(x = ~a, y = ~predM, type="scatter", mode = "lines",
                  line = list(shape="spline", color='#B79F00'),name=paste(male$Data), hoverinfo="none") }
    
      
    }
    else if(input$parameter=="Gear"){
      gearA<-filter(Agepred(),Level=="GOV 3647 Groundgear A")
      gearD<-filter(Agepred(),Level=="GOV 3647 Groundgear D")
      predGA<-vbTyp(age=a,Lin=gearA[["Linf"]],K=gearA[["K"]],t0=gearA[["t0"]])
      predGD<-vbTyp(age=a,Linf=gearD[["Linf"]],K=gearD[["K"]],t0=gearD[["t0"]])
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~fldGearDescription, colors=c("#F8766D","#00BFC4"),
                  text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Gear type:",fldGearDescription),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>%
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by gear)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                            max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                            zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
      if(is.na(gearA[["Linf"]]==T)){
        p  
      }
      else{ p = p %>%
        add_trace(x = ~a, y = ~predGA, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#F8766D"),name=paste(gearA$Data), hoverinfo="none") }
      
      if(is.na(gearD[["Linf"]]==T)){
        p  
      }
      else{ p = p %>%
        add_trace(x = ~a, y = ~predGD, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#00BFC4"),name=paste(gearD$Data), hoverinfo="none") }
      
      
    
    }
    else if(input$parameter=="Division"){
     dVIa<-filter(Agepred(),Level=="VIa")
     dVIIb<-filter(Agepred(),Level=="VIIb")
     dVIIc<-filter(Agepred(),Level=="VIIc")
     dVIIg<-filter(Agepred(),Level=="VIIg")
     dVIIj<-filter(Agepred(),Level=="VIIj")
     dVIIk<-filter(Agepred(),Level=="VIIk")
     predVIa<-vbTyp(age=a,Linf=dVIa[["Linf"]],K=dVIa[["K"]],t0=dVIa[["t0"]])
     predVIIb<-vbTyp(age=a,Linf=dVIIb[["Linf"]],K=dVIIb[["K"]],t0=dVIIb[["t0"]])
     predVIIc<-vbTyp(age=a,Linf=dVIIc[["Linf"]],K=dVIIc[["K"]],t0=dVIIc[["t0"]])
     predVIIg<-vbTyp(age=a,Linf=dVIIg[["Linf"]],K=dVIIg[["K"]],t0=dVIIg[["t0"]])
     predVIIj<-vbTyp(age=a,Linf=dVIIj[["Linf"]],K=dVIIj[["K"]],t0=dVIIj[["t0"]])
     predVIIk<-vbTyp(age=a,Linf=dVIIk[["Linf"]],K=dVIIk[["K"]],t0=dVIIk[["t0"]])
     p=plot_ly() %>% 
            add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",color= ~ICESCODE,
                      colors=c('VIa'='#F8766D','VIIb'='#00BFC4','VIIc'='#B79F00','VIIg'='#619CFF','VIIj'='#00BA38','VIIk'='#F564E3'),
                      text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Division:",ICESCODE),
                      hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
            layout(hovermode="closest", title=paste("Age vs Length (points coloured by divisions)"),
                   yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                                max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
                   xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                                zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
     if(dim(dVIa)[1]==0){p}
     else{
       if(is.na(dVIa[["Linf"]]==T)){p}
       else{p = p %>%
               add_trace(x = ~a, y = ~predVIa, type="scatter", mode = "lines",
                          line = list(shape="spline", color='#F8766D'),name=paste(dVIa$Data), hoverinfo="none")
       }}
     if(dim(dVIIb)[1]==0){p}
     else{
       if(is.na(dVIIb[["Linf"]]==T)){p}
       else{p = p %>%
               add_trace(x = ~a, y = ~predVIIb, type="scatter", mode = "lines",
                          line = list(shape="spline", color='#00BFC4'),name=paste(dVIIb$Data), hoverinfo="none")
       }}
     if(dim(dVIIc)[1]==0){p}
     else{
       if(is.na(dVIIc[["Linf"]]==T)){p}
       else{
         p = p %>%
              add_trace(x = ~a, y = ~predVIIc, type="scatter", mode = "lines",
                        line = list(shape="spline", color='#B79F00'),name=paste(dVIIc$Data), hoverinfo="none")  
          }} 
     if(dim(dVIIg)[1]==0){p}
     else{
       if(is.na(dVIIg[["Linf"]]==T)){p}
       else{
            p = p %>%
               add_trace(x = ~a, y = ~predVIIg, type="scatter", mode = "lines",
                         line = list(shape="spline", color='#619CFF'),name=paste(dVIIg$Data), hoverinfo="none")
            }}
     if(dim(dVIIj)[1]==0){p}
     else{
       if(is.na(dVIIj[["Linf"]]==T)){p}
       else{p = p %>%
               add_trace(x = ~a, y = ~predVIIj, type="scatter", mode = "lines",
                         line = list(shape="spline", color='#00BA38'),name=paste(dVIIj$Data), hoverinfo="none")  
           }}
     if(dim(dVIIk)[1]==0){p}
     else{
       if(is.na(dVIIk[["Linf"]]==T)){p}
       else{p = p %>%
               add_trace(x = ~a, y = ~predVIIk, type="scatter", mode = "lines",
                          line = list(shape="spline", color='#F564E3'),name=paste(dVIIk$Data), hoverinfo="none")}}  
           
          
    }
 })


output$latab=renderUI({
  
  if(dim(LengthWeightAgeSpA1())[1]==0){
    h3(paste("No Age data available for",species(), sep=" "))
  }else if(dim(LengthWeightAgeSpA())[1]==0){
    h3(paste("No Age data available for",species(), "for", input$slideryearS1, ".Try another year", sep= " "))
  }else{
    list(
      plotlyOutput("laplot"),
      fluidRow(column(5,h4("Life Parameters"),
                      DT::DTOutput("coeff_table"),offset = 3)),
      "Data fits are modelled using Von Bertalanffy Growth Models",HTML("<br>"),
      "If data fits are missing this is due to either no data available or small sample sizes",HTML("<br>"),
      "*Filtering also available on the RHS by clicking on the legend entry when a parameter is chosen")
  }
  
})
 
 ################
 ######Cohort####
 ################
 coeffTabC=reactive({
   dplyr::filter(coeff_L_A,Cohort=="Yes",Species%in%speciesFAO(),Parameter==input$parameter,
                 is.na(Year)|Year==input$slideryearS1)
   
 })
 
 output$coeff_table_cohort<-DT::renderDataTable({
   DT::datatable(coeffTabC()[c(2,6,7,8)],options = list(dom = 't'))%>%
     formatRound(c("Linf","K","t0"),digits=2)
 })
 
 LengthWeightAgeSpAC=reactive({if(is.null(input$slideryearS1)){
   subset(LengthWeightAge,age!="NA" & Cohort == maxyear & fldMainSpeciesCode%in%speciesFAO())
 }else{
   subset(LengthWeightAge,age!="NA" & Cohort == input$slideryearS1 & fldMainSpeciesCode%in%speciesFAO())}
 })
 
 
 
 AgepredC<-reactive({dplyr::filter(coeff_L_A,Cohort=="Yes",Species%in%speciesFAO(),Parameter==input$parameter,
               Year==input$slideryearS1)}) 
 
 output$cohortplot=renderPlotly({
   LWA_fish=LengthWeightAgeSpAC()
   a=seq(0,max(LengthWeightAgeSpA1()$age),length.out = 199)
   if(dim(LWA_fish)[1]==0){
     p=NULL
   }else if(input$parameter=="None"){
     pred<-vbTyp(age=a,Linf=AgepredC()[["Linf"]],K=AgepredC()[["K"]],t0=AgepredC()[["t0"]])
     p=plot_ly() %>% 
       add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                 text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age),
                 hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = FALSE) %>% 
       layout(hovermode="closest", title=paste("Length vs Age"),
              xaxis = list(title = 'Age (years)', zeroline=FALSE,
                           range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
              yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                           range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = TRUE)
     if(is.na(AgepredC()[["Linf"]]==T)){
       p  
     }
     else{ p = p %>%
       add_trace(x = ~a, y = ~pred, type="scatter", mode = "lines",
                 line = list(shape="spline", color="grey"),name=paste(AgepredC()$Data), hoverinfo="none") }
     
     
   }
   else if(input$parameter=="Sex"){
     fem<-filter(AgepredC(),Level=="female")
     male<-filter(AgepredC(),Level=="male")
     predF<-vbTyp(age=a,Linf=fem[["Linf"]],K=fem[["K"]],t0=fem[["t0"]])
     predM<-vbTyp(age=a,Linf=male[["Linf"]],K=male[["K"]],t0=male[["t0"]])
     p=plot_ly() %>% 
       add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                 color = ~obs.sex, colors=c('unclassified'='#F8766D','female'='#00BFC4','male'='#B79F00'),
                 text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Sex:", obs.sex),
                 hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
       layout(hovermode="closest", title=paste("Length vs Age (points coloured by sex)"),
              xaxis = list(title = 'Age (years)', zeroline=FALSE,
                           range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
              yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                           range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
              margin=(list(t=70)))
     if(is.na(fem[["Linf"]]==T)){
       p  
     }
     else{ p = p %>%
       add_trace(x = ~a, y = ~predF, type="scatter", mode = "lines",
                 line = list(shape="spline", color='#00BFC4'),name=paste(fem$Data), hoverinfo="none") }
     
     if(is.na(male[["Linf"]]==T)){
       p  
     }
     else{ p = p %>%
       add_trace(x = ~a, y = ~predM, type="scatter", mode = "lines",
                 line = list(shape="spline", color='#B79F00'),name=paste(male$Data), hoverinfo="none") }
     
     
   }
   else if(input$parameter=="Gear"){
     gearA<-filter(AgepredC(),Level=="GOV 3647 Groundgear A")
     gearD<-filter(AgepredC(),Level=="GOV 3647 Groundgear D")
     predGA<-vbTyp(age=a,Lin=gearA[["Linf"]],K=gearA[["K"]],t0=gearA[["t0"]])
     predGD<-vbTyp(age=a,Linf=gearD[["Linf"]],K=gearD[["K"]],t0=gearD[["t0"]])
     p=plot_ly() %>% 
       add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                 color = ~fldGearDescription, colors=c("#F8766D","#00BFC4"),
                 text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Gear type:",fldGearDescription),
                 hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>%
       layout(hovermode="closest", title=paste("Length vs Age (points coloured by gear)"),
              yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                           max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
              xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                           zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
     if(is.na(gearA[["Linf"]]==T)){
       p  
     }
     else{ p = p %>%
       add_trace(x = ~a, y = ~predGA, type="scatter", mode = "lines",
                 line = list(shape="spline", color="#F8766D"),name=paste(gearA$Data), hoverinfo="none") }
     
     if(is.na(gearD[["Linf"]]==T)){
       p  
     }
     else{ p = p %>%
       add_trace(x = ~a, y = ~predGD, type="scatter", mode = "lines",
                 line = list(shape="spline", color="#00BFC4"),name=paste(gearD$Data), hoverinfo="none") }
     
     
     
   }
   else if(input$parameter=="Division"){
     dVIa<-filter(AgepredC(),Level=="VIa")
     dVIIb<-filter(AgepredC(),Level=="VIIb")
     dVIIc<-filter(AgepredC(),Level=="VIIc")
     dVIIg<-filter(AgepredC(),Level=="VIIg")
     dVIIj<-filter(AgepredC(),Level=="VIIj")
     dVIIk<-filter(AgepredC(),Level=="VIIk")
     predVIa<-vbTyp(age=a,Linf=dVIa[["Linf"]],K=dVIa[["K"]],t0=dVIa[["t0"]])
     predVIIb<-vbTyp(age=a,Linf=dVIIb[["Linf"]],K=dVIIb[["K"]],t0=dVIIb[["t0"]])
     predVIIc<-vbTyp(age=a,Linf=dVIIc[["Linf"]],K=dVIIc[["K"]],t0=dVIIc[["t0"]])
     predVIIg<-vbTyp(age=a,Linf=dVIIg[["Linf"]],K=dVIIg[["K"]],t0=dVIIg[["t0"]])
     predVIIj<-vbTyp(age=a,Linf=dVIIj[["Linf"]],K=dVIIj[["K"]],t0=dVIIj[["t0"]])
     predVIIk<-vbTyp(age=a,Linf=dVIIk[["Linf"]],K=dVIIk[["K"]],t0=dVIIk[["t0"]])
     p=plot_ly() %>% 
       add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",color= ~ICESCODE,
                 colors=c('VIa'='#F8766D','VIIb'='#00BFC4','VIIc'='#B79F00','VIIg'='#619CFF','VIIj'='#00BA38','VIIk'='#F564E3'),
                 text=~paste("Species:",species(),"<br>Length:",length,"cm","<br>Age:",age, "<br>Division:",ICESCODE),
                 hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
       layout(hovermode="closest", title=paste("Age vs Length (points coloured by divisions)"),
              yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                           max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
              xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                           zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
     if(dim(dVIa)[1]==0){p}
     else{
       if(is.na(dVIa[["Linf"]]==T)){p}
       else{p = p %>%
         add_trace(x = ~a, y = ~predVIa, type="scatter", mode = "lines",
                   line = list(shape="spline", color='#F8766D'),name=paste(dVIa$Data), hoverinfo="none")
       }}
     if(dim(dVIIb)[1]==0){p}
     else{
       if(is.na(dVIIb[["Linf"]]==T)){p}
       else{p = p %>%
         add_trace(x = ~a, y = ~predVIIb, type="scatter", mode = "lines",
                   line = list(shape="spline", color='#00BFC4'),name=paste(dVIIb$Data), hoverinfo="none")
       }}
     if(dim(dVIIc)[1]==0){p}
     else{
       if(is.na(dVIIc[["Linf"]]==T)){p}
       else{
         p = p %>%
           add_trace(x = ~a, y = ~predVIIc, type="scatter", mode = "lines",
                     line = list(shape="spline", color='#B79F00'),name=paste(dVIIc$Data), hoverinfo="none")  
       }} 
     if(dim(dVIIg)[1]==0){p}
     else{
       if(is.na(dVIIg[["Linf"]]==T)){p}
       else{
         p = p %>%
           add_trace(x = ~a, y = ~predVIIg, type="scatter", mode = "lines",
                     line = list(shape="spline", color='#619CFF'),name=paste(dVIIg$Data), hoverinfo="none")
       }}
     if(dim(dVIIj)[1]==0){p}
     else{
       if(is.na(dVIIj[["Linf"]]==T)){p}
       else{p = p %>%
         add_trace(x = ~a, y = ~predVIIj, type="scatter", mode = "lines",
                   line = list(shape="spline", color='#00BA38'),name=paste(dVIIj$Data), hoverinfo="none")  
       }}
     if(dim(dVIIk)[1]==0){p}
     else{
       if(is.na(dVIIk[["Linf"]]==T)){p}
       else{p = p %>%
         add_trace(x = ~a, y = ~predVIIk, type="scatter", mode = "lines",
                   line = list(shape="spline", color='#F564E3'),name=paste(dVIIk$Data), hoverinfo="none")}}  
     
     
   }
 })
 
 
 
output$cohorttab=renderUI({
   if(dim(LengthWeightAgeSpA1())[1]==0){
     h3(paste("No Age data available for",species() , sep=" "))
   }else if(dim(LengthWeightAgeSpA())[1]==0){
     h3(paste("No Age data available for", species(), "for", input$slideryearS1, ".Try another year", sep= " "))
   }else{
     list(
       plotlyOutput("cohortplot"),
       fluidRow(column(5,h4("Life Parameters"),
                       DT::dataTableOutput("coeff_table_cohort"),offset = 3)),
       
       "Data fits are modelled using Von Bertalanffy Growth Models",HTML("<br>"),
       "If data fits are missing this is due to either no data available or small sample sizes",HTML("<br>"),
       "*Filtering also available on the RHS by clicking on the legend entry when a parameter is chosen")
   }
 })
 
 #####################
 ### Download Data ###
 #####################
 output$downloadstation_data <- downloadHandler(
   filename = function() {
     paste("Station_data",".csv", sep = "")
   },
   content = function(file) {
     write.csv(stn, file, row.names = FALSE)
   },
   contentType = "application/csv"
 )

output$downloadData_L <- downloadHandler(
  filename = function() {
    
    paste(speciesFAO(), "_Length_data",".csv", sep = "")
  },
  content = function(file) {
    write.csv(LengthDataS(), file, row.names = FALSE)
  },
  contentType = "application/csv"
)
output$Ldownload=renderUI({
  if(input$tabselected=="lf" & input$sp1!="Nephrops"){
    downloadButton("downloadData_L", "Download Length data")
  }
})
 
 
 output$downloadData_LWA <- downloadHandler(
   filename = function() {
     
     paste(speciesFAO(), "_Length_Weight_Age_data",".csv", sep = "")
   },
   content = function(file) {
     write.csv(LengthWeightAgeSp1(), file, row.names = FALSE)
   },
   contentType = "application/csv"
 )
 output$LWAdownload=renderUI({
   if(input$tabselected=="lw"  | input$tabselected=="la" | input$tabselected=="co"){
     downloadButton("downloadData_LWA", "Download Length Weight Age data")
   }
 })
 
 LifeP=reactive({
   if(input$tabselected=="la"){
   dplyr::filter(coeff_L_A,Cohort=="No",Species%in%speciesFAO())}
   else if (input$tabselected=="co"){
     dplyr::filter(coeff_L_A,Cohort=="Yes",Species%in%speciesFAO()) 
   }
   
 })
 output$downloadData_LP <- downloadHandler(
   filename = function() {
     
     paste(speciesFAO(), "_Life_Parameters",".csv", sep = "")
   },
   content = function(file) {
     write.csv(LifeP(), file, row.names = FALSE)
   },
   contentType = "application/csv"
 )
 output$LPdownload=renderUI({
   if(input$tabselected=="la" | input$tabselected=="co"){
     downloadButton("downloadData_LP", "Download Life parameters")
   }
 })
 
 mapd=reactive({
   subset(mapdataS, No_km2>0 & Species%in%speciesFAO())
 }) 
 output$downloadData_map <- downloadHandler(
   filename = function() {
     paste(speciesFAO(), "_Map_data",".csv", sep = "")
   },
   content = function(file) {
     if(input$sp1=="Nephrops"){write.csv(dat_raised, file, row.names = FALSE)}
     else{write.csv(mapd(), file, row.names = FALSE)}
   },
   contentType = "application/csv"
 )
 
 




}
