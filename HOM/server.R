#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

########################
### Reading in files ###
########################
species="Horse Mackerel"
speciesFAO="HOM"
### Get Data - pull out valid stns
# Used for Catch rate and CPUE
dat=readRDS("dat.RDS")
dat$symbSize <- sqrt( dat$Kg_Per_Hr/ pi )
dat$Year = as.numeric(substr(dat$Cruise,5,8))
#dat$fldSex=revalue(dat$fldSex, c("f"="F"))

### Get Data - pull out valid stns (grep & grepl are pattern matching functions, the latter ignores case)
stn=readRDS("stn.RDS")

# Read in shapefiles
div <- geojsonio::geojson_read("div_simple.geojson", what = "sp")
cont <- geojsonio::geojson_read("cont1_simple.geojson", what = "sp")

#Used for Distribution no/km2 and Abundance
data1=readRDS("data1.RDS")
##calculating swept area estimates
catch_km2<-with(data1,(1/AreaKmSq*CatchKg))
No_km2<-with(data1,(1/AreaKmSq*RaisedNo))
Area<-ifelse(data1$ICESCODE=="VIa","VI","VII")
data1<-cbind(data1,catch_km2,No_km2, Area)
#Agregate by Haul and species to get unique haul data for skt
mapdata<-aggregate(data1[,c("CatchKg", "RaisedNo", "catch_km2", "No_km2")], 
                   by=list(data1$Yr,data1$Haul,data1$LonDec, data1$LatDec),FUN=sum,  na.rm=TRUE)
names(mapdata) = c("Year", "Haul", "LonDec", "LatDec", "CatchKg", "RaisedNo", "catch_km2", "No_km2")
mapdata$symbSize <- sqrt( mapdata$No_km2/ pi )

#Length/Weight and Length/Age plots
LengthWeightAge=readRDS("LengthWeightAge.RDS")

#Used for Length Frequency and Total/Adults/Juvenile numbers
LengthData=readRDS("LengthData.RDS")
LengthData$CatchNos30minHaul=LengthData$CatchNos/LengthData$fldTowDuration*30

#Total Numbers
TotalNumbersMap=aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                          by=list(LengthData$Year,LengthData$fldCruiseStationNumber,
                                  LengthData$fldShotLonDecimalDegrees, LengthData$fldShotLatDecimalDegrees),
                          FUN=sum,  na.rm=TRUE)
names(TotalNumbersMap) = c("Year", "Haul", "LonDec", "LatDec", "CatchNos", "CatchNos30minHaul")
TotalNumbersMap$symbSize <- sqrt( TotalNumbersMap$CatchNos30minHaul/ pi )


#Juveniles
Juveniles= filter(LengthData, AgeClassification=="Juvenile")
JuvNumbersMap=aggregate(Juveniles[,c("CatchNos", "CatchNos30minHaul")],
                        by=list(Juveniles$Year,Juveniles$fldCruiseStationNumber,
                                Juveniles$fldShotLonDecimalDegrees, Juveniles$fldShotLatDecimalDegrees),
                        FUN=sum,  na.rm=TRUE)
names(JuvNumbersMap) = c("Year", "Haul", "LonDec", "LatDec", "CatchNos", "CatchNos30minHaul")
JuvNumbersMap$symbSize <- sqrt( JuvNumbersMap$CatchNos30minHaul/ pi )


#Adults
Adults= filter(LengthData, AgeClassification=="Adult")
AdultNumbersMap=aggregate(Adults[,c("CatchNos", "CatchNos30minHaul")],
                          by=list(Adults$Year,Adults$fldCruiseStationNumber,
                                  Adults$fldShotLonDecimalDegrees, Adults$fldShotLatDecimalDegrees),
                          FUN=sum,  na.rm=TRUE)
names(AdultNumbersMap) = c("Year", "Haul", "LonDec", "LatDec", "CatchNos", "CatchNos30minHaul")
AdultNumbersMap$symbSize <- sqrt( AdultNumbersMap$CatchNos30minHaul/ pi )


#Coefficents
coeff_all=readRDS("coeffs/coeff_all.RDS")
coeff_year=readRDS("coeffs/coeff_year.RDS")
coeff_sex=readRDS("coeffs/coeff_sex.RDS")
coeff_sex_year=readRDS("coeffs/coeff_sex_year.RDS")
coeff_div=readRDS("coeffs/coeff_div.RDS")
coeff_div_year=readRDS("coeffs/coeff_div_year.RDS")
coeff_gear=readRDS("coeffs/coeff_gear.RDS")
coeff_gear_year=readRDS("coeffs/coeff_gear_year.RDS")
coeff_cohort=readRDS("coeffs/coeff_cohort.RDS")
coeff_sex_cohort=readRDS("coeffs/coeff_sex_cohort.RDS")
coeff_gear_cohort=readRDS("coeffs/coeff_gear_cohort.RDS")
coeff_div_cohort=readRDS("coeffs/coeff_cohort_div.RDS")

#vbgm function
vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))



# Define server logic required
shinyServer(function(input, output) {
  ##########################
  ### Reactive filtering ###
  ##########################
  #For Length/Weight and Length/Age plots
  output$yearfilter=renderUI({
    if(input$tabselected=="lw"  | input$tabselected=="la" | input$tabselected=="co"){
      sliderInput("slideryear1", "Choose Year:", min = 2003, max = 2017, value = 2017, step = NULL, 
                  sep = "", animate = TRUE)
    }
  })
  
  #Divfilter only appears if Division parameter selected
  output$divfilter=renderUI({
    if(input$parameter=="Division"){
       divlist= factor(as.character(unique(dat$ICESCODE)))
       checkboxGroupInput("division1", h3("Select Division"),choices=as.list(sort(divlist)))}
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
  
  
  output$downloadData_map <- downloadHandler(
    filename = function() {
      paste(speciesFAO, "_Map_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat, file, row.names = FALSE)
    },
    contentType = "application/csv"
  )
  

  
  output$downloadData_LWA <- downloadHandler(
    filename = function() {
      #lwa.data.full<- readRDS("LengthWeightAge.RDS")
      paste(speciesFAO, "_Length_Weight_Age_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(LengthWeightAge, file, row.names = FALSE)
    },
    contentType = "application/csv"
    )
  
  output$LWAdownload=renderUI({
    if(input$tabselected=="lw"  | input$tabselected=="la" | input$tabselected=="co"){
      downloadButton("downloadData_LWA", "Download Length Weight Age data")
    }
  })

  
  ######################
  ### Filtering data ###
  ######################
  juv_length_split=LengthData$preRecruitLength[1]
  
  cat=reactive({
    dplyr::filter(dat, Cruise %in% paste0('IGFS', input$slideryear))
  })
  
  haul = reactive({
    dplyr::filter(stn, fldCruiseName==paste0('IGFS',  input$slideryear))
  })
  
  mapdata1=reactive({
    subset(mapdata, Year %in% input$slideryear & No_km2>0)
  })
  
  TotalNumbers=reactive({
    dplyr::filter(TotalNumbersMap, Year %in% input$slideryear & CatchNos30minHaul>0)
  })
  
  JuvNumbers=reactive({
    dplyr::filter(JuvNumbersMap, Year %in% input$slideryear & CatchNos30minHaul>0)
  })
  
  AdultNumbers=reactive({
    dplyr::filter(AdultNumbersMap, Year %in% input$slideryear & CatchNos30minHaul>0)
  })
  
  
  ###############
  ### Mapping ###
  ###############
  html_legend <- "<img src='x-mark-16.png'>Stations Surveyed"
  
  output$mymap <- renderLeaflet({
    mymap <-leaflet() %>%
      setView(lng = -9, lat = 53, zoom = 6) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
      addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)%>%
      #addControl(html = html_legend, position = "bottomright")%>%
      addLegend("bottomright",colors = c("blue","purple", "black", "yellow", "green"), 
                labels = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>",  "Total No of Fish per 30 min Haul", 
                           "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"))%>%
      hideGroup("Stations Surveyed")
    
    return(mymap)
  })
  
  icon.ship <- makeIcon(iconUrl  = 'x-mark-16.png', iconHeight = 7, iconWidth = 7)

  observe({
    new_zoom <- input$mymap_zoom
    leafletProxy('mymap') %>%
      clearGroup(group =  c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>", "Stations Surveyed", 
                            "Total No of Fish per 30 min Haul", "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"))%>%
      #clearShapes() %>%
      #addCircles(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, radius=10000/new_zoom, 
      #color = "grey", group="Stations Surveyed", popup=paste("Station: ",haul()$fldPrimeStation, "<br />", 
      #"Gear Type: ",haul()$Gear_Type)) %>%
      addMarkers(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, icon =icon.ship, 
                 group="Stations Surveyed", 
                 popup=paste("<b>Station:</b> ",haul()$fldPrimeStation, "<br />", "<b>Gear Type:</b> ",haul()$Gear_Type,"<br />", 
                             "<b>Tow Duration:</b> ",haul()$TowDurationMin, "mins", "<br />", "<b>Door Spread:</b> ",haul()$DoorSpread, "<br />",  
                             "<b>Wing Spread:</b> ",haul()$fldWingSpread, "<br />", "<b>Headline Height:</b> ",haul()$fldHeadlineHeight, "<br />",  
                             "<b>Distance KM:</b> ",round(haul()$Dist_Km,2),"<br />", "<b>Stratum:</b> ", haul()$fldStratum)) %>%
      addCircles(lng=cat()$Lon, lat=cat()$Lat, radius=10000*cat()$symbSize/new_zoom, group="Catch Rate kg/hr", 
                 popup=paste("<b>Cruise:</b> ",cat()$Cruise, "<br />", "<b>Haul</b>: ",cat()$Haul, "<br />", 
                             "<b>Station:</b> ",cat()$Prime_Stn, "<br />", "<b>Catch Kg/Hr:</b> ", round(cat()$Kg_Per_Hr,2))) %>%
      addCircles(lng=mapdata1()$LonDec, lat=mapdata1()$LatDec, radius=2000*mapdata1()$symbSize/new_zoom, color = "purple",  
                 group="Distribution No/km<sup>2</sup>", popup=paste("<b>Haul:</b> ",mapdata1()$Haul, "<br />", 
                                                                     "<b>No per km<sup>2</sup>:</b> ", round(mapdata1()$No_km2,0)))  %>%
      addCircles(lng=TotalNumbers()$LonDec, lat=TotalNumbers()$LatDec, radius=5000*TotalNumbers()$symbSize/new_zoom, 
                 color = "black",  group="Total No of Fish per 30 min Haul", 
                 popup=paste("<b>Haul:</b> ",TotalNumbers()$fldCruiseStationNumber, "<br />", "<b>No of Fish per 30 min Haul:</b> ",
                             round(TotalNumbers()$CatchNos30minHaul,0)))  %>%
      addCircles(lng=JuvNumbers()$LonDec, lat=JuvNumbers()$LatDec, radius=5000*JuvNumbers()$symbSize/new_zoom, 
                 color = "yellow",  group="No of Juvenile Fish per 30 min Haul", 
                 popup=paste("<b>Haul:</b> ",JuvNumbers()$Haul, "<br />", "<b>No of Juvenile Fish per 30 min Haul:</b> ",
                             round(JuvNumbers()$CatchNos30minHaul,0)))  %>%
      addCircles(lng=AdultNumbers()$LonDec, lat=AdultNumbers()$LatDec, radius=5000*AdultNumbers()$symbSize/new_zoom, 
                 color = "green",  group="No of Adult Fish per 30 min Haul", 
                 popup=paste("<b>Haul:</b> ",AdultNumbers()$Haul, "<br />", "<b>No of Adult Fish per 30 min Haul:</b> ",
                             round(AdultNumbers()$CatchNos30minHaul,0)))  %>%
      addLayersControl(
        baseGroups = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>","Total No of Fish per 30 min Haul", 
                       "No of Juvenile Fish per 30 min Haul", "No of Adult Fish per 30 min Haul"),
        overlayGroups = c("Stations Surveyed"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  
  ############
  ### CPUE ###
  ############
  output$cpueplotall=renderPlotly({
    catchAll <- aggregate(list(KgHr=dat$Kg_Per_Hr), list(Cruise=dat$Cruise, Year= dat$Year),mean, na.rm=TRUE)
    p=ggplot(dat, aes(x=Year, y=Kg_Per_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +
      theme_bw() + theme(legend.position = "none")
    ggplotly(p)
    })
  
  output$cpueplotparam=renderPlotly({
    if(input$parameter=="None"){
      p=NULL
    }else if(input$parameter=="Sex"){
      catchSex <- aggregate(list(KgHr=dat$Kg_Per_Hr), 
                            list(Cruise=dat$Cruise, Year=dat$Year, fldSex=dat$fldSex), mean, na.rm=TRUE)
      p=ggplot(dat, aes(x=Year, y=Kg_Per_Hr, colour=fldSex)) + geom_jitter(width = 0.05) + 
        geom_line(data=catchSex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +
        facet_wrap(~fldSex) + theme_bw() + theme(legend.position = "none")
    }else if(input$parameter=="Gear"){
      catchGear <- aggregate(list(KgHr=dat$Kg_Per_Hr), 
                             list(Cruise=dat$Cruise, Year= dat$Year, GearDescription=dat$GearDescription), mean, na.rm=TRUE)
      p=ggplot(dat, aes(x=Year, y=Kg_Per_Hr, colour=GearDescription)) + geom_jitter(width = 0.05) + 
        geom_line(data=catchGear, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +
        facet_wrap(~GearDescription) + theme_bw() + theme(legend.position = "none")
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        p=NULL
      }else{
        cpuebydiv=filter(dat, ICESCODE %in% c(input$division1))
        catchArea <- aggregate(list(KgHr=cpuebydiv$Kg_Per_Hr), 
                               list(Cruise=cpuebydiv$Cruise, Year= cpuebydiv$Year,  
                                    ICESCODE=cpuebydiv$ICESCODE), mean, na.rm=TRUE)
        p=ggplot(cpuebydiv, aes(x=Year, y=Kg_Per_Hr, colour=ICESCODE)) + geom_jitter(width = 0.05) + 
          geom_line(data=catchArea, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +
          facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else ggplotly(p)
  })
  
  #################
  ### Abundance ###
  #################
  output$abundanceplotall=renderPlotly({
    #Order the data
    meanAll<-aggregate(data1[,c("catch_km2","No_km2")],by=list(data1$Yr),FUN=mean,  na.rm=TRUE) 
    names(meanAll)=c("Year", "catch_km2", "No_km2")
    p=ggplot(data1, aes(x=Yr, y=No_km2)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=meanAll, aes(x=Year, y =No_km2), size=1)+ 
      ylab("No/KM<sup>2</sup>")+ xlab("Year") +
      theme_bw() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$abundanceplotparam=renderPlotly({
    if(input$parameter=="None"){
      p=NULL
    }else if(input$parameter=="Sex"){
      meanSex<-aggregate(data1[,c("catch_km2","No_km2")], by=list(data1$Yr, data1$Sex),FUN=mean,  na.rm=TRUE)
      names(meanSex)=c("Year", "Sex", "catch_km2", "No_km2")
      p=ggplot(data1,aes(x=Yr, y=No_km2, colour=Sex)) +geom_jitter(width=0.05) + 
        geom_line(data=meanSex, aes(x=Year, y =No_km2), size=1)+
        labs(y = "No/KM<sup>2</sup>", x="Year") +
        facet_wrap(~Sex) + theme_bw() + theme(legend.position = "none")
    }else if(input$parameter=="Gear"){
      meanGear<-aggregate(data1[,c("catch_km2","No_km2")],
                          by=list(data1$Yr, data1$fldGearDescription),FUN=mean,  na.rm=TRUE)
      names(meanGear)=c("Year", "fldGearDescription", "catch_km2", "No_km2")
      p=ggplot(data1,aes(x=Yr, y=No_km2, colour=fldGearDescription)) +geom_jitter(width=0.05) + 
        geom_line(data=meanGear, aes(x=Year, y =No_km2), size=1)+
        labs(y = "No/KM<sup>2</sup>", x="Year") +
        facet_wrap(~fldGearDescription) + theme_bw() + theme(legend.position = "none")
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        abundancebydiv=filter(data1, ICESCODE %in% c(input$division1))
        meanDiv<-aggregate(abundancebydiv[,c("catch_km2","No_km2")], 
                           by=list(abundancebydiv$Yr,abundancebydiv$ICESCODE), FUN=mean, na.rm=TRUE)
        names(meanDiv)=c("Year", "ICESCODE", "catch_km2", "No_km2")
        p=ggplot(abundancebydiv, aes(x=Yr, y=No_km2, colour=ICESCODE)) + geom_jitter(width = 0.05) + 
          geom_line(data=meanDiv, aes(x=Year, y =No_km2), size=1)+
          labs(y = "No/KM<sup>2</sup>", x="Year") +
          facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else ggplotly(p)
  })
  
  
  ########################
  ### Length Frequency ###
  ########################
  # Get recent data for current species
  output$lfplotall=renderPlot({
    lftotal<-aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                       by=list(LengthData$Year,LengthData$LengthClass),FUN=sum,  na.rm=TRUE)
    names(lftotal)=c("Year", "LengthClass", "CatchNos", "CatchNos30minHaul")
    ggplot(lftotal, aes(LengthClass, Year, height = CatchNos, group = Year, alpha=.5)) + 
      geom_density_ridges(stat = "identity", scale = 2.6) + 
      labs(y = "Year", x="Length Class") +
      theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split)
  })
  
  output$lfplotparam=renderPlot({
    if(input$parameter=="None"){
    }else if(input$parameter=="Sex"){
      lfsex<-aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                       by=list(LengthData$Year,LengthData$LengthClass, LengthData$fldSex),FUN=sum,  na.rm=TRUE)
      names(lfsex)=c("Year", "LengthClass", "fldSex", "CatchNos", "CatchNos30minHaul")
      ggplot(lfsex, aes(LengthClass, Year, height = CatchNos, fill=fldSex, group = Year, alpha=.5)) + 
        geom_density_ridges(stat = "identity", scale = 2.6) + 
        labs(y = "Year", x="Length Class") +
        facet_wrap(~fldSex) + theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split)
    }else if(input$parameter=="Gear"){
      lfgear<-aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                        by=list(LengthData$Year,LengthData$LengthClass, LengthData$fldGearDescription),FUN=sum,  na.rm=TRUE)
      names(lfgear)=c("Year", "LengthClass", "fldGearDescription", "CatchNos", "CatchNos30minHaul")
      AllOptions=expand(lfgear, Year, fldGearDescription, LengthClass)
      MakingZeros=left_join(AllOptions, lfgear, by=c("Year", "fldGearDescription", "LengthClass"))
      MakingZeros$CatchNos[is.na(MakingZeros$CatchNos)] <- 0
      MakingZeros$CatchNos30minHaul[is.na(MakingZeros$CatchNos30minHaul)] <- 0
      ggplot(MakingZeros, aes(LengthClass, Year, height = CatchNos, fill=fldGearDescription,  group = Year, alpha=.5)) + 
        geom_density_ridges(stat = "identity", scale = 2.6) + labs(y = "Year", x="Length Class") +
        facet_wrap(~fldGearDescription) + theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split)
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
      }else{
        lfdiv<-aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                         by=list(LengthData$Year,LengthData$LengthClass, LengthData$ICESCODE),FUN=sum,  na.rm=TRUE)
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
          labs(y = "Year", x="Length Class") +
          facet_wrap(~ICESCODE) + theme_bw() + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split)
      }
    }
  })
  
  ##########################
  ### Length/Weight Plot ###
  ##########################
  LengthWeightAgeSp=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAge, fldFishWholeWeight!="NA" & Year == 2017)
    }else{
      filter(LengthWeightAge, fldFishWholeWeight!="NA" & Year == input$slideryear1)}
    })
  
  LengthWeightAgeSp1=reactive({
    filter(LengthWeightAge, fldFishWholeWeight!="NA")
  })
  
  output$lwplot=renderPlotly({
    if(input$parameter=="None"){
      LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$length),]
      fit1=lm(log10(fldFishWholeWeight) ~ log10(length), data = LengthWeightAgeSp)
      p=plot_ly(LengthWeightAgeSp, x = ~length, y = ~fldFishWholeWeight, type = 'scatter', 
                text=~paste("Length:",length,"cm","<br>Weight:",fldFishWholeWeight),
                hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste("Length vs Weight"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                            max(LengthWeightAgeSp1()$length)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = FALSE) %>%
        add_trace(x=~length, y=10^(fitted(fit1)), type="scatter", 
                  mode='lines+markers',
                  line = list(color = '#1f77b4', shape="spline"),
                  marker = list(color = 'grey', opacity=0))
      p$elementId <- NULL
    }else if(input$parameter=="Sex"){
      LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$length),]
      fit2=lm(log10(fldFishWholeWeight) ~ log10(length) + obs.sex, data = LengthWeightAgeSp)
      p <- plot_ly(LengthWeightAgeSp, x = ~length, y = ~fldFishWholeWeight, type = 'scatter', 
                   text=~paste("Length:",length,"cm","<br>Weight:",fldFishWholeWeight, "<br>Sex:",obs.sex),
                   color = ~obs.sex, colors=c('unclassified'='#999999','male'='#6699ff','female'='#ff66cc'),
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
                   text=~paste("Length:",length,"cm","<br>Weight:",fldFishWholeWeight,
                               "<br>Gear type:",fldGearDescription),
                   hoverinfo = 'text',color= ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                   marker =list(opacity = 0.5)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by gear)"),
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
                     colors=c('VIa'='#8DD3C7','VIIb'='#FDB462','VIIc'='#BEBADA','VIIg'='#FB8072','VIIj'='#80B1D3','VIIk'='#FFFFB3'),
                     text=~paste("Length:",length,"cm","<br>Weight:",fldFishWholeWeight, "<br>Division:",ICESCODE),
                     hoverinfo = 'text',color= ~ICESCODE, marker =list(opacity = 0.5)) %>%  
          layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by divisions)"),
                 xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$length),
                                                              max(LengthWeightAgeSp1()$length)+1)),
                 yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
                 margin=(list(t=70)), showlegend = TRUE)
        p$elementId <- NULL
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p
  })
  
  ##################
  ### Length/Age ###
  ##################
  LengthWeightAgeSpA=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAge, age!="NA" & Year == 2017)
    }else{
      filter(LengthWeightAge, age!="NA" & Year == input$slideryear1)}
  })
  
  LengthWeightAgeSpA1=reactive({
    filter(LengthWeightAge, age!="NA")
  })
  output$coeff_table=renderTable({
    if(input$parameter=="None"){    
      alldata=data.frame(Data= "All data", Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]])
      yeardata=data.frame(Data= paste(input$slideryear1, "data"), 
                        Linf=coeff_year[which(coeff_year$Year==input$slideryear1), "Linf"],
                        K=coeff_year[which(coeff_year$Year==input$slideryear1), "K"], 
                        t0=coeff_year[which(coeff_year$Year==input$slideryear1), "t0"])
      rbind(alldata, yeardata)
    }else if(input$parameter=="Sex"){
      alldata=data.frame(Data= "All data", Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]])
      alldataF=data.frame(Data= paste("All Females"), 
                          Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Female"]),
                          K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Female"]), 
                          t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Female"]))
      alldataM=data.frame(Data= paste("All Males"), 
                          Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Male"]),
                          K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Male"]), 
                          t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Male"]))
      yeardataF=data.frame(Data= paste(input$slideryear1, " Females"), 
                           Linf=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnlinf" & coeff_sex_year$Year==input$slideryear1), "Female"]),
                           K=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnk"& coeff_sex_year$Year==input$slideryear1), "Female"]), 
                           t0=-exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnnt0"& coeff_sex_year$Year==input$slideryear1), "Female"]))
      yeardataM=data.frame(Data= paste(input$slideryear1, " Males"), 
                           Linf=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnlinf" & coeff_sex_year$Year==input$slideryear1), "Male"]),
                           K=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnk"& coeff_sex_year$Year==input$slideryear1), "Male"]), 
                           t0=-exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnnt0"& coeff_sex_year$Year==input$slideryear1), "Male"]))
      rbind(alldata, alldataF, alldataM, yeardataF, yeardataM)
    }else if(input$parameter=="Gear"){
      alldata=data.frame(Data= "All data", Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]])
      alldataA=data.frame(Data= paste("All GOV 3647 Groundgear A"), 
                          Linf=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "Linf"],
                          K=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "K"], 
                          t0=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "t0"])
      alldataD=data.frame(Data= paste("All GOV 3647 Groundgear D"), 
                          Linf=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "Linf"],
                          K=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "K"], 
                          t0=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "t0"])
      yeardataA=data.frame(Data= paste(input$slideryear1, " GOV 3647 Groundgear A"), 
                           Linf=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "Linf"],
                           K=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "K"], 
                           t0=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "t0"])
      yeardataD=data.frame(Data= paste(input$slideryear1, " GOV 3647 Groundgear D"), 
                           Linf=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "Linf"],
                           K=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "K"], 
                           t0=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "t0"])
      rbind(alldata, alldataA, alldataD, yeardataA, yeardataD)
    }else if(input$parameter=="Division"){
      yeardataVIa=data.frame(Data= paste(input$slideryear1, " VIa division"), 
                             Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "Linf"],
                             K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "K"],
                             t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "t0"])
      yeardataVIIb=data.frame(Data= paste(input$slideryear1, " VIIb division"), 
                              Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1), "Linf"],
                              K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1), "K"],
                              t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1), "t0"])
      yeardataVIIc=data.frame(Data= paste(input$slideryear1, " VIIc division"), 
                              Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1), "Linf"],
                              K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1), "K"],
                              t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1), "t0"])
      yeardataVIIg=data.frame(Data= paste(input$slideryear1, " VIIg division"), 
                              Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1), "Linf"],
                              K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1), "K"], 
                              t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1), "t0"])
      yeardataVIIj=data.frame(Data= paste(input$slideryear1, " VIIj division"), 
                              Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "Linf"],
                              K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "K"], 
                              t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "t0"])
      yeardataVIIk=data.frame(Data= paste(input$slideryear1, " VIIk division"), 
                              Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1 ), "Linf"],
                              K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1), "K"],
                              t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1), "t0"])
      rbind(yeardataVIa, yeardataVIIb, yeardataVIIc, yeardataVIIg, yeardataVIIj, yeardataVIIk)    
      }
    })
  
  output$laplot=renderPlotly({
    LWA_fish=LengthWeightAgeSpA()
    x=seq(0, max(LengthWeightAgeSpA1()$age), length.out = 199)
    predicted=data.frame(age=x, predlengthAll= vbTyp(x, Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]]),
                         predlengthYear= vbTyp(x, 
                                               Linf= coeff_year[which(coeff_year$Year==input$slideryear1), "Linf"],
                                               K=coeff_year[which(coeff_year$Year==input$slideryear1), "K"], 
                                               t0=coeff_year[which(coeff_year$Year==input$slideryear1), "t0"]),
                         predlengthF= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Female"]),
                                            K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Female"]), 
                                            t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Female"])),
                         predlengthM= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Male"]),
                                            K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Male"]), 
                                            t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Male"])),
                         predlengthFYear= vbTyp(x, Linf=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnlinf" & coeff_sex_year$Year==input$slideryear1), "Female"]),
                                                K=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnk"& coeff_sex_year$Year==input$slideryear1), "Female"]), 
                                                t0=-exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnnt0"& coeff_sex_year$Year==input$slideryear1), "Female"])),
                         predlengthMYear= vbTyp(x, Linf=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnlinf" & coeff_sex_year$Year==input$slideryear1), "Male"]),
                                                K=exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnk"& coeff_sex_year$Year==input$slideryear1), "Male"]), 
                                                t0=-exp(coeff_sex_year[which(coeff_sex_year$Parameter =="lnnt0"& coeff_sex_year$Year==input$slideryear1), "Male"])),
                         predlengthGearA= vbTyp(x, 
                                              Linf= coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "Linf"],
                                              K=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "K"],
                                              t0=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear A"), "t0"]),
                         predlengthGearAyear= vbTyp(x, 
                                                Linf=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "Linf"],
                                                K=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "K"],
                                                t0=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear A" & coeff_gear_year$Year==input$slideryear1), "t0"]),
                         predlengthGearD= vbTyp(x, 
                                                Linf= coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "Linf"],
                                                K=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "K"],
                                                t0=coeff_gear[which(coeff_gear$Gear =="GOV 3647 Groundgear D"), "t0"]),
                         predlengthGearDyear= vbTyp(x, 
                                                    Linf=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "Linf"],
                                                    K=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "K"],
                                                    t0=coeff_gear_year[which(coeff_gear_year$Gear =="GOV 3647 Groundgear D" & coeff_gear_year$Year==input$slideryear1), "t0"]),
                         predlengthVIa= vbTyp(x, 
                                              Linf= coeff_div[which(coeff_div$ICESCODE=="VIa"), "Linf"][[1]],
                                              K=coeff_div[which(coeff_div$ICESCODE=="VIa"), "K"][[1]],
                                              t0=coeff_div[which(coeff_div$ICESCODE=="VIa"), "t0"][[1]]),
                         predlengthVIaYear= vbTyp(x, 
                                                  Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "Linf"][[1]],
                                                  K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "K"][[1]],
                                                  t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIa" & coeff_div_year$Year==input$slideryear1), "t0"][[1]]),
                         predlengthVIIb= vbTyp(x, 
                                               Linf= coeff_div[which(coeff_div$ICESCODE=="VIIb"), "Linf"][[1]],
                                               K=coeff_div[which(coeff_div$ICESCODE=="VIIb" ), "K"][[1]],
                                               t0=coeff_div[which(coeff_div$ICESCODE=="VIIb"), "t0"][[1]]),
                         predlengthVIIbYear= vbTyp(x, 
                                                   Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1), "Linf"][[1]],
                                                   K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1), "K"][[1]], 
                                                   t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIb" & coeff_div_year$Year==input$slideryear1 ), "t0"][[1]]),
                         predlengthVIIc= vbTyp(x, 
                                               Linf= coeff_div[which(coeff_div$ICESCODE=="VIIc"), "Linf"][[1]],
                                               K=coeff_div[which(coeff_div$ICESCODE=="VIIc"), "K"][[1]], 
                                               t0=coeff_div[which(coeff_div$ICESCODE=="VIIc"), "t0"][[1]]),
                         predlengthVIIcYear= vbTyp(x, 
                                                   Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1), "Linf"][[1]],
                                                   K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1 ), "K"][[1]], 
                                                   t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIc" & coeff_div_year$Year==input$slideryear1 ), "t0"][[1]]),
                         predlengthVIIg= vbTyp(x, 
                                               Linf= coeff_div[which(coeff_div$ICESCODE=="VIIg"), "Linf"][[1]],
                                               K=coeff_div[which(coeff_div$ICESCODE=="VIIg"), "K"][[1]],
                                               t0=coeff_div[which(coeff_div$ICESCODE=="VIIg"), "t0"][[1]]),
                         predlengthVIIgYear= vbTyp(x, 
                                                   Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1 ), "Linf"][[1]],
                                                   K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1 ), "K"][[1]],
                                                   t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIg" & coeff_div_year$Year==input$slideryear1), "t0"][[1]]),
                         predlengthVIIj= vbTyp(x, 
                                               Linf= coeff_div[which(coeff_div$ICESCODE=="VIIj"), "Linf"][[1]],
                                               K=coeff_div[which(coeff_div$ICESCODE=="VIIj"), "K"][[1]], 
                                               t0=coeff_div[which(coeff_div$ICESCODE=="VIIj"), "t0"][[1]]),
                         predlengthVIIjYear= vbTyp(x, 
                                                   Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "Linf"][[1]],
                                                   K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "K"][[1]],
                                                   t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIj" & coeff_div_year$Year==input$slideryear1), "t0"][[1]]),
                         predlengthVIIk= vbTyp(x, 
                                               Linf= coeff_div[which(coeff_div$ICESCODE=="VIIk"), "Linf"][[1]],
                                               K=coeff_div[which(coeff_div$ICESCODE=="VIIk"), "K"][[1]], 
                                               t0=coeff_div[which(coeff_div$ICESCODE=="VIIk"), "t0"][[1]]),
                         predlengthVIIkYear= vbTyp(x, 
                                                   Linf= coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1 ), "Linf"][[1]],
                                                   K=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1), "K"][[1]],
                                                   t0=coeff_div_year[which(coeff_div_year$ICESCODE=="VIIk" & coeff_div_year$Year==input$slideryear1), "t0"][[1]])
    )
    
    
    if(dim(LWA_fish)[1]==0){
      p=NULL
    }else if(input$parameter=="None"){
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  text=~paste("Length:",length,"cm","<br>Age:",age),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = FALSE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      if(all(!is.na(predicted$predlengthAll))){
        p = p %>%
          add_trace(data=predicted, x = ~age, y = ~predlengthAll, type="scatter", mode = "lines",
                  line = list(shape="spline", color="grey"), name="All data fit", hoverinfo="none")
      }
      if(all(!is.na(predicted$predlengthYear))){
        p = p %>%
          add_trace(data=predicted, x = ~age, y = ~predlengthYear, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#1f77b4"), name=paste(input$slideryear1, "data fit"), hoverinfo="none")
      }
      p$elementId <- NULL
    }else if(input$parameter=="Sex"){
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~obs.sex, colors=c('unclassified'='#999999','male'='#6699ff','female'='#ff66cc'),
                  text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Sex:", obs.sex),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by sex)"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      if(all(!is.na(predicted$predlengthFYear))){
        p = p %>%
        add_trace(data=predicted, x = ~age, y = ~predlengthFYear, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#ff66cc"), name=paste(input$slideryear1,"Female data fit"), hoverinfo="none")
      }
      if(all(!is.na(predicted$predlengthMYear))){
        p = p %>%
          add_trace(data=predicted, x = ~age, y = ~predlengthMYear, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#6699ff"), name=paste(input$slideryear1,"Male data fit"), hoverinfo="none")
      }
      p$elementId <- NULL
    }else if(input$parameter=="Gear"){
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                  text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Gear type:",fldGearDescription),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>%
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by gear)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                            max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                            zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE) 
      if(all(!is.na(predicted$predlengthGearAyear))){
        p = p %>%
          add_trace(data=predicted, x = ~age, y = ~predlengthGearAyear, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#132B43"), name="GOV 3647 Groundgear A", hoverinfo="none")
        }
      if(all(!is.na(predicted$predlengthGearDyear))){
          p = p %>%
            add_trace(data=predicted, x = ~age, y = ~predlengthGearDyear, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#56B1F7"), name="GOV 3647 Groundgear D", hoverinfo="none")  
        }
      p$elementId <- NULL
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        grspnew.w2 = filter(LengthWeightAgeSpA(), ICESCODE %in% c(input$division1))
        if(dim(grspnew.w2)[1]==0){
          p=NULL 
        }else{
          
          p=plot_ly() %>% 
          add_trace(data=grspnew.w2, x = ~age, y = ~length, type="scatter", mode="markers",color= ~ICESCODE,
                    colors=c('VIa'='#8DD3C7','VIIb'='#FDB462','VIIc'='#BEBADA','VIIg'='#FB8072','VIIj'='#80B1D3','VIIk'='#FFFFB3'),
                    text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Division:",ICESCODE),
                    hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
         layout(hovermode="closest", title=paste("Age vs Length (points coloured by divisions)"),
                 yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                              max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
                 xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                              zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
          
          if(all(!is.na(predicted$predlengthVIaYear)) & "VIa" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIaYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#8DD3C7'), name=paste(input$slideryear1,"VIa data fit"), hoverinfo="none")}  
          if(all(!is.na(predicted$predlengthVIIbYear))& "VIIb" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIIbYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#FDB462'), name=paste(input$slideryear1,"VIIb data fit"), hoverinfo="none")}  
          if(all(!is.na(predicted$predlengthVIIcYear))& "VIIc" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIIcYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#BEBADA'), name=paste(input$slideryear1,"VIIc data fit"), hoverinfo="none")}  
          if(all(!is.na(predicted$predlengthVIIgYear))& "VIIg" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIIgYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#FB8072'), name=paste(input$slideryear1,"VIIg data fit"), hoverinfo="none")}  
          if(all(!is.na(predicted$predlengthVIIjYear))& "VIIj" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIIjYear, type="scatter", mode = "lines",
                    line = list(shape="spline", color='#80B1D3'), name=paste(input$slideryear1,"VIIj data fit"), hoverinfo="none")}  
          if(all(!is.na(predicted$predlengthVIIkYear))& "VIIk" %in% input$division1 ){
            p=p %>% add_trace(data=predicted, x = ~age, y = ~predlengthVIIkYear, type="scatter", mode = "lines",
                            line = list(shape="spline", color='#FFFFB3'), name=paste(input$slideryear1,"VIIk data fit"), hoverinfo="none")}  
         
          p$elementId <- NULL
        }
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p
  })
  
  
  output$predlength=reactive({
    round(vbTyp(input$age, 
          Linf= coeff_year[which(coeff_year$Year==input$slideryear1), "Linf"],
          K=coeff_year[which(coeff_year$Year==input$slideryear1), "K"], 
          t0=coeff_year[which(coeff_year$Year==input$slideryear1), "t0"]),2)
    })
  
  output$latab=renderUI({
    if(dim(LengthWeightAgeSpA1())[1]==0){
      h3(paste("No Age data available for", species, sep=" "))
    }else if(dim(LengthWeightAgeSpA())[1]==0){
      h3(paste("No Age data available for", species, "for", input$slideryear1, ".Try another year", sep= " "))
    }else{
      list(
        plotlyOutput("laplot"),
        h2("Life Parameters"),
        fluidRow(column(6,tableOutput("coeff_table"))#,
                 #column(6,numericInput("age", "Fish Age:", value=0, min = 0, max = 10),
                #        HTML("<b>Predicted Length:</b>"),
                #        textOutput("predlength"))
                 ),
        "Data fits are modelled using Von Bertalanffy Growth Models",HTML("<br>"),
        "If data fits are missing this is due to either no data available or small sample sizes",HTML("<br>"),
        "*Filtering also available through the legend on the RHS when a parameter is chosen")
    }
  })
  
  
  
  
  
  ##############
  ### Cohort ###
  ##############
  output$coeff_table_cohort=renderTable({
    if(input$parameter=="None"){ 
      coeff_cohort$Cohort=as.character(coeff_cohort$Cohort)
      coeff_cohort_year=filter(coeff_cohort, Cohort==input$slideryear1)    
      coeff_cohort_year
    }else if(input$parameter=="Gear"){
      coeff_gear_cohort$Cohort=as.character(coeff_gear_cohort$Cohort)
      coeff_gear_cohort_year=filter(coeff_gear_cohort, Cohort==input$slideryear1)    
      coeff_gear_cohort_year
    }else if(input$parameter=="Sex"){
      #coeff_sex_cohort$Cohort=as.character(coeff_sex_cohort$Cohort)
      yeardataF=data.frame(Data= paste(input$slideryear1, " Females"), 
                           Linf=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnlinf" & coeff_sex_cohort$Cohort==input$slideryear1), "Female"]),
                           K=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnk"& coeff_sex_cohort$Cohort==input$slideryear1), "Female"]), 
                           t0=-exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnnt0"& coeff_sex_cohort$Cohort==input$slideryear1), "Female"]))
      yeardataM=data.frame(Data= paste(input$slideryear1, " Males"), 
                           Linf=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnlinf" & coeff_sex_cohort$Cohort==input$slideryear1), "Male"]),
                           K=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnk"& coeff_sex_cohort$Cohort==input$slideryear1), "Male"]), 
                           t0=-exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnnt0"& coeff_sex_cohort$Cohort==input$slideryear1), "Male"]))
      rbind(yeardataF, yeardataM)
      #coeff_sex_cohort_year
    }else if(input$parameter=="Division"){
      coeff_div_cohort$Cohort=as.character(coeff_div_cohort$Cohort)
      coeff_div_cohort_year=filter(coeff_div_cohort, Cohort==input$slideryear1)    
      coeff_div_cohort_year
    }
    
  })
  
  LWAcohort=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAge, age!="NA" & Cohort == 2017)
    }else{
      filter(LengthWeightAge, age!="NA" & Cohort == input$slideryear1)}
  })
  
  output$cohortplot=renderPlotly({
    LWA_fish=LWAcohort()
    
    x=seq(0, max(LengthWeightAgeSpA1()$age), length.out = 199)
    if(dim(LWA_fish)[1]==0){
      p=NULL
    }else if(input$parameter=="None"){
      predictedcohort=data.frame(age=x, predlengthAll= vbTyp(x, Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]]),
                                 predlengthCohort= vbTyp(x, 
                                                         Linf= coeff_cohort[which(coeff_cohort$Cohort==input$slideryear1), "Linf"],
                                                         K=coeff_cohort[which(coeff_cohort$Cohort==input$slideryear1), "K"], 
                                                         t0=coeff_cohort[which(coeff_cohort$Cohort==input$slideryear1), "t0"]))
      
      predictedcohort=filter(predictedcohort, !is.na(predlengthCohort))
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  text=~paste("Length:",length,"cm","<br>Age:",age),
                  hoverinfo = 'text',
                  marker =list(opacity = 0.5), showlegend = FALSE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      if(dim(predictedcohort)[1]>0){
      p=p %>%
        add_trace(data=predictedcohort, x = ~age, y = ~predlengthCohort, type="scatter", mode = "lines",
                  line = list(shape="spline", color="#1f77b4"), name=paste0(input$slideryear1, " data fit"), hoverinfo="none")
      }
      p$elementId <- NULL
    }else if(input$parameter=="Sex"){
      predictedcohort=data.frame(age=x, predlengthAll= vbTyp(x, Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]]),
                                 predlengthF= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Female"]),
                                                    K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Female"]), 
                                                    t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Female"])),
                                 predlengthM= vbTyp(x, Linf=exp(coeff_sex[which(coeff_sex$Parameter =="lnlinf"), "Male"]),
                                                    K=exp(coeff_sex[which(coeff_sex$Parameter =="lnk"), "Male"]), 
                                                    t0=-exp(coeff_sex[which(coeff_sex$Parameter =="lnnt0"), "Male"])),
                                 predlengthFYear= vbTyp(x, Linf=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnlinf" & coeff_sex_cohort$Cohort==input$slideryear1), "Female"]),
                                                        K=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnk"& coeff_sex_cohort$Cohort==input$slideryear1), "Female"]), 
                                                        t0=-exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnnt0"& coeff_sex_cohort$Cohort==input$slideryear1), "Female"])),
                                 predlengthMYear= vbTyp(x, Linf=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnlinf" & coeff_sex_cohort$Cohort==input$slideryear1), "Male"]),
                                                        K=exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnk"& coeff_sex_cohort$Cohort==input$slideryear1), "Male"]), 
                                                        t0=-exp(coeff_sex_cohort[which(coeff_sex_cohort$Parameter =="lnnt0"& coeff_sex_cohort$Cohort==input$slideryear1), "Male"])))

      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~obs.sex, colors=c('unclassified'='#999999','male'='#6699ff','female'='#ff66cc'),
                  text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Sex:", obs.sex),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by sex)"),
               xaxis = list(title = 'Age (years)', zeroline=FALSE,
                            range= c(min(LengthWeightAgeSpA1()$age)-.1,max(LengthWeightAgeSpA1()$age)+1)),
               yaxis = list(title = 'Length (cm)', zeroline=FALSE,
                            range = c(0, max(LengthWeightAgeSpA1()$length, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      if(all(!is.na(predictedcohort$predlengthFYear))){
        p = p %>%
          add_trace(data=predictedcohort, x = ~age, y = ~predlengthFYear, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#ff66cc"), name="Females", hoverinfo="none")
      }
      if(all(!is.na(predictedcohort$predlengthMYear))){
        p = p %>%
          add_trace(data=predictedcohort, x = ~age, y = ~predlengthMYear, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#6699ff"), name="Males", hoverinfo="none")
      }
      
      
      p$elementId <- NULL
    }else if(input$parameter=="Gear"){
      predictedcohort=data.frame(age=x, predlengthAll= vbTyp(x, Linf=coeff_all[[1]], K=coeff_all[[2]], t0=coeff_all[[3]]),
                                 predlengthCohortA= vbTyp(x, 
                                                         Linf= coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear A"), "Linf"],
                                                         K=coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear A"), "K"], 
                                                         t0=coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear A"), "t0"]),
                                 predlengthCohortD= vbTyp(x, 
                                                          Linf= coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear D"), "Linf"],
                                                          K=coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear D"), "K"], 
                                                          t0=coeff_gear_cohort[which(coeff_gear_cohort$Cohort==input$slideryear1 & coeff_gear_cohort$Gear=="GOV 3647 Groundgear D"), "t0"]))
      
      p=plot_ly() %>% 
        add_trace(data=LWA_fish, x = ~age, y = ~length, type="scatter", mode="markers",
                  color = ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                  text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Gear type:",fldGearDescription),
                  hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
        layout(hovermode="closest", title=paste("Length vs Age (points coloured by gear)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                            max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                            zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE) 
      if(all(!is.na(predictedcohort$predlengthCohortA))){
        p = p %>%
          add_trace(data=predictedcohort, x = ~age, y = ~predlengthCohortA, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#132B43"), name="GOV 3647 Groundgear A", hoverinfo="none")
      }
      if(all(!is.na(predictedcohort$predlengthCohortD))){
        p = p %>%
          add_trace(data=predictedcohort, x = ~age, y = ~predlengthCohortD, type="scatter", mode = "lines",
                    line = list(shape="spline", color="#56B1F7"), name="GOV 3647 Groundgear D", hoverinfo="none")
      }
      p$elementId <- NULL
    }else if(input$parameter=="Division"){
      predictedcohort=data.frame(age=x, 
                                 predlengthVIaYear= vbTyp(x, 
                                                          Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIa" & coeff_div_cohort$Cohort==input$slideryear1), "Linf"],
                                                          K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIa" & coeff_div_cohort$Cohort==input$slideryear1), "K"],
                                                          t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIa" & coeff_div_cohort$Cohort==input$slideryear1), "t0"]),
                                 predlengthVIIbYear= vbTyp(x, 
                                                           Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIb" & coeff_div_cohort$Cohort==input$slideryear1), "Linf"],
                                                           K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIb" & coeff_div_cohort$Cohort==input$slideryear1), "K"], 
                                                           t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIb" & coeff_div_cohort$Cohort==input$slideryear1 ), "t0"]),
                                 predlengthVIIcYear= vbTyp(x, 
                                                           Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIc" & coeff_div_cohort$Cohort==input$slideryear1), "Linf"],
                                                           K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIc" & coeff_div_cohort$Cohort==input$slideryear1 ), "K"], 
                                                           t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIc" & coeff_div_cohort$Cohort==input$slideryear1 ), "t0"]),
                                 predlengthVIIgYear= vbTyp(x, 
                                                           Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIg" & coeff_div_cohort$Cohort==input$slideryear1 ), "Linf"],
                                                           K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIg" & coeff_div_cohort$Cohort==input$slideryear1 ), "K"],
                                                           t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIg" & coeff_div_cohort$Cohort==input$slideryear1), "t0"]),
                                 predlengthVIIjYear= vbTyp(x, 
                                                           Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIj" & coeff_div_cohort$Cohort==input$slideryear1), "Linf"],
                                                           K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIj" & coeff_div_cohort$Cohort==input$slideryear1), "K"],
                                                           t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIj" & coeff_div_cohort$Cohort==input$slideryear1), "t0"]),
                                 predlengthVIIkYear= vbTyp(x, 
                                                           Linf= coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIk" & coeff_div_cohort$Cohort==input$slideryear1 ), "Linf"],
                                                           K=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIk" & coeff_div_cohort$Cohort==input$slideryear1), "K"],
                                                           t0=coeff_div_cohort[which(coeff_div_cohort$ICESCODE=="VIIk" & coeff_div_cohort$Cohort==input$slideryear1), "t0"]))

      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        grspnew.w2 = filter(LWA_fish, ICESCODE %in% c(input$division1))
        if(dim(grspnew.w2)[1]==0){
          p=NULL 
        }else{
          
          p=plot_ly() %>% 
            add_trace(data=grspnew.w2, x = ~age, y = ~length, type="scatter", mode="markers",color= ~ICESCODE,
                      colors=c('VIa'='#8DD3C7','VIIb'='#FDB462','VIIc'='#BEBADA','VIIg'='#FB8072','VIIj'='#80B1D3','VIIk'='#FFFFB3'),
                      text=~paste("Length:",length,"cm","<br>Age:",age, "<br>Division:",ICESCODE),
                      hoverinfo = 'text',marker =list(opacity = 0.5), showlegend = TRUE) %>% 
            layout(hovermode="closest", title=paste("Age vs Length (points coloured by divisions)"),
                   yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$length), 
                                                                max(LengthWeightAgeSpA1()$length)+1), zeroline = FALSE),
                   xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$age, na.rm = T)*1.05), 
                                zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)
          
          if(all(!is.na(predictedcohort$predlengthVIaYear)) & "VIa" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIaYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#8DD3C7'), name=paste(input$slideryear1,"VIa data fit"), hoverinfo="none")}  
          if(all(!is.na(predictedcohort$predlengthVIIbYear))& "VIIb" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIIbYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#FDB462'), name=paste(input$slideryear1,"VIIb data fit"), hoverinfo="none")}  
          if(all(!is.na(predictedcohort$predlengthVIIcYear))& "VIIc" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIIcYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#BEBADA'), name=paste(input$slideryear1,"VIIc data fit"), hoverinfo="none")}  
          if(all(!is.na(predictedcohort$predlengthVIIgYear))& "VIIg" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIIgYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#FB8072'), name=paste(input$slideryear1,"VIIg data fit"), hoverinfo="none")}  
          if(all(!is.na(predictedcohort$predlengthVIIjYear))& "VIIj" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIIjYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#80B1D3'), name=paste(input$slideryear1,"VIIj data fit"), hoverinfo="none")}  
          if(all(!is.na(predictedcohort$predlengthVIIkYear))& "VIIk" %in% input$division1 ){
            p=p %>% add_trace(data=predictedcohort, x = ~age, y = ~predlengthVIIkYear, type="scatter", mode = "lines",
                              line = list(shape="spline", color='#FFFFB3'), name=paste(input$slideryear1,"VIIk data fit"), hoverinfo="none")}  
          
          
          p$elementId <- NULL
        }
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p
  }) 
  
  output$cohorttab=renderUI({
    if(dim(LengthWeightAgeSpA1())[1]==0){
      h3(paste("No Age data available for", species, sep=" "))
    }else if(dim(LWAcohort())[1]==0){
      h3(paste("No Age data available for", species, "for", input$slideryear1, ".Try another year", sep= " "))
    }else{
      list(
        plotlyOutput("cohortplot"),
        h2("Life Parameters"),
        tableOutput("coeff_table_cohort"),
        "Data fits are modelled using Von Bertalanffy Growth Models",HTML("<br>"),
        "If data fits are missing this is due to either no data available or small sample sizes",HTML("<br>"),
        "*Filtering also available through the legend on the RHS when a parameter is chosen")
    }
  })
  
  })
