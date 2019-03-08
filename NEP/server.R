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
species="Nephrops"
speciesFAO="NEP"
### Get Data - pull out valid stns
# Used for Catch rate and CPUE
dat1=read.csv("dat.csv")
#dat1 <- read.csv("H:/test/Exploing IGFS/IGFS test/Nephrops/dat.csv")
dat<-dat1
dat$Functional_Unit<-as.factor(dat$Functional_Unit)
dat<-filter(dat,Functional_Unit==16|Functional_Unit==17|Functional_Unit==19|Functional_Unit==22|Functional_Unit==99|Functional_Unit==2021)
dat$Functional_Unit<-droplevels(dat$Functional_Unit)
levels(dat$Functional_Unit)<-c("16" ,  "17" ,  "19" ,  "22" ,  "Outside FU" ,  "20-21")

dat$Kg_Hr= dat$PredWt_Kg/dat$TowDurationMin*60
dat$No_Km2= dat$NepCount/dat$AreaKmSq
dat$No_30min = dat$NepCount/dat$TowDurationMin*30
dat$AgeClassification<-dat$CLmm

dat$AgeClassification[which(dat$CLmm>17)]<-"Adult"
dat$AgeClassification[which(dat$CLmm<=17)]<-"Juvenile"
dat$AgeClassification<-as.factor(dat$AgeClassification)
dat$Functional_Unit<-as.factor(dat$Functional_Unit)

dat_raised= aggregate(dat[,c("NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")], 
                      by=list(dat$Year, dat$Survey_Code, dat$Haul, dat$Functional_Unit, dat$Fishing_Grounds,
                              dat$fldShotLatDecimalDegrees,dat$fldShotLonDecimalDegrees, 
                             dat$fldHaulLatDecimalDegrees, dat$fldHaulLonDecimalDegrees),
                      FUN=sum,  na.rm=TRUE)
names(dat_raised) = c("Year", "Survey_Code", "Haul", "Functional_Unit", "Fishing_Grounds",
                      "Lat", "Lon", "LatV2", "LonV2", "NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")

dat_raised$symbSize <- sqrt( dat_raised$Kg_Hr/ pi )
dat_raised$symbSize2 <- sqrt( dat_raised$No_Km2/ pi )
dat_raised$symbSize3 <- sqrt( dat_raised$No_30min/ pi )
maxyear=max(dat$Year, na.rm=TRUE)

### Get Data - pull out valid stns (grep & grepl are pattern matching functions, the latter ignores case)
stn=readRDS("stn.RDS")

# Read in shapefiles
div <- geojsonio::geojson_read("div_simple.geojson", what = "sp")
cont <- geojsonio::geojson_read("cont1_simple.geojson", what = "sp")
FU <- rgdal::readOGR("Nephrops_Functional_Unit_Cut.shp")

#FU<-geojsonio::geojson_read("FU_simple.geojson", what = "sp")
#FU1 <- rgdal::readOGR("WGNEPS_FUs.shp")
#FU1<-FU1[-18,]#removing FU18

centers <-readRDS("centers.RDS")


#Juveniles
Juveniles= filter(dat, AgeClassification=="Juvenile")
if(dim(Juveniles)[1]>0){
  JuvNumbersMap=aggregate(Juveniles[,c("NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")],
                          by=list(Juveniles$Year, Juveniles$Survey_Code, Juveniles$Haul, Juveniles$Functional_Unit, Juveniles$Fishing_Grounds,
                                  Juveniles$fldShotLatDecimalDegrees,Juveniles$fldShotLonDecimalDegrees, 
                                  Juveniles$fldHaulLatDecimalDegrees, Juveniles$fldHaulLonDecimalDegrees),
                          FUN=sum,  na.rm=TRUE)
  names(JuvNumbersMap) = c("Year", "Survey_Code", "Haul", "Functional_Unit", "Fishing_Grounds",
                           "Lat", "Lon", "LatV2", "LonV2", "NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")
  JuvNumbersMap$symbSize <- sqrt( JuvNumbersMap$No_30min/ pi )
}else{
  JuvNumbersMap=data.frame("Year"=2017, "Survey_Code"=NA, "Haul"=NA, "Functional_Unit"=NA, "Fishing_Grounds"=NA,
                           "Lat"=55.109, "Lon"=-9.558, "LatV2"=NA, "LonV2"=NA, "NepCount"=NA, "PredWt_Kg"=NA, "Kg_Hr"=NA, "No_Km2"=NA, "No_30min"=NA,"symbSize"=NA)}



#Adults
Adults= filter(dat, AgeClassification=="Adult")
if(dim(Adults)[1]>0){
  AdultNumbersMap=aggregate(Adults[,c("NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")],
                            by=list(Adults$Year, Adults$Survey_Code, Adults$Haul, Adults$Functional_Unit, Adults$Fishing_Grounds,
                                    Adults$fldShotLatDecimalDegrees,Adults$fldShotLonDecimalDegrees, 
                                    Adults$fldHaulLatDecimalDegrees, Adults$fldHaulLonDecimalDegrees),
                            FUN=sum,  na.rm=TRUE)
  names(AdultNumbersMap) = c("Year", "Survey_Code", "Haul", "Functional_Unit", "Fishing_Grounds",
                             "Lat", "Lon", "LatV2", "LonV2", "NepCount", "PredWt_Kg", "Kg_Hr", "No_Km2", "No_30min")
  AdultNumbersMap$symbSize <- sqrt( AdultNumbersMap$No_30min/ pi )
}else{
  AdultNumbersMap=data.frame("Year"=2017, "Survey_Code"=NA, "Haul"=NA, "Functional_Unit"=NA, "Fishing_Grounds"=NA,
                             "Lat"=55.109, "Lon"=-9.558, "LatV2"=NA, "LonV2"=NA, "NepCount"=NA, "PredWt_Kg"=NA, "Kg_Hr"=NA, "No_Km2"=NA, "No_30min"=NA,"symbSize"=NA)}





########################Data reshaping for Length/Weight Plot#########
lw<-dat[c(3,6,7,8,9,11,12,13,14,19)]
lw$NepCount<-round(lw$NepCount,0)
lw$Weight_g<-(lw$PredWt_Kg/lw$NepCount)*1000


indLW<-lw%>%uncount(NepCount)
indLW$FUnit<-as.factor(indLW$Functional_Unit)


###define colour palette###



###########################################################################################################
############################################################################################################
#########################################################################################################
# Define server logic required
shinyServer(function(input, output) {

 
  
  ##########################
  ### Reactive filtering ###
  ##########################
  #For Length/Weight and Length/Age plots
  output$yearfiltermap=renderUI({
      sliderInput("slideryear", "Choose Year:", min = 2003, max = maxyear, value = maxyear, step = 1, 
                  sep = "", animate = TRUE)
  })  
  output$yearfilter=renderUI({
    if(input$tabselected=="lw"  | input$tabselected=="la" | input$tabselected=="co"){
      sliderInput("slideryear1", "Choose Year:", min = 2003, max = maxyear, value = maxyear, step = 1, 
                  sep = "", animate = TRUE)
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

  output$downloadData_map <- downloadHandler(
    filename = function() {
      paste(speciesFAO, "_Map_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat, file, row.names = FALSE)
    },
    contentType = "application/csv" )
  
  
  output$downloadData_LWA <- downloadHandler(
    filename = function() {
      #lwa.data.full<- readRDS("LengthWeightAge.RDS")
      paste(speciesFAO, "_Length_Weight_Age_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(indLW, file, row.names = FALSE)
    },
    contentType = "application/csv"
  )
  
  output$LWAdownload=renderUI({
    if(input$tabselected=="lw"){
      downloadButton("downloadData_LWA", "Download Length Weight data")
    }
  })
  
  ######################
  ### Filtering data ###
  ######################
  juv_length_split=17
  
  cat=reactive({
    dplyr::filter(dat_raised, Survey_Code %in% paste0('IGFS', input$slideryear))
  })
  
  haul = reactive({
    dplyr::filter(stn, fldCruiseName==paste0('IGFS',  input$slideryear))
  })
  
  JuvNumbers=reactive({
    dplyr::filter(JuvNumbersMap, Year %in% input$slideryear & No_30min>0)
  })
  
  AdultNumbers=reactive({
    dplyr::filter(AdultNumbersMap, Year %in% input$slideryear & No_30min>0)
  })
  
  ###############
  ### Mapping ###
  ###############
  html_legend <- "<img src='x-mark-16.png'>Stations Surveyed"
  
  output$mymap <- renderLeaflet({
    mymap <-leaflet() %>%
      setView(lng = -9, lat = 53, zoom = 5.5) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      #addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
      #addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)%>%
      addPolylines(color = "grey",data= FU, group = "Functional Units", weight = 2)%>%
      addLabelOnlyMarkers(data = centers,
                          lng = ~x, lat = ~y, label = ~paste("", region),
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
      #addPolygons(data=FU, color = "#444444", weight = 1, smoothFactor = 0.5,
      #            opacity = 1.0, fillOpacity = 0.5,
      #            #fillColor = ~colorQuantile("YlOrRd", FU)(FU),
      #            highlightOptions = highlightOptions(color = "white", weight = 2,
      #                                                bringToFront = FALSE))%>%
      addLegend("bottomright",colors = c("blue","purple", "black","yellow", "green"), 
                labels = c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>",  "Total No of Nephrops per 30 min Haul", "No of Juvenile Nephrops per 30 min Haul", "No of Adult Nephrops per 30 min Haul"))%>%
      hideGroup("Stations Surveyed")
    
    return(mymap)
  })
  
  icon.ship <- makeIcon(iconUrl  = 'x-mark-16.png', iconHeight = 7, iconWidth = 7)
  
  observe({
    new_zoom <- input$mymap_zoom
    leafletProxy('mymap') %>%
      clearGroup(group =  c("Catch Rate kg/hr", "Distribution No/km<sup>2</sup>",  "Total No of Nephrops per 30 min Haul", "No of Juvenile Nephrops per 30 min Haul", "No of Adult Nephrops per 30 min Haul"))%>%
      addMarkers(lng=haul()$fldShotLonDecimalDegrees, lat=haul()$fldShotLatDecimalDegrees, icon =icon.ship, 
                 group="Stations Surveyed", 
                 popup=paste("<b>Station:</b> ",haul()$fldPrimeStation, "<br />", "<b>Gear Type:</b> ",haul()$Gear_Type,"<br />", 
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
  })
  ##########
  ## CPUE ##

  
  output$cpueplotall=renderPlotly({
    if(input$parameter=="16"){
      FU16<-filter(dat,Functional_Unit=="16")
      catchAll <- aggregate(list(KgHr=FU16$Kg_Hr), list(Cruise=FU16$Survey_Code, Year= FU16$Year),mean, na.rm=TRUE)
      p=ggplot(FU16, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
        theme_bw()  + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
      ggplotly(p)
    }  else if(input$parameter=="17")
      {FU17<-filter(dat,Functional_Unit=="17")
    catchAll <- aggregate(list(KgHr=FU17$Kg_Hr), list(Cruise=FU17$Survey_Code, Year= FU17$Year),mean, na.rm=TRUE)
    p=ggplot(FU17, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="19")
    {FU19<-filter(dat,Functional_Unit=="19")
    catchAll <- aggregate(list(KgHr=FU19$Kg_Hr), list(Cruise=FU19$Survey_Code, Year= FU19$Year),mean, na.rm=TRUE)
    p=ggplot(FU19, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="20-21")
    {FU20<-filter(dat,Functional_Unit=="20-21")
    catchAll <- aggregate(list(KgHr=FU20$Kg_Hr), list(Cruise=FU20$Survey_Code, Year= FU20$Year),mean, na.rm=TRUE)
    p=ggplot(FU20, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="22")
    {FU22<-filter(dat,Functional_Unit=="22")
    catchAll <- aggregate(list(KgHr=FU22$Kg_Hr), list(Cruise=FU22$Survey_Code, Year= FU22$Year),mean, na.rm=TRUE)
    p=ggplot(FU22, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="Outside FU")
    {FUout<-filter(dat,Functional_Unit=="Outside FU")
    catchAll <- aggregate(list(KgHr=FUout$Kg_Hr), list(Cruise=FUout$Survey_Code, Year= FUout$Year),mean, na.rm=TRUE)
    p=ggplot(FUout, aes(x=Year, y=Kg_Hr)) + geom_jitter(width = 0.05, colour="grey") + 
      geom_line(data=catchAll, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Functional_Unit)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    
  })
  
  output$cpueplotparam=renderPlotly({
    if(input$parameter=="16"){
      FU16<-filter(dat,Functional_Unit=="16")
      catchsex <- aggregate(list(KgHr=FU16$Kg_Hr), list(Cruise=FU16$Survey_Code, Year= FU16$Year,Sex=FU16$Sex),mean, na.rm=TRUE)
      p=ggplot(FU16, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
        geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Sex)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year)+ theme(axis.title.x=element_blank())
      ggplotly(p)
    }  else if(input$parameter=="17")
    {FU17<-filter(dat,Functional_Unit=="17")
    catchsex <- aggregate(list(KgHr=FU17$Kg_Hr), list(Cruise=FU17$Survey_Code, Year= FU17$Year,Sex=FU17$Sex),mean, na.rm=TRUE)
    p=ggplot(FU17, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="19")
    {FU19<-filter(dat,Functional_Unit=="19")
    catchsex <- aggregate(list(KgHr=FU19$Kg_Hr), list(Cruise=FU19$Survey_Code, Year= FU19$Year,Sex=FU19$Sex),mean, na.rm=TRUE)
    p=ggplot(FU19, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +facet_wrap(~Sex)+  
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="20-21")
    {FU20<-filter(dat,Functional_Unit=="20-21")
    catchsex <- aggregate(list(KgHr=FU20$Kg_Hr), list(Cruise=FU20$Survey_Code, Year= FU20$Year,Sex=FU20$Sex),mean, na.rm=TRUE)
    p=ggplot(FU20, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") +facet_wrap(~Sex)+ 
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="22")
    {FU22<-filter(dat,Functional_Unit=="22")
    catchsex <- aggregate(list(KgHr=FU22$Kg_Hr), list(Cruise=FU22$Survey_Code, Year= FU22$Year,Sex=FU22$Sex),mean, na.rm=TRUE)
    p=ggplot(FU22, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="Outside FU")
    {FUout<-filter(dat,Functional_Unit=="Outside FU")
    catchsex <- aggregate(list(KgHr=FUout$Kg_Hr), list(Cruise=FUout$Survey_Code, Year= FUout$Year, Sex=FUout$Sex),mean, na.rm=TRUE)
    p=ggplot(FUout, aes(x=Year, y=Kg_Hr,colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=catchsex, aes(x=Year, y =KgHr), size=1)+ ylab("KG/Hour") + facet_wrap(~Sex) +
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    
  })
  
 
      
    
  
  #################  
  ### Abundance ###
  #################
  output$abundanceplotall=renderPlotly({
    if(input$parameter=="16"){
        FU16<-filter(dat,Functional_Unit=="16")
        meanAll <- aggregate(FU16[,c("No_Km2")],by=list(FU16$Year),FUN=mean,  na.rm=TRUE)
        names(meanAll)=c("Year", "No_Km2")
        p=ggplot(FU16, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
          geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
          theme_bw()  + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
        ggplotly(p)
      }  else if(input$parameter=="17")
      {FU17<-filter(dat,Functional_Unit=="17")
      meanAll <-  aggregate(FU17[,c("No_Km2")],by=list(FU17$Year),FUN=mean,  na.rm=TRUE)
      names(meanAll)=c("Year", "No_Km2")
      p=ggplot(FU17, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
      ggplotly(p)}
      else if(input$parameter=="19")
      {FU19<-filter(dat,Functional_Unit=="19")
      meanAll <-  aggregate(FU19[,c("No_Km2")],by=list(FU19$Year),FUN=mean,  na.rm=TRUE)
      names(meanAll)=c("Year", "No_Km2")
      p=ggplot(FU19, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
     
      ggplotly(p)}
      else if(input$parameter=="20-21")
      {FU20<-filter(dat,Functional_Unit=="20-21")
      meanAll <-  aggregate(FU20[,c("No_Km2")],by=list(FU20$Year),FUN=mean,  na.rm=TRUE)
      names(meanAll)=c("Year", "No_Km2")
      p=ggplot(FU20, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
     
      ggplotly(p)}
      else if(input$parameter=="22")
      {FU22<-filter(dat,Functional_Unit=="22")
      meanAll <-  aggregate(FU22[,c("No_Km2")],by=list(FU22$Year),FUN=mean,  na.rm=TRUE)
      names(meanAll)=c("Year", "No_Km2")
      p=ggplot(FU22, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
      ggplotly(p)}
      else if(input$parameter=="Outside FU")
      {FUout<-filter(dat,Functional_Unit=="Outside FU")
      meanAll <-  aggregate(FUout[,c("No_Km2")],by=list(FUout$Year),FUN=mean,  na.rm=TRUE)
      names(meanAll)=c("Year", "No_Km2")
      p=ggplot(FUout, aes(x=Year, y=No_Km2)) + geom_jitter(width = 0.05, colour="grey") + 
        geom_line(data=meanAll, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Functional_Unit)+
        theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
      ggplotly(p)}
      
    })
    
    
  output$abundanceplotparam=renderPlotly({
    
    if(input$parameter=="16"){
      FU16<-filter(dat,Functional_Unit=="16")
      meansex <- aggregate(FU16[,c("No_Km2")],by=list(FU16$Year,FU16$Sex),FUN=mean,  na.rm=TRUE)
      names(meansex)=c("Year", "Sex", "No_Km2")
      p=ggplot(FU16, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
        geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
        theme_bw()  + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
      ggplotly(p)
    }  else if(input$parameter=="17")
    {FU17<-filter(dat,Functional_Unit=="17")
    meansex <-  aggregate(FU17[,c("No_Km2")],by=list(FU17$Year,FU17$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(FU17, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="19")
    {FU19<-filter(dat,Functional_Unit=="19")
    meansex <-  aggregate(FU19[,c("No_Km2")],by=list(FU19$Year,FU19$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(FU19, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    
    ggplotly(p)}
    else if(input$parameter=="20-21")
    {FU20<-filter(dat,Functional_Unit=="20-21")
    meansex <-  aggregate(FU20[,c("No_Km2")],by=list(FU20$Year,FU20$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(FU20, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    
    ggplotly(p)}
    else if(input$parameter=="22")
    {FU22<-filter(dat,Functional_Unit=="22")
    meansex <-  aggregate(FU22[,c("No_Km2")],by=list(FU22$Year,FU22$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(FU22, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)}
    else if(input$parameter=="Outside FU")
    {FUout<-filter(dat,Functional_Unit=="Outside FU")
    meansex <-  aggregate(FUout[,c("No_Km2")],by=list(FUout$Year,FUout$Sex),FUN=mean,  na.rm=TRUE)
    names(meansex)=c("Year", "Sex", "No_Km2")
    p=ggplot(FUout, aes(x=Year, y=No_Km2, colour=Sex)) + geom_jitter(width = 0.05) + 
      geom_line(data=meansex, aes(x=Year, y =No_Km2), size=1)+ ylab("No/KM<sup>2</sup>") + facet_wrap(~Sex)+
      theme_bw() + theme(legend.position = "none")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8))+scale_x_continuous(breaks=dat$Year) + theme(axis.title.x=element_blank())
    ggplotly(p)} 
    
  })
    

 
  
  
  

  ### Length Frequency ###
  ########################
  # Get recent data for current species
  output$lfplotall=renderPlot({
    if(input$parameter=="16"){
      FU16<-filter(dat,Functional_Unit=="16")
      lfAll <- aggregate(FU16[,c("NepCount")],by=list(FU16$Year,FU16$CLmm,FU16$Functional_Unit),FUN=sum,  na.rm=TRUE)
      names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
      p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
        theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
     p
    }  else if(input$parameter=="17")
    {FU17<-filter(dat,Functional_Unit=="17")
    lfAll <- aggregate(FU17[,c("NepCount")],by=list(FU17$Year,FU17$CLmm,FU17$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="19")
    {FU19<-filter(dat,Functional_Unit=="19")
    lfAll <- aggregate(FU19[,c("NepCount")],by=list(FU19$Year,FU19$CLmm,FU19$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="20-21")
    {FU20<-filter(dat,Functional_Unit=="20-21")
    lfAll <- aggregate(FU20[,c("NepCount")],by=list(FU20$Year,FU20$CLmm,FU20$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="22")
    {FU22<-filter(dat,Functional_Unit=="22")
    lfAll <- aggregate(FU22[,c("NepCount")],by=list(FU22$Year,FU22$CLmm,FU22$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="Outside FU")
    {FUout<-filter(dat,Functional_Unit=="Outside FU")
    lfAll <- aggregate(FUout[,c("NepCount")],by=list(FUout$Year,FUout$CLmm,FUout$Functional_Unit),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~FU)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
   
  })
  
  output$lfplotparam=renderPlot({
    if(input$parameter=="16"){
      FU16<-filter(dat,Functional_Unit=="16")
      lfAll <- aggregate(FU16[,c("NepCount")],by=list(FU16$Year,FU16$CLmm,FU16$Functional_Unit,FU16$Sex),FUN=sum,  na.rm=TRUE)
      names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
      p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
        theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
      p
    }  else if(input$parameter=="17")
    {FU17<-filter(dat,Functional_Unit=="17")
    lfAll <- aggregate(FU17[,c("NepCount")],by=list(FU17$Year,FU17$CLmm,FU17$Functional_Unit,FU17$Sex),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="19")
    {FU19<-filter(dat,Functional_Unit=="19")
    lfAll <- aggregate(FU19[,c("NepCount")],by=list(FU19$Year,FU19$CLmm,FU19$Functional_Unit,FU19$Sex),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="20-21")
    {FU20<-filter(dat,Functional_Unit=="20-21")
    lfAll <- aggregate(FU20[,c("NepCount")],by=list(FU20$Year,FU20$CLmm,FU20$Functional_Unit,FU20$Sex),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="22")
    {FU22<-filter(dat,Functional_Unit=="22")
    lfAll <- aggregate(FU22[,c("NepCount")],by=list(FU22$Year,FU22$CLmm,FU22$Functional_Unit,FU22$Sex),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
    else if(input$parameter=="Outside FU")
    {FUout<-filter(dat,Functional_Unit=="Outside FU")
    lfAll <- aggregate(FUout[,c("NepCount")],by=list(FUout$Year,FUout$CLmm,FUout$Functional_Unit,FUout$Sex),FUN=sum,  na.rm=TRUE)
    names(lfAll)=c("Year", "LengthClass", "FU","Sex","NepCount")
    p=ggplot(lfAll, aes(LengthClass, Year, height = NepCount, group = Year,fill=Sex, alpha=.5)) +geom_density_ridges(stat = "identity", scale = 2.6) + facet_wrap(~Sex)+
      theme_bw()  + theme(legend.position = "none")+ geom_vline(xintercept=juv_length_split,lty=2)+scale_y_continuous(breaks=lfAll$Year) 
    p}
  })
 
  ##########################
  ### Length/Weight Plot ###
  ###########################1f77b4  c('Male'='#6699ff','Female'='#ff66cc') c("#132B43", "#56B1F7")
  LengthWeightAgeSp=reactive({
    if(is.null(input$slideryear1)){
      filter(indLW, Weight_g!="NA" & Year == 2017)
    }else{
      filter(indLW, Weight_g!="NA" & Year == input$slideryear1)}
  })
  
  LengthWeightAgeSp1=reactive({
    filter(indLW, Weight_g!="NA")
  })
  
  output$lwplot=renderPlotly({
    if(input$parameter=="16"){
      FU16 <- filter(LengthWeightAgeSp(), Functional_Unit=="16")
      p <- plot_ly(FU16, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    else if(input$parameter=="17"){
      FU17 <- filter(LengthWeightAgeSp(), Functional_Unit=="17")
      p <- plot_ly(FU17, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    else if(input$parameter=="19"){
      FU19 <- filter(LengthWeightAgeSp(), Functional_Unit=="19")
      p <- plot_ly(FU19, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    else if(input$parameter=="20-21"){
      FU20 <- filter(LengthWeightAgeSp(), Functional_Unit=="20-21")
      p <- plot_ly(FU20, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    else if(input$parameter=="22"){
      FU22 <- filter(LengthWeightAgeSp(), Functional_Unit=="22")
      p <- plot_ly(FU22, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    else if(input$parameter=="Outside FU"){
      FUout <- filter(LengthWeightAgeSp(), Functional_Unit=="Outside FU")
      p <- plot_ly(FUout, x = ~CLmm, y = ~Weight_g, type = 'scatter', mode = 'markers',colors=c("Female"="#F8766D","Male"="#00BFC4"),
                   text=~paste("Length:",CLmm,"cm","<br>Weight:",Weight_g,
                               "<br>Sex:",Sex),
                   hoverinfo = 'text',color= ~Sex,
                   marker =list(opacity = 1)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by Sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$CLmm),
                                                            max(LengthWeightAgeSp1()$CLmm)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$Weight_g, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p
    }
    
   
  })
  
  output$LengthWeightUI=renderUI({
    if(dim(LengthWeightAgeSp1())[1]==0){
      h3(paste("No Weight data available for", species, sep=" "))
    }else if(dim(LengthWeightAgeSp())[1]==0){
      h3(paste("No Weight data available for", species, "for", input$slideryear1, ".Try another year", sep= " "))
    }else{
      list(plotlyOutput("lwplot"),
           "*Filtering also available on the RHS by clicking on the legend entry")
    }
  })
  
  
  
  
  
  
  
  
  
  })
