#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ##########################
  ### reactive filtering ###
  ##########################
  #For Length/Weight and Length/Age plots
  LengthWeightAge=readRDS("LengthWeightAge.RDS")
  LengthWeightAge$cohort=LengthWeightAge$Year - LengthWeightAge$fldResult1
  
  
  LengthWeightAge_missingremoved=filter(LengthWeightAge, !is.na(fldResult1) & fldResult1>=0)
  output$speciesfilter=renderUI({
    specieslist=unique(factor(LengthWeightAge_missingremoved$fldMainSpeciesCode))
    selectInput("species", h3("Select Species"),
                choices = as.list(specieslist), selected = "HAD")
  })
  
  output$yearfilter=renderUI({
    if(input$tabselected=="lw"  | input$tabselected=="la"){
      sliderInput("slideryear1", "Choose Year:", min = 2003, max = 2017, value = 2017, step = NULL, 
                  sep = "", animate = TRUE)
    }
  })
  
  #Divfilter only appears if Division parameter selected
  output$divfilter=renderUI({
    if(input$parameter=="Division"){
       divlist= factor(c("VIa", "VIIb", "VIIc", "VIIg", "VIIj","VIIk"))
       checkboxGroupInput("division1", h3("Select Division"),choices=as.list(sort(divlist)))}
  })
  
  ########################
  ### Reading in files ###
  ########################
  ### Get Data - pull out valid stns
  #Length/Weight and Length/Age plots

  ##########################
  ### Length/Weight Plot ###
  ##########################
  LengthWeightAgeS=reactive({
    filter(LengthWeightAge, fldMainSpeciesCode == input$species)
  })
  
  
  LengthWeightAgeSp=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAgeS(), fldFishWholeWeight!="NA" & Year == 2017)
    }else{
      filter(LengthWeightAgeS(), fldFishWholeWeight!="NA" & Year == input$slideryear1)}
    })
  
  LengthWeightAgeSp1=reactive({
    filter(LengthWeightAgeS(), fldFishWholeWeight!="NA")
  })
  
  output$lwplot=renderPlotly({
    if(input$parameter=="None"){
      LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$fldFishLength),]
      fit1=lm(log10(fldFishWholeWeight) ~ log10(fldFishLength/10+0.5), data = LengthWeightAgeSp)
      
      p=plot_ly(LengthWeightAgeSp, x = ~(fldFishLength+5)/10, y = ~fldFishWholeWeight, type = 'scatter', 
              text=~paste("length:",(fldFishLength+5)/10,"cm","<br>weight:",fldFishWholeWeight),
              hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
       layout(hovermode="closest", title=paste("Length vs Weight"),
              xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$fldFishLength/10),
                                                           max(LengthWeightAgeSp1()$fldFishLength/10)+1)),
              yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
              margin=(list(t=70)), showlegend = FALSE) %>%
       add_trace(x=~(fldFishLength+5)/10, y=10^(fitted(fit1)), type="scatter", 
                 mode='lines+markers',
                 line = list(color = '#1f77b4', shape="spline"),
                 marker = list(color = 'grey', opacity=0))
      p$elementId <- NULL
    }else if(input$parameter=="Sex"){
      #LengthWeightAgeSp=filter(LengthWeightAge, fldFishWholeWeight!="NA" & Year == 2006)
      LengthWeightAgeSp=LengthWeightAgeSp()[order(LengthWeightAgeSp()$fldFishLength),]
      fit2=lm(log10(fldFishWholeWeight) ~ log10(fldFishLength/10+0.5) + fldFishSex, data = LengthWeightAgeSp)
      
      p <- plot_ly(LengthWeightAgeSp, x = ~(fldFishLength/10 +0.5), y = ~fldFishWholeWeight, type = 'scatter', 
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>weight:",fldFishWholeWeight),
                   color = ~fldFishSex, colors=c('U'='#999999','M'='#6699ff','O'='#cccccc','F'='#ff66cc','I'='#ccff99'), 
                   hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste("Length vs Weight (points coloured by sex)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$fldFishLength/10),
                                                            max(LengthWeightAgeSp1()$fldFishLength/10)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE) %>%
      add_trace(x=~(fldFishLength+5)/10, y=10^(fitted(fit2)), type="scatter", 
                mode='lines+markers',
                line = list(shape="spline"),
                marker = list(color = 'grey', opacity=0))
      #add_trace(y = tmpfemale$tmpy, x=tmpfemale$tmpx, type='scatter',
        #          mode='lines+markers',
        #          line = list(color = '#ff66cc', opacity=0.5),
        #          marker = list(color = 'grey', opacity=0), showlegend=FALSE)%>%
        #add_trace(y = tmpmale$tmpy, x=tmpmale$tmpx, type='scatter',
        #          mode='lines+markers',
        #          line = list(color = '#6699ff', opacity=0.5),
        #          marker = list(color = 'grey', opacity=0), showlegend=FALSE)
      p$elementId <- NULL
    }else if(input$parameter=="Gear"){
      p <- plot_ly(LengthWeightAgeSp(), x = ~(fldFishLength+5)/10, y = ~fldFishWholeWeight, type = 'scatter', mode = 'markers',
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>weight:",fldFishWholeWeight,
                               "<br>gear type:",fldGearDescription),
                   hoverinfo = 'text',color= ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                   marker =list(opacity = 0.5)) %>%  
        layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by gear)"),
               xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$fldFishLength/10),
                                                            max(LengthWeightAgeSp1()$fldFishLength/10)+1)),
               yaxis = list(title = 'Weight (g)', range = c(0, max(LengthWeightAgeSp1()$fldFishWholeWeight, na.rm = T)*1.05)),
               margin=(list(t=70)), showlegend = TRUE)
      p$elementId <- NULL
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        grspnew.w2 <- filter(LengthWeightAgeSp(), ICESCODE %in% c(input$division1))
        p <- plot_ly(grspnew.w2, x = ~(fldFishLength+5)/10, y = ~fldFishWholeWeight, type = 'scatter', mode = 'markers',
                     text=~paste("length:",(fldFishLength+5)/10,"cm","<br>weight:",fldFishWholeWeight, "<br>area:",ICESCODE),
                     hoverinfo = 'text',color= ~ICESCODE, marker =list(opacity = 0.5)) %>%  
          layout(hovermode="closest", title=paste(species,"Length vs Weight (points coloured by divisions)"),
                 xaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSp1()$fldFishLength/10),
                                                              max(LengthWeightAgeSp1()$fldFishLength/10)+1)),
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
  LengthWeightAgeSpYear=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAgeS(), fldResult1!="NA" & Year == 2017)
    }else{
      filter(LengthWeightAgeS(), fldResult1!="NA" & Year == input$slideryear1)}
  })
  
  LengthWeightAgeSpAll=reactive({
    filter(LengthWeightAgeS(), fldResult1!="NA")
  })
  
  output$startingvaluesall=renderText({
    paste("Linf=", round(svTyp[[1]],2), ", K=", round(svTyp[[2]],2), ", t0=", round(svTyp[[3]],2))
  })

  
  svTypAll=reactive({
    vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAgeSpAll())
  })
  svTypYear=reactive({
    vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAgeSpYear())
  })
  
  output$startingvaluesall=renderText({
    paste("Starting Values based on all data: Linf=", round(svTypAll()[[1]],2), ", K=", round(svTypAll()[[2]],2), ", t0=", round(svTypAll()[[3]],2))
  })
  
  output$startingvaluesyear=renderText({
    paste("Starting Values based by year: Linf=", round(svTypYear()[[1]],2), ", K=", round(svTypYear()[[2]],2), ", t0=", round(svTypYear()[[3]],2))
  })
  
  fitTypAll1=reactive({
    vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
    nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpYear(), start=svTypAll())
  })
  fitTypAll2=reactive({
    vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
    nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpAll(), start=svTypAll())
  })
  fitTypYear1=reactive({
    vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
    nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpYear(), start=svTypYear())
  })
  
  output$fittedvaluesall=renderText({
    paste("Fitted Values based on all data SV: Linf=", round(coef(fitTypAll1())[[1]],2), ", K=", round(coef(fitTypAll1())[[2]],2), ", t0=", round(coef(fitTypAll1())[[3]],2))
  })
  output$fittedvaluesall2=renderText({
    paste("Fitted Values based on all data SV: Linf=", round(coef(fitTypAll2())[[1]],2), ", K=", round(coef(fitTypAll2())[[2]],2), ", t0=", round(coef(fitTypAll2())[[3]],2))
  })
  output$fittedvaluesyear=renderText({
    paste("Fitted Values based on year data SV: Linf=", round(coef(fitTypYear1())[[1]],2), ", K=", round(coef(fitTypYear1())[[2]],2), ", t0=", round(coef(fitTypYear1())[[3]],2))
  })
  
  
  output$laplot=renderPlotly({
    if(input$parameter=="None"){
      vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
      fitTypAll=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpYear(), start=svTypAll())
      x=seq(0, max(LengthWeightAgeSpYear()$fldResult1) ,length.out = 199)
      predicted_df_all <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTypAll)[[1]], K=coef(fitTypAll)[[2]], t0=coef(fitTypAll)[[3]]), 
                                 x=seq(0, max(LengthWeightAgeSpYear()$fldResult1) ,length.out = 199))
      
      fitTypYear=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpYear(), start=svTypYear())
      x=seq(0, max(LengthWeightAgeSpYear()$fldResult1) ,length.out = 199)
      predicted_df_year <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTypYear)[[1]], K=coef(fitTypYear)[[2]], t0=coef(fitTypYear)[[3]]), 
                                 x=seq(0, max(LengthWeightAgeSpYear()$fldResult1) ,length.out = 199))
      
      fitTypAll2=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpAll(), start=svTypAll())
      x=seq(0, max(LengthWeightAgeSpAll()$fldResult1) ,length.out = 199)
      predicted_df_all2 <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTypAll2)[[1]], K=coef(fitTypAll2)[[2]], t0=coef(fitTypAll2)[[3]]), 
                                     x=seq(0, max(LengthWeightAgeSpAll()$fldResult1) ,length.out = 199))
      
      m=ggplot(LengthWeightAgeSpYear(), aes(x=fldResult1, y=fldFishLength,color=factor(cohort)))+
        geom_point() +
        geom_line(color='red',data = predicted_df_all, aes(x=x, y=length_pred), size=2)+
        geom_line(color='blue',data = predicted_df_year, aes(x=x, y=length_pred), size=1)+
        geom_line(color='green',data = predicted_df_all2, aes(x=x, y=length_pred), size=1)+
        scale_x_continuous(limits = c(-0.1, max(LengthWeightAgeSpAll()$fldResult1)))+
        scale_y_continuous(limits = c(min(LengthWeightAgeSpAll()$fldFishLength), 
                                      max(LengthWeightAgeSpAll()$fldFishLength)+1))+
        labs(x = "Age", y="Length")+ theme_bw()
      p=ggplotly(m)
      p = style(p, marker = list(color = '#1f77b4', opacity=0.5),traces = 1,# hoverinfo = "y", 
                text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),
                             hoverinfo = 'text')#,mode = 'markers', marker =list(opacity = 0.5))
      p = style(p,traces = 2,# hoverinfo = "y", 
               hoverinfo = "none")
      p = style(p,traces = 3,# hoverinfo = "y", 
                hoverinfo = "none")
      
      #p <- plot_ly(LengthWeightAgeSpA(), y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
      #             text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),#, "grams<br>date:", Date), 
      #             hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
      #  layout(hovermode="closest", title=paste(species,"Age vs Length"),
      #         yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$fldFishLength/10), 
      #                                                      max(LengthWeightAgeSpA1()$fldFishLength/10)+1), zeroline = FALSE),
      #         xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$fldResult1, na.rm = T)*1.05), 
      #                      zeroline = FALSE), margin=(list(t=70)), showlegend = FALSE)# %>%
        #add_lines(y = ~fitted(loess(LengthWeightAgeSpA()$fldFishLength/10+0.5 ~ LengthWeightAgeSpA()$fldResult1)),
        #          line = list(color = '#07A4B5'),
        #          name = "Loess Smoother", showlegend = TRUE)
      p$elementId <- NULL
    }else if(input$parameter=="Sex"){
      p <- plot_ly(LengthWeightAgeSpYear(), y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),
                   color = ~fldFishSex, colors=c('U'='#999999','M'='#6699ff','O'='#cccccc','F'='#ff66cc','I'='#ccff99'), 
                   hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by sex)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpAll()$fldFishLength/10), 
                                                            max(LengthWeightAgeSpAll()$fldFishLength/10)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpAll()$fldResult1, na.rm = T)*1.05), zeroline = FALSE), 
               margin=(list(t=70)), showlegend = TRUE) 
      p$elementId <- NULL
    }else if(input$parameter=="Gear"){
      p <- plot_ly(LengthWeightAgeSpYear(), y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1, "<br>gear type:",fldGearDescription),
                   color= ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                   hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by gear)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpAll()$fldFishLength/10), 
                                                            max(LengthWeightAgeSpAll()$fldFishLength/10)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpAll()$fldResult1, na.rm = T)*1.05), 
                            zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE) 
      p$elementId <- NULL
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        grspnew.w2 <- filter(LengthWeightAgeSpYear(), ICESCODE %in% c(input$division1))
        p <- plot_ly(grspnew.w2, y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                     text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1, "<br>division:",ICESCODE),
                     color= ~ICESCODE, hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
          layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by divisions)"),
                 yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpAll()$fldFishLength/10), 
                                                              max(LengthWeightAgeSpAll()$fldFishLength/10)+1), zeroline = FALSE),
                 xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpAll()$fldResult1, na.rm = T)*1.05), 
                              zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)  
        p$elementId <- NULL
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p
  })
  
  
  output$latab=renderUI({
    if(dim(LengthWeightAgeSpAll())[1]==0){
      h3(paste("No Age data available for", input$species, sep=" "))
    }else if(dim(LengthWeightAgeSpYear())[1]==0){
      h3(paste("No Age data available for", input$species, "for", input$slideryear1, ".Try another year", sep= " "))
    }else{
      plotlyOutput("laplot")
    }
  })
  })
