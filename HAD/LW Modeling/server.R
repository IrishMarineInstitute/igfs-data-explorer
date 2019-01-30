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
  species="Haddock"
  ### Get Data - pull out valid stns
  #Length/Weight and Length/Age plots
  LengthWeightAge=readRDS("LengthWeightAge.RDS")
  LengthWeightAge$cohort=LengthWeightAge$Year-LengthWeightAge$fldResult1
  
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
  LengthWeightAgeSpA=reactive({
    if(is.null(input$slideryear1)){
      filter(LengthWeightAge, fldResult1!="NA" & Year == 2017)
    }else{
      filter(LengthWeightAge, fldResult1!="NA" & Year == input$slideryear1)}
  })
  
  LengthWeightAgeSpA1=reactive({
    filter(LengthWeightAge, fldResult1!="NA")
  })
  
  output$laplot=renderPlotly({
    if(input$parameter=="None"){
      svTyp=vbStarts(fldFishLength ~ fldResult1, data=LengthWeightAgeSpA(), plot=TRUE)
      vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
      fitTyp=nls(fldFishLength ~ vbTyp(fldResult1, Linf, K, t0), data=LengthWeightAgeSpA(), start=svTyp)
      predicted_df <- data.frame(length_pred = vbTyp(x, Linf = coef(fitTyp)[[1]], K=coef(fitTyp)[[2]], t0=coef(fitTyp)[[3]]), 
                                 x=seq(0, 13,length.out = 199))
      
      m=ggplot(LengthWeightAgeSpA(), aes(x=fldResult1, y=fldFishLength))+
        geom_point() +
        geom_line(color='red',data = predicted_df, aes(x=x, y=length_pred))+
        scale_x_continuous(limits = c(-0.1, max(LengthWeightAgeSpA1()$fldResult1)))+
        scale_y_continuous(limits = c(min(LengthWeightAgeSpA1()$fldFishLength), 
                                      max(LengthWeightAgeSpA1()$fldFishLength)+1))+
        labs(x = "Age", y="Length")+ theme_bw()
      p=ggplotly(m)
      p = style(p, marker = list(color = '#1f77b4', opacity=0.5),traces = 1,# hoverinfo = "y", 
                text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),
                             hoverinfo = 'text')#,mode = 'markers', marker =list(opacity = 0.5))
      p = style(p, line = list(color = 'grey'),traces = 2,# hoverinfo = "y", 
                #text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),
                hoverinfo = "none")#,mode = 'markers', marker =list(opacity = 0.5))
      
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
      p <- plot_ly(LengthWeightAgeSpA(), y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1),
                   color = ~fldFishSex, colors=c('U'='#999999','M'='#6699ff','O'='#cccccc','F'='#ff66cc','I'='#ccff99'), 
                   hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by sex)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$fldFishLength/10), 
                                                            max(LengthWeightAgeSpA1()$fldFishLength/10)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$fldResult1, na.rm = T)*1.05), zeroline = FALSE), 
               margin=(list(t=70)), showlegend = TRUE) 
      p$elementId <- NULL
    }else if(input$parameter=="Gear"){
      p <- plot_ly(LengthWeightAgeSpA(), y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                   text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1, "<br>gear type:",fldGearDescription),
                   color= ~fldGearDescription, colors=c("#132B43", "#56B1F7"),
                   hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
        layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by gear)"),
               yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$fldFishLength/10), 
                                                            max(LengthWeightAgeSpA1()$fldFishLength/10)+1), zeroline = FALSE),
               xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$fldResult1, na.rm = T)*1.05), 
                            zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE) 
      p$elementId <- NULL
    }else if(input$parameter=="Division"){
      if(is.null(input$division1)){
        ## "Select a Division"
        p=NULL
      }else{
        grspnew.w2 <- filter(LengthWeightAgeSpA(), ICESCODE %in% c(input$division1))
        p <- plot_ly(grspnew.w2, y = ~(fldFishLength+5)/10, x = ~fldResult1, type = 'scatter', 
                     text=~paste("length:",(fldFishLength+5)/10,"cm","<br>age:",fldResult1, "<br>division:",ICESCODE),
                     color= ~ICESCODE, hoverinfo = 'text',mode = 'markers', marker =list(opacity = 0.5)) %>% 
          layout(hovermode="closest", title=paste(species,"Age vs Length (points coloured by divisions)"),
                 yaxis = list(title = 'Length (cm)', range= c(min(LengthWeightAgeSpA1()$fldFishLength/10), 
                                                              max(LengthWeightAgeSpA1()$fldFishLength/10)+1), zeroline = FALSE),
                 xaxis = list(title = 'Age', range = c(-0.1, max(LengthWeightAgeSpA1()$fldResult1, na.rm = T)*1.05), 
                              zeroline = FALSE), margin=(list(t=70)), showlegend = TRUE)  
        p$elementId <- NULL
      }
    }
    if(is.null(p)) plotly_empty(type = "scatter", mode="markers") else p
  })
  })
