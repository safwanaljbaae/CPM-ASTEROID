#!/bin/bash
# !===============================================================
# !==   Safwan Aljbaae                                          ==
# !==   Division of Space Mechanics and Control, INPE           == 
# !==   C.P. 515, 12227-310 São José dos Campos, SP, Brazi      ==
# !==   TEL      : +55 (12) 997183840                           ==
# !==   email    : safwan.aljbaae@gmail.com                     ==
# !==   Page web : https://sites.google.com/site/safwanaljbaae/ ==
# !==   April 2020                                           ==
# ================================================================

library(shiny)
library(rgl) 
library(plotly)
library(gmailr, warn.conflicts = FALSE)

options(shiny.maxRequestSize=1000*1024^2)


server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
   H_0_pps  <- reactive({ 
#       xx0 <- formatC(as.double(input$H_pps), format="E", digits=3, flag="#")
      xx0 <- input$H_pps
   })
   H_0_orbit <- reactive({ 
#       xx0 <- formatC(as.double(input$H_orbit), format="E", digits=3, flag="#")
      xx0 <- input$H_orbit
   })

   y_0  <- reactive({ 
      xx1 <- formatC(as.double(input$ycol)*10.0, format="E", digits=3, flag="#")
      xx1 <- gsub("E", "D", as.character(xx1))
      xx1 <- gsub("\\.", "", xx1)
   })
   dy_0  <- reactive({ 
      xx2 <- formatC(as.double(input$dycol)*10.0, format="E", digits=3, flag="#")
      xx2 <- gsub("E", "D", as.character(xx2))
      xx2 <- gsub("\\.", "", xx2)
   })

   data_h <- reactive({
      h0 <- H_0_pps()
      if(input$CE == "Only the polyhedral shape of Apophis") {
         inFile <- paste("SCV/Y_POINCARE/Y_WITHOUT_CE/", h0, "/map_",h0,".scv", sep="")
      }
      if(input$CE == "polyhedral shape of Apophis + Planets") {
        inFile <- paste("SCV/Y_POINCARE/Y_WITH_CE/", h0, "/map_",h0,".scv", sep="")
      }
      if(input$CE == "polyhedral shape of Apophis + Planets + SRP") {
         inFile <- paste("SCV/Y_POINCARE/Y_WITH_CE_SRP/", h0, "/map_",h0,".scv", sep="")
      }
     
      df <- read.csv(inFile, header = FALSE, sep = "")
   })
   
   data_x <- reactive({ 
     y0 <- y_0()
     dy0 <- dy_0()
     h0 <- H_0_orbit()
     if(input$CE_1 == "NO") {
        inFile <- paste("SCV/WITHOUT_CE/", h0, "/DATA/coeff_x_0.", y0,"_0.", dy0, ".scv", sep="")
        print(inFile)
     }
     else {
        inFile <- paste("SCV/WITH_CE/", h0, "/DATA/coeff_x_0.", y0,"_0.", dy0, ".scv", sep="")
     }      
     df <- read.csv(inFile, header = FALSE, sep = "")             
  })
  data_y <- reactive({ 
     y0 <- y_0()
     dy0 <- dy_0()
     h0 <- H_0_orbit()
     if(input$CE_1 == "NO") {
        inFile <- paste("SCV/WITHOUT_CE/", h0, "/DATA/coeff_y_0.", y0,"_0.", dy0, ".scv", sep="")
     }
     else {
        inFile <- paste("SCV/WITH_CE/", h0, "/DATA/coeff_y_0.", y0,"_0.", dy0, ".scv", sep="")
     }      
     df <- read.csv(inFile, header = FALSE, sep = "")             
  })
  data_z <- reactive({ 
     y0 <- y_0()
     dy0 <- dy_0()
     h0 <- H_0_orbit()
     if(input$CE_1 == "NO") {
        inFile <- paste("SCV/WITHOUT_CE/", h0, "/DATA/coeff_z_0.", y0,"_0.", dy0, ".scv", sep="")
     }
     else {
        inFile <- paste("SCV/WITH_CE/", h0, "/DATA/coeff_z_0.", y0,"_0.", dy0, ".scv", sep="")
     }      
     df <- read.csv(inFile, header = FALSE, sep = "")             
  })
  
  output$contents_x <- renderTable(align = 'r',{
     if(input$disp == "head") {
        return( head(format(data_x(), digits=6)))
     }
     else {
        return(format(data_x(), digits=6))
     }      
  })
  output$contents_y <- renderTable(align = 'r',{
     if(input$disp == "head") {
        return( head(format(data_y(), digits=6)))
     }
     else {
        return(format(data_y(), digits=6))
     }      
  })
  output$contents_z <- renderTable(align = 'r',{
     if(input$disp == "head") {
        return( head(format(data_z(), digits=6)))
     }
     else {
        return(format(data_z(), digits=6))
     }      
  })
  output$type_orbits <- renderPlotly({
     if(input$CE == "Only the polyhedral shape of Apophis") {
        inage_pps1 <- paste("www/FIGURES/Y_POINCARE/Y_WITHOUT_CE/type_Y_WITHOUT_CE.png", sep="")
     }
     if(input$CE == "polyhedral shape of Apophis + Planets") {
        inage_pps1 <- paste("www/FIGURES/Y_POINCARE/Y_WITH_CE/type_Y_WITH_CE.png", sep="")
     }
     if(input$CE == "polyhedral shape of Apophis + Planets + SRP") {
         inage_pps1 <- paste("www/FIGURES/Y_POINCARE/Y_WITH_CE_SRP/type_Y_WITH_CE_SRP.png", sep="")
     }
     
     plotly_empty(type = "scatter", mode   = 'markers') %>%
              layout(images = list(source = base64enc::dataURI(file = inage_pps1),
                     x = 0.1, y = 0,
                     sizex = 1.0, sizey = 1.0,
                     xref = "paper", yref = "paper",
                     xanchor = "left", yanchor = "bottom"),
                     xaxis = list(showgrid = F, showticklabels=F),yaxis = list(showgrid = F, showticklabels=F),
                     margin = list(t = 1)
              )
     
  })
  output$Plot_PPS <- renderPlotly({
     if (input$pps_hd){
        df_h <- data_h()
        names(df_h) <- c("y0", "dy0", "y", "dy")
        pal <- c('blue','yellow','green', 'pink', 'red', 'black')
        plot_ly(df_h, x = ~y, y = ~dy, type = "scatter",  mode = 'markers', 
#             marker = list( color=~y0, size=1.0, opacity=.9, colorscale='Hot', colorbar=list(title='Y0 (km)'), cmin = 0.0, cmax = 10.0)) %>% 
              marker = list( color="black", size=1.0, opacity=.9)) %>% 
        layout(xaxis = list(title = "Y (km)", range = c(-4, 4)),
           yaxis = list(title = "Y' (km/s)", range = c(-8.0e-5, 8.0e-5)))
#         layout(xaxis = list(title = "Y (km)", range = c(-2, 2)),
#            yaxis = list(title = "Y' (km)", range = c(-6.0e-5, 6.0e-5)))   
     }
     else{
        h0 <- H_0_pps()
        if(input$CE == "Only the polyhedral shape of Apophis") {
            inage_pps2 <- paste("www/FIGURES/Y_POINCARE/Y_WITHOUT_CE/map_", h0, ".png", sep="")
        }
        if(input$CE == "polyhedral shape of Apophis + Planets") {
            inage_pps2 <- paste("www/FIGURES/Y_POINCARE/Y_WITH_CE/map_", h0, ".png", sep="")
        }
        if(input$CE == "polyhedral shape of Apophis + Planets + SRP") {
            inage_pps2 <- paste("www/FIGURES/Y_POINCARE/Y_WITH_CE_SRP/map_", h0, ".png", sep="")
        }
           plotly_empty(type = "scatter", mode   = 'markers') %>%
              layout(images = list(source = base64enc::dataURI(file = inage_pps2),
                     x = 0.1, y = 0,
                     sizex = 1.0, sizey = 1.0,
                     xref = "paper", yref = "paper",
                     xanchor = "left", yanchor = "bottom"),
                     xaxis = list(showgrid = F, showticklabels=F),yaxis = list(showgrid = F, showticklabels=F),
                     margin = list(t = 1)
              )
     }
     
  })
  
 
  
  output$MyPlot <- renderPlotly({
    
    step <- 0.000347222 * 50.0
    n_t <- as.double(input$time)/step
    
    data_orbit <- matrix(nrow=n_t, ncol=3)

    df_x <- data_x()
    a_x <- df_x[1,1]
    b_x <- df_x[1,2]
    c_x <- df_x[1,3]
    n_coef_x <- dim(df_x)[1]
    
    df_y <- data_y()
    a_y <- df_y[1,1]
    b_y <- df_y[1,2]
    c_y <- df_y[1,3]
    n_coef_y <- dim(df_y)[1]
    
    df_z <- data_z()
    a_z <- df_z[1,1]
    b_z <- df_z[1,2]
    c_z <- df_z[1,3]
    n_coef_z <- dim(df_z)[1]
    
#     ellipsoid of the central body
    a_ellips = 0.261132561887 
    b_ellips = 0.181366924232 
    c_ellips = 0.159437219106

    for(i in 1:n_t) {
       t=step*i
       data_orbit[i,1] <- a_x + b_x*t + c_x*t*t
       data_orbit[i,2] <- a_y + b_y*t + c_y*t*t
       data_orbit[i,3] <- a_z + b_z*t + c_z*t*t
       for(j in 2:n_coef_x) {
           data_orbit[i,1] <- data_orbit[i,1] + df_x[j,2] * sin(t*df_x[j,1]) +
                                                df_x[j,3] * cos(t*df_x[j,1]) +
                                                df_x[j,4] * t*sin(t*df_x[j,1]) +
                                                df_x[j,5] * t*cos(t*df_x[j,1])
        }
        for(j in 2:n_coef_y) {
           data_orbit[i,2] <- data_orbit[i,2] + df_y[j,2] * sin(t*df_y[j,1]) +
                                                df_y[j,3] * cos(t*df_y[j,1]) +
                                                df_y[j,4] * t*sin(t*df_y[j,1]) +
                                                df_y[j,5] * t*cos(t*df_y[j,1])
        }
        for(j in 2:n_coef_z) {
           data_orbit[i,3] <- data_orbit[i,3] + df_z[j,2] * sin(t*df_z[j,1]) +
                                                df_z[j,3] * cos(t*df_z[j,1]) +
                                                df_z[j,4] * t*sin(t*df_z[j,1]) +
                                                df_z[j,5] * t*cos(t*df_z[j,1])
       }
       
       ellips_eq =(data_orbit[i,1]/a_ellips)**2 + (data_orbit[i,2]/b_ellips)**2 + (data_orbit[i,3]/c_ellips)**2
       d = (data_orbit[i,1])**2 + (data_orbit[i,2])**2 + (data_orbit[i,3])**2
       if(ellips_eq < 1.0) {
          break
       }
       if(d > 1.0e2) {
          break
       }


    }

    faces <- read.csv("SCV/shape_f.dat", header = FALSE, sep = "")
    vertices <- read.csv("SCV/shape_v.dat", header = FALSE, sep = "")
    for(i in 1:dim(faces)[1]) {
       faces$V1[i] = faces$V1[i] - 1
       faces$V2[i] = faces$V2[i] - 1
       faces$V3[i] = faces$V3[i] - 1
    }

    data_orbit <- data.frame(data_orbit)
    names(data_orbit) <- c("x", "y", "z")   
    amax <- 1.25* max(max(abs(data_orbit$x), na.rm = TRUE), max(abs(data_orbit$y), na.rm = TRUE), max(abs(data_orbit$z), na.rm = TRUE))
    
#     plot_ly(x = ~vertices$V1, y = ~vertices$V2, z = ~vertices$V3, i = ~faces$V1, j = ~faces$V2, k= ~faces$V3,type = 'mesh3d')


    fig <- plot_ly(data_orbit, x = ~x, y = ~y, z = ~z, colors = c('blue'), mode="marker", marker = list(size = 0.5, opacity = 1.0), linetype = 0)%>%
    add_trace(x = ~vertices$V1, y = ~vertices$V2, z = ~vertices$V3, i = ~faces$V1, j = ~faces$V2, k= ~faces$V3, type = 'mesh3d')
    fig <- fig %>% add_markers()
    mode(fig)
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'X (km)', range = c(-amax,amax)), 
                                       yaxis = list(title = 'Y (km)', range = c(-amax,amax)),
                                       zaxis = list(title = 'Z (km)', range = c(-amax,amax))))
    
#     fig
    
  })
  
    observe({
       if(is.null(input$send) || input$send==0) return(NULL)
#           use_secret_file("SCV/R_contact_me.json")
          gm_auth_configure(path = "SCV/R_contact_me.json")
    
#     create a MIME email object:
    email <- gm_mime() %>%
    gm_to(input$to) %>%
    gm_from(input$from) %>%
    gm_subject(input$subject) %>%
    gm_text_body(input$message)
    gm_send_message(email)
    

    
    
    
    
  })
  

})
