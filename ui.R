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
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
library(shinyAce)
library(vembedr)


ui <- shinyUI(fluidPage(
  titlePanel("Close Proximity Motion relative to an Asteroid"),
  tabsetPanel(
     tabPanel("Home",
        titlePanel("CPM-Asteroid:"),
        sidebarPanel(
           p("Please note that this site is under development so you will notice a lot of amendments on each visit", style = "color:red; font-family: 'times'; font-size:18pt"),
           width = 8
        ),

        sidebarPanel(
           p("  The Close Proximity Motion relative to an Asteroid (CPM-Apophis) is an interactive site that contains the motion of a massless particle around An asteroid with irregular shape.", style = "color:black; font-family: 'times'; font-size:14pt"),
           width = 8
        )
     ),
     tabPanel("(99942) Apophis",
        tabsetPanel(
           tabPanel("PPS",
              tags$head(tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
              HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js"           integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
              HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
              HTML('
                 <script>
                    document.addEventListener("DOMContentLoaded", function(){renderMathInElement(document.body, {delimiters: [{left: "$", right: "$", display: false}]});})
                 </script>')
              ),
              titlePanel("Poincaré surfaces of section"),
              sidebarPanel(
                 p(" To build the Poincaré surfaces of section in the potential of Apophis in the body-fixed frame, we use the mechanical energy of an orbits around our target as presented in the folwing:", style = "color:black; font-family: 'times'; font-size:14pt"),
                 p(" $ \\\\$"),
                 
                 p(" $H = \\frac{1}{2}(\\dot{x}^2+\\dot{y}^2+\\dot{z}^2) - \\frac{1}{2} \\omega^{2}(x^2 + y^2) -  U \\\\$", style = "color:black; font-family: 'times'; font-size:12pt"),
                 
                 p("where: $x,y,x$ and $\\dot{x},\\dot{y},\\dot{x}$ are the location and velocity of the particle in the body-fixed frame of reference. $U$ is the gravitational potential of the asteroid calculated considering its polyhedral shape.
                 $\\\\$", style = "color:black; font-family: 'times'; font-size:14pt"),
                 
                 p("The equatorial prograde motion of a massless particle around Apophis is determined numerically with the Runge-Kutta 7/8 integrator with variable step size, optimized for the accuracy of $10^{-12}$, covering a period of 1000 days. However, this does not necessarily ensure that the nature of all orbits remains unchanged in time, because some orbits can become chaotic as time goes on. We distributed our initial conditions in the y-axis, with $x_{0} = z_{0} = \\dot{y}_{0} = \\dot{z}_{0} = 0$ and $\\dot{x}_{0}$ was computed according to the equation of the mechanical energy. We variy $y_{0}$ between 0.5 and 10 km from the asteroid centre with an interval of 0.1 km.", style = "color:black; font-family: 'times'; font-size:14pt"),
                 
                 
                 p("We classified the orbit types into (i) bounded motion: the test body stays inside the system’s disc with a radius chosen to be 10 times greater than Apophis Hill sphere (34 km); (ii) escape motion: the distance from the central body becomes greater than 400 km; (iii) collision: a collision with the central body occurs when the particle crosse the limit of the polyhedral shape of Apophis ", style = "color:black; font-family: 'times'; font-size:14pt"),
                 
                 uiOutput('ex1'),
                 width = 10
              ),
              sidebarPanel(
                 selectInput('H_pps', 'Mechanical energy', choices = c("0.100E-09", "0.200E-09", "0.300E-09", "0.400E-09", "0.500E-09", "0.600E-09", "0.700E-09", "0.800E-09", "0.900E-09", "1.000E-09", "1.100E-09", "1.200E-09", "1.300E-09", "1.400E-09", "1.500E-09", "1.600E-09", "1.700E-09", "1.800E-09", "1.900E-09", "2.000E-09", "2.100E-09", "2.200E-09", "2.300E-09", "2.400E-09", "2.500E-09", "2.600E-09", "2.700E-09", "2.800E-09", "2.900E-09", "3.000E-09", "3.000E-09", "3.100E-09", "3.200E-09", "3.300E-09", "3.400E-09", "3.500E-09", "3.600E-09", "3.700E-09", "3.800E-09", "3.900E-09", "4.000E-09", "4.100E-09", "4.200E-09", "4.300E-09", "4.400E-09", "4.500E-09", "4.600E-09", "4.700E-09", "4.800E-09", "4.900E-09", "5.000E-09"
                 )),
                 selectInput('CE', 'Perturbation', choices = c("Only the polyhedral shape of Apophis", "polyhedral shape of Apophis + Planets", "polyhedral shape of Apophis + Planets + SRP")),
                 
                 checkboxInput("pps_hd", "High resolution ", FALSE)
              ),
              mainPanel(
#                  plotlyOutput('Plot_PPS')
#                  column(width=12,offset=0,plotlyOutput('type_orbits',width="50%",height="350px")),
#                  column(width=12,offset=0,plotlyOutput('Plot_PPS',width="50%",height="350px"))
                 
                 fluidPage(fluidRow(column(5, plotlyOutput("Plot_PPS")), column(5, plotlyOutput("type_orbits"))))
                 
              )
           ),
           
            
           
           tabPanel("Orbits",
              pageWithSidebar(
                 headerPanel('Plot the Orbits'),
                 sidebarPanel(
                    # "Empty inputs" - they will be updated after the data is uploaded
                    selectInput('H_orbit', 'Mechanical energy', choices = c("0.100E-09", "0.200E-09", "0.300E-09", "0.400E-09", "0.500E-09", "0.600E-09", "0.700E-09", "0.800E-09", "0.900E-09", "1.000E-09", "1.100E-09", "1.200E-09", "1.300E-09", "1.400E-09", "1.500E-09", "1.600E-09", "1.700E-09", "1.800E-09", "1.900E-09", "2.000E-09", "2.100E-09", "2.200E-09", "2.300E-09", "2.400E-09", "2.500E-09", "2.600E-09", "2.700E-09", "2.800E-09", "2.900E-09", "3.000E-09")),
                    selectInput('ycol', 'Y Variable', seq(0.5, 10, by=0.1)),
                    selectInput('dycol', 'Y\' Variable', choices = c("0.0")),
                    selectInput('CE_1', 'Considering the CE with Earth', choices = c("NO")),
                    textInput(inputId = "time", label = "Time in days:", value = "30"),
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display", choices = c(Head = "head",  All = "all"), selected = "head")
                 ),
                 mainPanel(
                    sidebarPanel(
                       p(" The equatorial prograde motion of a massless particle around (99942) Apophis is determined numerically with the Runge-Kutta 7/8 integrator with variable step size, optimized for the accuracy of $10^{12}$. The dynamic environment is developed in the full gravitational potential of the central body, taking into account its 3D irregular shape. In this site, we present some orbits in our system.", style = "color:black; font-family: 'times'; font-size:14pt"), 
                       
                       p("In order to characterize the periodicity of these orbits, we perform a frequency analysis of the x, y, and z-coordinate of each orbit and determine the leading frequencies. For that purpose, we first evaluated and removed the quadratic variation of the x-coordinate of the form:", style = "color:black; font-family: 'times'; font-size:14pt"),
                       
                       p("$\\alpha + \\beta t + \\gamma t^{2}$. where ($\\alpha, \\beta, and \\gamma$) are presented in the first line of each table below.", style = "color:black; font-family: 'times'; font-size:14pt"),

                       p("Then, we performed the frequency analysis of the signal, using the Fast Fourier Transform (FFT) to determine the leading frequencies. Our analysis are well adapted for dense polynomials using the software TRIP developed at the IMCCE-Paris Observatory", style = "color:black; font-family: 'times'; font-size:14pt"),
                       
                       p("Finally, we carried out a nonlinear regression in which our signal is modelled by the least-square method following an expression combining Fourier-type and Poisson-type components in the form:", style = "color:black; font-family: 'times'"),
                       
                       p(" $\\newcommand{\\dps}{\\displaystyle} x(t) = \\displaystyle\\sum_{i=1}^{N} \\bigg[\\dps\\underbrace{A_{i}\\sin(f_{i}t) + B_{i}\\cos(f_{i}t)}_\\textrm{Fourier} + \\dps\\underbrace{C_{i}t \\sin(f_{i}t) + D_{i}t \\cos(f_{i}t}_\\textrm{Poisson})\\bigg]$", style = "color:black; font-family: 'times'; font-size:14pt"),
                       
                       p(" $N$ is the number of the frequency, $A, B, C, \\textrm{and} D$ presented in the tables below (From line 2)", style = "color:black; font-family: 'times'; font-size:14pt"),
                       
#                        
                       
                       width = 10
                    ),
                    sidebarPanel(
                       plotlyOutput('MyPlot'),
                       titlePanel("X-coordinate"),
                       tableOutput('contents_x'),
                       titlePanel("Y-coordinate"),
                       tableOutput('contents_y'),
                       titlePanel("Z-coordinate"),
                       tableOutput('contents_z'),
                       width = 10
                    )
                 )
              )
           ),
           
           tabPanel("CE",
              pageWithSidebar(
                 headerPanel('Close Appoch with the Earth'),
                 sidebarPanel(
                    p("text", style = "color:black; font-family: 'times'; font-size:14pt"),
                 ),
                 mainPanel(
                    # tags$img(src = "FIGURES/WITHOUT_CE/map_1.600E-09.png"), # the immage need to be in the folder www
                    tags$video(src = "ce_apophis.mp4", width = "70%", height = "auto", type = "video/mp4", controls = "controls"),
                 )
              )
           )
        )
     ),
     tabPanel(title = "Contact Us",
        pageWithSidebar(
           headerPanel(' '),
              sidebarPanel(
                 p("For questions or comments about this data sets, please contact", style = "color:black; font-family: 'times'; font-size:14pt"),
                 p("Safwan Aljbaae (safwan.aljbaae@gmail.com)", style = "color:black; font-family: 'times'; font-size:14pt"),

              ),
              mainPanel(

              )
           )

     
#         shinyUI(
#            pageWithSidebar(
#               headerPanel("Email sender"),
#               sidebarPanel(
#                  textInput("from", "From:", value="safwanaljbaae@yahoo.com"),
#                  textInput("to", "To:", value="safwan.aljbaae@gmail.com"),
#                  textInput("subject", "Subject:", value="test"),
#                  actionButton("send", "Send mail")
#               ),
#               mainPanel(
#                  aceEditor("message", value="write message here"
#                  )
#               )
#            )
#         )
     )
     

    
#     No video with supported format and MIME type found
    

  )
)

)
