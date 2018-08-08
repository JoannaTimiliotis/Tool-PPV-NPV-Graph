#-------------------------------------------------------------------------
# Joanna Timiliotis
# University of Basel
# PPV NPV Tool 
# August 8th, 2018
#-------------------------------------------------------------------------


# Section 1: Load libraries  =============================================    

library(shiny)
library(shinydashboard)
library(ShinyPsych)
library(shinythemes)
library(shinyBS)
library(rintrojs)
library(dygraphs)


#load extra functions
source ("Functions.R")

ui <-navbarPageWithInputs(theme= shinytheme("flatly"), HTML('Graphen <i class="glyphicon glyphicon-signal"></i>'), id = "tabs", selected = "A", 
                          inputs  = (list( )),
                          
                          
        # JS, CSS, HTML Files                
      
        includeCSS("StylesheetConditions.css"),   #Appearance, Layout  
        
        
                        
tabPanel("", value = "A", 
   fluidRow(
           sidebarPanel( id="sidebar",         

         
    
             
# Tooltip________________________________________________________________________________________________________________________
        
bsPopover("Graphen", "Tool Graphen", 
          "placeholder", options = list(container = "bsPopover")),


# Inputs ________________________________________________________________________________________________________________________                           

sliderInput("rangesens", "Sensitivität:",min = 0, max = 1, value = c(0.7,0.9)),
sliderInput("rangespec", "Spezifität:",min = 0, max = 1, value = c(0.7,0.9))
      ),

     mainPanel(  
     
       dygraphOutput("dygraph", width = "90%") 
      
      
       )#endMainPanel 
      )#fluidRow
   )#tabPanel
  )#endNavbarpage

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(  ) 
)  
# Server =======================================================================================================================================
server <- function(input, output, session)  {
  

  
# Popover ______________________________________________________________________________________________________________________________

  
addPopover(session, "rangesens", "Sensitivität", content = HTML("Wahrscheinlichkeit, mit der ein diagnostischer Test einen Kranken korrekt als krank einstuft
"), placement = "bottom", trigger = 'hover')
  
addPopover(session, "rangespec", "Spezifität", content = HTML("Wahrscheinlichkeit, mit der ein diagnostischer Test einen nicht-kranken korrekt als nicht krank einstuft"),  placement = "bottom", trigger = 'hover')
  
addPopover(session, "dygraph", "Prädiktive Werte - Graph", content = HTML('<span style="color: #3498DB;">PPV</span>: Wahrscheinlichkeit, dass bei einem positiven Befund eines Tests die Person auch tatsächlich krank ist<br>
                                                        <br><span style="color: #E74C3C;">NPV</span>: Wahrscheinlichkeit, dass bei einem negativen Befund eines Tests die Person auch tatsächlich nicht krank ist'),  placement = "left", trigger = 'hover')


 # Slider, Controls ______________________________________________________________________________________________________________________________
 

 

output$dygraph <- renderDygraph({
  PRE <- seq(from = 0, to = 1, by = 0.01)
  SEN <-   as.numeric(sub(",", ".", ((input$rangesens[1]+input$rangesens[2])/2))) #allow point AND comma
  SEN0 <-   as.numeric(sub(",", ".", ((input$rangesens[1]+input$rangesens[2])/2))) #allow point AND comma
  SEN1 <-   as.numeric(sub(",", ".", input$rangesens[1])) #allow point AND comma
  SEN2 <-   as.numeric(sub(",", ".", input$rangesens[2])) #allow point AND comma
  SPE <-   as.numeric(sub(",", ".", ((input$rangespec[1]+input$rangespec[2])/2))) #allow point AND comma
  SPE0 <-   as.numeric(sub(",", ".", ((input$rangespec[1]+input$rangespec[2])/2))) #allow point AND comma
  SPE1 <-  as.numeric(sub(",", ".", input$rangespec[1]))  #allow point AND comma
  SPE2 <-  as.numeric(sub(",", ".", input$rangespec[2]))  #allow point AND comma
  PPV <- ((SEN * PRE) / ( (SEN *PRE)+(1-SPE)*(1-PRE) ) ) 
  PPV0 <- ((SEN * PRE) / ( (SEN *PRE)+(1-SPE)*(1-PRE) ) ) 
  PPV1 <- ((SEN1 * PRE) / ( (SEN1 *PRE)+(1-SPE1)*(1-PRE) ) ) 
  PPV1b <- ((SEN1 * PRE) / ( (SEN1 *PRE)+(1-SPE1)*(1-PRE) ) ) 
  PPV2 <- ((SEN2 * PRE) / ( (SEN2 *PRE)+(1-SPE2)*(1-PRE) ) ) 
  PPV2b <- ((SEN2 * PRE) / ( (SEN2 *PRE)+(1-SPE2)*(1-PRE) ) ) 
   NPV <- (((1-PRE)*SPE) / ( ((1-PRE)*SPE)+(PRE*(1-SEN)) ) )  
  NPV0 <- (((1-PRE)*SPE) / ( ((1-PRE)*SPE)+(PRE*(1-SEN)) ) )  
  NPV1 <- (((1-PRE)*SPE1) / ( ((1-PRE)*SPE1)+(PRE*(1-SEN1)) ) )  
  NPV2 <- (((1-PRE)*SPE2) / ( ((1-PRE)*SPE2)+(PRE*(1-SEN2)) ) )  
  NPV1b <- (((1-PRE)*SPE1) / ( ((1-PRE)*SPE1)+(PRE*(1-SEN1)) ) )  
  NPV2b <- (((1-PRE)*SPE2) / ( ((1-PRE)*SPE2)+(PRE*(1-SEN2)) ) )  
  
  
  plotdyPPV0<- data.frame(PRE, PPV0)
  plotdyPPV<- data.frame( PPV)
  plotdyPPV1<- data.frame( PPV1)
  plotdyPPV1b<- data.frame( PPV1b)
  plotdyPPV2<- data.frame(PPV2)
  plotdyPPV2b<- data.frame(PPV2b)
  plotdyNPV<- data.frame( NPV)
  plotdyNPV0<- data.frame( NPV0)
  plotdyNPV1<- data.frame( NPV1)
  plotdyNPV2<- data.frame( NPV2)
  plotdyNPV1b<- data.frame( NPV1b)
  plotdyNPV2b<- data.frame( NPV2b)
  
  
  plotall <-cbind(plotdyPPV0,
                  plotdyPPV, 
                  plotdyNPV0, 
                  plotdyNPV, 
                  plotdyPPV1, 
                  plotdyNPV1,
                  plotdyPPV2,  
                  plotdyNPV2
                 )
  
 
 
  
  dygraph(plotall , main = "PPV/NPV- Kurven", xlab = "Prävalenz",
          ylab = "Prädiktive Werte ")%>%
   # dyShading(from = index[input$range_one[1]], to = index[input$range_one[2]], color = "#FFE6E6") %>%
    #dyShading(from = index[input$range_two[1]], to = index[input$range_two[2]], color = "#CCEBD6")
  
  
  dyOptions(stackedGraph = FALSE, fillGraph = FALSE, fillAlpha = 0.15, stepPlot = FALSE, stemPlot = FALSE, drawPoints = FALSE, pointSize = 1, 
            drawGapEdgePoints = FALSE, connectSeparatedPoints = FALSE, 
            strokeWidth = 1, strokePattern = NULL, strokeBorderWidth = NULL, strokeBorderColor = "white", plotter = NULL, 
            colors = NULL, colorValue = 0.5, colorSaturation = 1, 
            drawXAxis = TRUE, drawYAxis = TRUE, includeZero = FALSE, drawAxesAtZero = FALSE, logscale = FALSE, axisTickSize = 3, 
            axisLineColor = "black", axisLineWidth = 0.3, axisLabelColor = "black", axisLabelFontSize = 14, axisLabelWidth = 60, 
            drawGrid = TRUE, gridLineColor = NULL, gridLineWidth = 0.3, titleHeight = NULL, 
            rightGap = 5, digitsAfterDecimal = 2, labelsKMB = FALSE, labelsKMG2 = FALSE, maxNumberWidth = 6, sigFigs = NULL, 
            panEdgeFraction = NULL, animatedZooms = FALSE, mobileDisableYTouch = TRUE, timingName = NULL, useDataTimezone = FALSE, retainDateWindow = FALSE)%>%
  
    dySeries("PPV",  color = "#3498DB", label = "PPV") %>%
    
    dySeries(c("PPV1", "PPV0", "PPV2"), color = "grey", label = "Range PPV") %>%
    
    dySeries("NPV",  color = "#E74C3C", label = "NPV") %>%
    
    dySeries(c("NPV1", "NPV0", "NPV2"), color = "grey", label = "Range NPV") %>%
    

    dyLegend( show =  "always", width = 100, showZeroValues = FALSE, #labelsDiv = "legendDivID",
              labelsSeparateLines = TRUE, hideOnMouseOut = TRUE)%>%
    
    
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) #%>%
  
  
 
})



  
} #functionEnd
shinyApp(ui, server)

