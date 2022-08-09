library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)

# TODO
# Check if new point is close enough to another point, if yes, close the polygon
# When polygon is closed, send a message to the server to process who is in and out
# Send back the percentage of cells in and out of gate


shinyUI(
  
  fillPage(

    shinyjs::useShinyjs(),
    
    tags$head(HTML("<script type='text/javascript' src='gating.js'></script>")),

      sidebarLayout( 
        sidebarPanel(
          actionButton('clearBtn', 'Clear'),
          actionButton('saveBtn', 'Save')
        ),
        
        mainPanel(
          
          tags$canvas(id="gate_canvas", style="background-color: white"),
          fluidRow( column(8, imageOutput(outputId = "image_div")   ,  
                           style = "height:5px; visibility:hidden") )
        ), # END mainPanel
        fluid=FALSE, position='right'
      )
  ) # END fluidPage
) # END shinyUI

