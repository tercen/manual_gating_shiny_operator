library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)

shinyUI(
  
  fillPage(
    shinyjs::useShinyjs(),

    tags$head(HTML("<script type='text/javascript' src='gating.js'></script>")),
    tags$style(HTML('
                .toolbar-el{
                    margin-left:0px; 
                    margin-right:-25px;
                    padding:0px;
                }
                  
                .btn-tool {
                    background-color:#b7e0ed;
                    border-style:none;
                    margin: 5px 5px 1px 5px;
                    padding: 0px;
                }
                

                .btn-tool:hover {
                    background-color:#9ce6ff;
                    border-style:none;
                }
                

                .btn-active {
                    background-color:#b7e0ed;
                    border-style:solid;
                    border-color:#d9f573;
                    margin: 4px 4px 0px 4px;
                    padding: 0px;
                }
                
                .btn-active:hover {
                    background-color:#bed177;
                    border-style:solid;
                    border-color:#d9f573;
                }
                
                
                .span4 { max-width: 24px; }
                .well { max-width: 24px; }
                ' )),
      
    
    
        sidebarLayout(
          sidebarPanel(  fluidRow(column(1, HTML('<button type="button" id="polyDrawBtn" class="btn-tool" title="Polygon drawing mode">
                              <img width="24" height="24" src="poly_draw.png" onClick=select_button("polyDrawBtn");></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('<button type="button" id="quadDrawBtn" class="btn-tool" title="Quadrant drawing mode">
                              <img width="24" height="24" src="quadrant_draw.png" onClick=select_button("quadDrawBtn");></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('<button type="button" id="circDrawBtn" class="btn-tool" title="Ellipsoid drawing mode">
                              <img width="24" height="24" src="ellipsoid_gate.png" onClick=select_button("circDrawBtn");></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('<button type="button" id="lineDrawBtn" class="btn-tool" title="Line drawing mode"
                              style="visibility:hidden; display:none">
                              <img width="24" height="24" src="1d_draw.png" onClick=select_button("lineDrawBtn");></img>
                            </button>'), class="toolbar-el", align='left' ) ),

                         fluidRow(column(1, HTML('</br>'))),
                         
                         fluidRow(column(1, HTML('<button type="button" id="eraseBtn" class="btn-tool" title="Erase polygon">
                              <img width="24" height="24" src="erase.png" onClick=clear_poly();></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('</br>'))),
                         
                         fluidRow(column(1, HTML('<button type="button" id="saveBtn" class="btn-tool" title="Save gate">
                              <img width="24" height="24" src="save.png" onClick=save_gate();></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('</br>'))),
                         
                         fluidRow(column(1, HTML('<button type="button" id="linearBtn" class="btn-tool" title="Linear Transform">
                              <img width="24" height="24" src="identity.png" onClick=select_transform("linear");></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('<button type="button" id="biexpBtn" class="btn-tool" title="Biexp Transform">
                              <img width="24" height="24" src="biexp.png" onClick=select_transform("biexp");></img>
                            </button>'), class="toolbar-el", align='left')),
                         
                         fluidRow(column(1, HTML('<button type="button" id="logBtn" class="btn-tool" title="Logicle Transform">
                              <img width="24" height="24" src="log.png" onClick=select_transform("logicle");></img>
                            </button>'), class="toolbar-el", align='left')),
                         width = 1, fluid=FALSE),
          mainPanel(
            
            fluidRow(
              column(6, 'Manual Gating', id="tool_label", class="toolbar-el", style="font-weight:bold; font-size:36px",
                     align='center')),
            tags$canvas(id="gate_canvas", style="background-color: white"),
            fluidRow( column(8, imageOutput(outputId = "image_div")   ,  
                             style = "height:5px; visibility:hidden") )
          ) # END mainPanel
        )
    
        
        
      # )
  ) # END filPage
) # END shinyUI

