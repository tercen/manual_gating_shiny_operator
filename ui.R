library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)
library(spsComps)

shinyUI(
  
  fixedPage(
  # fillPage(
    shinyjs::useShinyjs(),
    spsDepend('shinyCatch'),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(type = "text/javascript", src = "gating.js"),
    ),
    

    fixedRow(
      
      column( 12, 
              HTML('<button type="button" id="polyDrawBtn" class="btn-tool"  data-group="draw"  title="Polygon drawing mode">
                              <img class="btn-icon" src="poly_gate.png" onClick=select_button("polyDrawBtn"); 
                                id="polyDrawBtnImg" style="visibility:hidden"></img>
                            </button>'),
              HTML('<button type="button" id="quadDrawBtn" class="btn-tool" data-group="draw" title="Quadrant drawing mode">
                              <img class="btn-icon" src="quad_gate.png" onClick=select_button("quadDrawBtn"); 
                              id="quadDrawBtnImg" style="visibility:hidden"></img>
                            </button>'),
              HTML('<button type="button" id="circDrawBtn" class="btn-tool" data-group="draw" title="Ellipsoid drawing mode">
                              <img class="btn-icon" src="ellipsoid_gate.png"  onClick=select_button("circDrawBtn"); 
                              id="circDrawBtnImg" style="visibility:hidden"></img>
                            </button>'),
              HTML('<button type="button" id="lineDrawBtn" class="btn-tool" data-group="draw" title="Line drawing mode"
                              style="visibility:hidden; display:none">
                              <img class="btn-icon" src="1d_gate.png" onClick=select_button("lineDrawBtn");></img>
                            </button>'),
              HTML('<span class="separator">|</span>'),
              HTML('<button type="button" id="eraseBtn" class="btn-tool" title="Erase polygon">
                              <img class="btn-icon" src="eraser_icon.png" onClick=clear_poly(); 
                              id="eraseBtnImg" style="visibility:hidden"></img>
                            </button>'),
              HTML('<div id="saveDisabledDiv" class="div-btnDisable" >
                            <button type="button" id="saveBtn" class="btn-tool" title="Save gate">
                              <img class="btn-icon" src="save_icon.png" onClick=save_gate(); 
                              id="saveBtnImg" style="visibility:hidden;"></img>
                            </button></div>'),
              HTML('<img  src="Connecting.gif" style="position: absolute; right: 0px; margin-top: 5px; margin-right: 10px;" 
                   height="30" width="150" id="connectGif"/>'),
              class="toolbar", 
              fixedRow(
                column(8, tags$canvas(id="gate_canvas", style="background-color: white;" ) ),
                column(1, 
                       imageOutput(outputId = "image_div")   ,  
                       style = "height:5px; ") #visibility:hidden;
              ),
              
              fixedRow( column(6, HTML('<span id="removedData">0 out 0 non-finite data points removed.</span>'),
                               style="margin-left:55px; margin-top: -45px; font-style: italic; font-size: 10px; visibility: hidden")),
              
              
              fixedRow(
               column(2, textInput("gateFlagPref", "Population", value="Pop"), 
                              class="input-row",
                              style="top: 200%"),
              ),
              
              fixedRow(
                column(2, uiOutput("fileChooser"),
                       class="input-row",
                        style="top: 300%; visibility: hidden;")
              )
              
              ) # END column(12) -- top column
      
    ) # END fiexdRow
      
 
        
        
      
  ) # END fixedPage
) # END shinyUI

