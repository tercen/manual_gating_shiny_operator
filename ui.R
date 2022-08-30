library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)

shinyUI(
  fixedPage(
  # fillPage(
    shinyjs::useShinyjs(),

    tags$head(HTML("<script type='text/javascript' src='gating.js'></script>")),
    tags$style(HTML('
                .toolbar-el{
                    margin-left:0px; 
                    margin-right:-25px;
                    padding:0px;
                }
                  
                .btn-tool {
                    background-color:#eff5ffff;
                    border-style:none;
                    margin: 0px 0px 0px 5px;
                    padding: 0px;
                }
                

                .btn-tool:hover {
                    background-color:#dae8ffff;
                    border-style:none;
                }
                

                .btn-active {
                    background-color:#d9e7feff;
                    border-style:none;
                    margin: 0px 0px 0px 5px;
                    padding: 0px;
                }
                
                .btn-active:hover {
                    background-color:#dae8ffff;
                    border-style:none;
                }
                
                .btn-icon{
                  height: 28px;
                  width: 28px;
                  margin-top: 5px;
                  vertical-align: middle;
                  display: block;
                }
                .toolbar{
                  position:absolute; 
                  left: 0px; 
                  height:40px; 
                  line-height:40px;
                  background-color: #eff5ffff
                }
                
                .separator{
                  display:inline-block;
                  color:#dae8ffff; 
                  font-size:26px; 
                  height: 28px;
                  margin-left:0.25em;
                  margin-right:0.25em;
                  margin-top: 0em;
                  padding: 0px;
                  vertical-align: top;
                }
                ' )),
      
    fixedRow(
      column( 12, 
              HTML('<button type="button" id="polyDrawBtn" class="btn-tool"  data-group="draw"  title="Polygon drawing mode">
                              <img class="btn-icon" src="poly_gate.png" onClick=select_button("polyDrawBtn");></img>
                            </button>'),
              HTML('<button type="button" id="quadDrawBtn" class="btn-tool" data-group="draw" title="Quadrant drawing mode">
                              <img class="btn-icon" src="quad_gate.png" onClick=select_button("quadDrawBtn");></img>
                            </button>'),
              HTML('<button type="button" id="circDrawBtn" class="btn-tool" data-group="draw" title="Ellipsoid drawing mode">
                              <img class="btn-icon" src="ellipsoid_gate.png"  onClick=select_button("circDrawBtn");></img>
                            </button>'),
              HTML('<button type="button" id="lineDrawBtn" class="btn-tool" data-group="draw" title="Line drawing mode"
                              style="visibility:hidden; display:none">
                              <img class="btn-icon" src="1d_gate.png" onClick=select_button("lineDrawBtn");></img>
                            </button>'),
              HTML('<span class="separator">|</span>'),
              HTML('<button type="button" id="eraseBtn" class="btn-tool" title="Erase polygon">
                              <img class="btn-icon" src="eraser_icon.png" onClick=clear_poly();></img>
                            </button>'),
              HTML('<button type="button" id="saveBtn" class="btn-tool" title="Save gate">
                              <img class="btn-icon" src="save_icon.png" onClick=save_gate();></img>
                            </button>'),
              HTML('<span class="separator">|</span>'),
              HTML('<button type="button" id="linearBtn" class="btn-tool" data-group="trans" title="Linear Transform">
                              <img class="btn-icon" src="identity_icon.png" onClick=select_transform("linearBtn");></img>
                            </button>'),
              HTML('<button type="button" id="biexpBtn" class="btn-tool" data-group="trans" title="Biexp Transform">
                              <img class="btn-icon" src="biexp_icon.png" onClick=select_transform("biexpBtn");></img>
                            </button>'),
              HTML('<button type="button" id="logBtn" class="btn-tool" data-group="trans" title="Logicle Transform">
                              <img class="btn-icon" src="logicle_icon.png" onClick=select_transform("logBtn");></img>
                            </button>'),
              class="toolbar",
              fixedRow(
                column(8, tags$canvas(id="gate_canvas", style="background-color: white;" ) ),
                column(2, "Parameter Panel", style=" border-style:solid; position:absolute; left:650px; visibility:hidden;"),
                column(1, 
                       imageOutput(outputId = "image_div")   ,  
                       style = "height:5px; visibility:hidden;")
                
              )
              ) # END column(12) -- top column
      
    ) # END fiexdRow
      
 
        
        
      
  ) # END fixedPage
) # END shinyUI

