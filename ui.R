library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)

# TODO
# Check if new point is close enough to another point, if yes, close the polygon
# When polygon is closed, send a message to the server to process who is in and out
# Send back the percentage of cells in and out of gate

js <- "
$(document).on('shiny:sessioninitialized', function(event) {
  Shiny.setInputValue('pageLoaded', Math.random());
});

function render(canvas, points) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      for (let i = 0; i < points.length; i++) {
        ctx.beginPath();
        ctx.arc(points[i].x, points[i].y, 3, 0, Math.PI * 2, false);
        ctx.strokeStyle = rgbToHex(0, 255, 0);
        ctx.stroke();

        if( i >= 1){
            ctx.moveTo(points[i-1].x, points[i-1].y);
            ctx.lineTo(points[i].x, points[i].y);
            ctx.stroke();
        }
        
      }
      ctx.restore();
} // END OF render

globalThis.plist = new Array();
"




shinyUI(
  
  fillPage(

    shinyjs::useShinyjs(),
    
    tags$script(HTML(js)),

    tags$head(HTML("<script type='text/javascript'>
    
                    function componentToHex(c) {
                      var hex = c.toString(16);
                      return hex.length == 1 ? '0' + hex : hex;
                    }
                    
                    function rgbToHex(r, g, b) {
                      return '#' + componentToHex(r) + componentToHex(g) + componentToHex(b);
                    }
              
              
                $(document).on('shiny:value', function(event) {
                   // TODO Check to see it is the image
                   if (event.target.id === 'image_div') {
                      Shiny.setInputValue('pageLoaded', Math.random());
                   }
                    
                });
                
                Shiny.addCustomMessageHandler('clear_poly', function(msg){
                    globalThis.plist = new Array();
                    var channel_image = document.getElementById('channel_image');
                    var canvas = document.getElementById('gate_canvas');
                    
                    canvas.width = channel_image.naturalWidth;
                    canvas.height = channel_image.naturalHeight;
                    
                    var ctx = canvas.getContext('2d');
                    ctx.clearRect(0, 0, canvas.width, canvas.height);
                    ctx.drawImage(channel_image,0,0);
                })
                
                Shiny.addCustomMessageHandler('pct_selected', function(pct_msg){
                    var channel_image = document.getElementById('channel_image');
                    var canvas = document.getElementById('gate_canvas');
                    
                    canvas.width = channel_image.naturalWidth;
                    canvas.height = channel_image.naturalHeight;
                    
                    
                    
                    var ctx = canvas.getContext('2d');
                    ctx.drawImage(channel_image,0,0);
                    render(canvas, globalThis.plist);
                    
                    let x = 0;
                    let y = 0;
                    
                    for (let i = 0; i < globalThis.plist.length; i++) {
                        x = x + globalThis.plist[i].x
                        y = y + globalThis.plist[i].y
                    }
                    
                    x = x / globalThis.plist.length;
                    y = y / globalThis.plist.length;
                    
                    ctx.save();
                    ctx.font = '16px Arial'
                    
                    ctx.textBaseline = 'top';
                    //ctx.textAlign = 'center';
                    var text_width = ctx.measureText(pct_msg).width*1.1;

                    
                    ctx.fillStyle = 'rgba(128, 128, 128, 0.6)';
                    ctx.fillRect(x-5, y-5, text_width, parseInt('20px Arial', 10));
                    
                    ctx.fillStyle = '#000';
                    
                    ctx.fillText(pct_msg, x, y);
                    ctx.restore();
                })
              
                Shiny.addCustomMessageHandler('image_loaded', function(msg){
                     var channel_image = document.getElementById('channel_image');
                     var canvas = document.getElementById('gate_canvas');
                

                     canvas.width = channel_image.naturalWidth;
                     canvas.height = channel_image.naturalHeight;
                     
                     
                     
                     
                
                      function getColorData(x, y, canvas) {
                          const red = y * (canvas.width * 4) + x * 4;
                          var colorIndices =  [red, red + 1, red + 2, red + 3];
                          
                          let imageData = canvas.getContext('2d').getImageData(0,0,canvas.width, canvas.height);
                          
                          const redIndex = colorIndices[0];
                          const greenIndex = colorIndices[1];
                          const blueIndex = colorIndices[2];

                          const red_px = imageData.data[redIndex];
                          const green_px = imageData.data[greenIndex];
                          const blue_px = imageData.data[blueIndex];
                          
                          return [red_px, green_px, blue_px]

                      }
                      

                    
                     var ctx = canvas.getContext('2d');
                     ctx.drawImage(channel_image,0,0);
                     

                     let tol = 5;
                     
                     // 7, 7, 15
                     // Ray casting to get the plotted axis limits
                     for (let i = 0; i < canvas.width; i++) {
                        let col = getColorData(i, 425, canvas);

                        let r = parseInt(col[0], 10);
                        let g = parseInt(col[1], 10);
                        let b = parseInt(col[2], 10);
                        
                        if( Math.abs(r-7) <= tol && Math.abs(g-7) <= tol && Math.abs(b-15) <= tol){
                           var left_axis = i;
                           break;
                        }
                     }
                     
                     for (let i = canvas.width-1; i >= 0; i--) {
                        let col = getColorData(i, 425, canvas);

                        let r = parseInt(col[0], 10);
                        let g = parseInt(col[1], 10);
                        let b = parseInt(col[2], 10);
                        
                        if( Math.abs(r-7) <= tol && Math.abs(g-7) <= tol && Math.abs(b-15) <= tol ){
                           var right_axis = i;
                           break;
                        }
                     }
                     
                     
                     
                     for (let i = 0; i < canvas.height; i++) {
                        let col = getColorData(425, i, canvas);

                        let r = parseInt(col[0], 10);
                        let g = parseInt(col[1], 10);
                        let b = parseInt(col[2], 10);
                        
                        if( Math.abs(r-7) <= tol && Math.abs(g-7) <= tol && Math.abs(b-15) <= tol ){
                           var top_axis = i;
                           break;
                        }
                     }
                     
                     for (let i = canvas.height-1; i >= 0; i--) {
                        let col = getColorData(425, i, canvas);

                        let r = parseInt(col[0], 10);
                        let g = parseInt(col[1], 10);
                        let b = parseInt(col[2], 10);
                        
                        if( Math.abs(r-7) <= tol && Math.abs(g-7) <= tol && Math.abs(b-15) <= tol ){
                           var bottom_axis = i;
                           break;
                        }
                     }
                     

                     // EVENTS
                     
                     
                     
                      function processEvent(evt) {
                          var rect = canvas.getBoundingClientRect();
                          var offsetTop = rect.top;
                          var offsetLeft = rect.left;
                      
                          if (evt.touches) {
                              return {
                                  x: evt.touches[0].clientX - offsetLeft,
                                  y: evt.touches[0].clientY - offsetTop
                              }
                          } else {
                              return {
                                  x: evt.clientX - offsetLeft,
                                  y: evt.clientY - offsetTop
                              }
                          }
                      }
                      
                      
                      function onDown(evt) {
                          evt.preventDefault();
                          var coords = processEvent(evt);
                          
                          // callback('down', coords.x, coords.y, evt.shiftKey);
                          
                          if( globalThis.plist != '' && globalThis.plist.length > 1){
                              let first_pt = globalThis.plist[0];
                              let dist = Math.sqrt(Math.pow(coords.x - first_pt.x,2) + Math.pow(coords.y - first_pt.y,2) );
  
                              if( dist < 10 ){
                                  coords.x = first_pt.x;
                                  coords.y = first_pt.y;
                                  
                                  globalThis.plist.push(coords);
                                  
                                  let poly = {
                                    'coords': globalThis.plist,
                                    'type': 'list',
                                    'bottom': bottom_axis,
                                    'top': top_axis,
                                    'left': left_axis,
                                    'right': right_axis
                                  };
                                  
                                  
                                  Shiny.setInputValue('polygon', poly);
                            }else{
                                globalThis.plist.push(coords);
                            }
                         }else{
                            globalThis.plist.push(coords);
                         }
                         render(canvas, globalThis.plist);

                     }
                      
                      canvas.ontouchstart = onDown;
                      canvas.onmousedown = onDown;
                      render(canvas, globalThis.plist);
                      Shiny.setInputValue('remove_spinner', Math.random());
                      
                })


                </script>")),

    
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

