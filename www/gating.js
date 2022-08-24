globalThis.plist = new Array();


var currentButton = '';
var currentMode  = 'none';
var isPolygonClosed = false;
var current_draw = 0;
var axis_bounds = [-1, -1, -1, -1];


function render(canvas, points) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      
      ctx.lineWidth = 3;
      for (let i = 0; i < points.length; i++) {
        ctx.beginPath();
        ctx.fillStyle = rgbToHex(0, 200, 0);
        ctx.arc(points[i].x, points[i].y, 3, 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fill();

        ctx.strokeStyle = rgbToHex(0, 200, 0);
        ctx.stroke();

        if( i >= 1){
            ctx.moveTo(points[i-1].x, points[i-1].y);
            ctx.lineTo(points[i].x, points[i].y);
            ctx.stroke();
        }
        
      }
      ctx.restore();
} // END OF render


function render_quadrant(canvas, center, bounds) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      
      ctx.lineWidth = 3;
      ctx.beginPath();
      ctx.fillStyle = rgbToHex(0, 200, 0);
      ctx.arc(center.x, center.y, 3, 0, Math.PI * 2, true);
      ctx.closePath();
      ctx.fill();

      ctx.strokeStyle = rgbToHex(0, 200, 0);
      ctx.stroke();
      

      for (let i = 0; i < bounds.length; i++) {
        ctx.beginPath();
        ctx.fillStyle = rgbToHex(0, 200, 0);
        ctx.arc(bounds[i], bounds[i].y, 3, 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fill();

        ctx.strokeStyle = rgbToHex(0, 200, 0);
        ctx.stroke();

        ctx.moveTo(bounds[i].x, bounds[i].y);
        ctx.lineTo(center.x, center.y);
        ctx.stroke();

        
      }
      ctx.restore();
} // END OF render_quadrant



function componentToHex(c) {
  var hex = c.toString(16);
  return hex.length == 1 ? '0' + hex : hex;
}

function rgbToHex(r, g, b) {
  return '#' + componentToHex(r) + componentToHex(g) + componentToHex(b);
}




// DEPRECATED: Remove later
$(document).on('shiny:value', function(event) {
  if (event.target.id === 'image_div') {
    if( parseInt(current_draw,10) === 0){
      current_draw = current_draw + 1
    }else{
      Shiny.setInputValue('pageLoaded', Math.random());
    }
    
    
  }
});

$(document).on('shiny:sessioninitialized', function(event) {
  Shiny.setInputValue('pageLoaded', Math.random());
});

$(document).on('shiny:connected', function(event) {
  Shiny.setInputValue('connected', Math.random());
});

function save_gate(){
 var canvas = document.getElementById('gate_canvas');
 
  Shiny.setInputValue('save', [canvas.toDataURL(), Math.random()]); // Ask server to save 
  hide_all_btn();
  
  document.getElementById('tool_label').innerHTML = "Manual Gating (View Mode)";
}


function select_transform( t_type){
  globalThis.plist = new Array();
  let tmpBtn = currentButton;
  select_button("");

  Shiny.setInputValue('transformSelected', t_type);

  select_button(tmpBtn);
  
  clear_poly();
}

function select_button(objId){
  if( currentButton != ''  ){
    currentMode = 'none';
    let btn = document.getElementById(currentButton);
    let div = btn.parentElement;
    btn.classList.remove('btn-active');
    div.classList.remove('btn-active');

    currentButton = '';
  }
  
  if( objId != ''){
    let btn = document.getElementById(objId);
    let div = btn.parentElement;
    
    div.style = "background-color: #FFFFFF;";
    
  
    btn.classList.add('btn-active');
    
    currentButton = objId;
    
    if( objId == 'polyDrawBtn'){
      currentMode = 'polyDraw';
    }
    
    if( objId == 'quadDrawBtn'){
      currentMode = 'quadDraw';
    }
    
    if( objId == 'lineDrawBtn'){
      currentMode = 'lineDraw';
    }
  }
    
}
  
 

// Client-side handling of clearing the polygon points
function clear_poly(){
  globalThis.plist = new Array();
  var channel_image = document.getElementById('channel_image');
  var canvas = document.getElementById('gate_canvas');
  
  canvas.width = channel_image.naturalWidth;
  canvas.height = channel_image.naturalHeight;
  
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.drawImage(channel_image,0,0);
  isPolygonClosed = false;
}

function hide_all_btn(){
  document.getElementById('saveBtn').style = 'visibility:hidden';
  document.getElementById('eraseBtn').style = 'visibility:hidden';
  document.getElementById('polyDrawBtn').style = 'visibility:hidden';
  document.getElementById('quadDrawBtn').style = 'visibility:hidden';
  document.getElementById('linearBtn').style = 'visibility:hidden';
  document.getElementById('biexpBtn').style = 'visibility:hidden';
  document.getElementById('logBtn').style = 'visibility:hidden';
}

Shiny.addCustomMessageHandler('setViewOnly', function(ignore){
  hide_all_btn();
  
  document.getElementById('tool_label').innerHTML = "Manual Gating (View Mode)";
})

Shiny.addCustomMessageHandler('set_data_mode', function(data_mode){

  if(data_mode == '1d'){
    document.getElementById('polyDrawBtn').style = 'visibility:hidden; display:none';
    document.getElementById('quadDrawBtn').style = 'visibility:hidden; display:none';
    document.getElementById('lineDrawBtn').style = 'visibility:visible; display:inline';
  }
})


// =========================================================
// Show selected % after the polygon is closed
// =========================================================
Shiny.addCustomMessageHandler('flag_info', function(flag_info){
  var channel_image = document.getElementById('channel_image');
  var canvas = document.getElementById('gate_canvas');
  
  canvas.width = channel_image.naturalWidth;
  canvas.height = channel_image.naturalHeight;
  
  
  var ctx = canvas.getContext('2d');
  ctx.drawImage(channel_image,0,0);
  
  if(currentMode == 'polyDraw'){
    render(canvas, globalThis.plist);
    isPolygonClosed = true; // No new points allowed
  }
  
  if(currentMode == 'quadDraw'){
    render_quadrant(canvas, globalThis.plist[0], globalThis.plist.slice(1,5) );
  }
  
  if(currentMode == 'lineDraw'){
    render_quadrant(canvas, globalThis.plist[0], globalThis.plist.slice(1,3) );
  }
  

  let pct = flag_info.pct;
  let x = flag_info.x;
  let y = flag_info.y;
  
  
  ctx.save();
  ctx.font = '20px Arial'
  
  ctx.textBaseline = 'top';


  for (let i = 0; i < pct.length; i++) {
    var text_width = ctx.measureText(pct[i]).width*1.1;
    ctx.fillStyle = 'rgba(190, 190, 190, 0.8)';
    ctx.fillRect(x[i]-5, y[i]-5, text_width, parseInt('24px Arial', 10));
    
    ctx.fillStyle = '#000';
    
    ctx.fillText(pct[i], x[i], y[i]);
    
  }
  


  ctx.restore();
  
  
})


Shiny.addCustomMessageHandler('axis_bounds', function(bounds){
  axis_bounds = bounds;
})

Shiny.addCustomMessageHandler('image_loaded', function(msg){
  var channel_image = document.getElementById('channel_image');
  var canvas = document.getElementById('gate_canvas');
  
  
  canvas.width = channel_image.naturalWidth;
  canvas.height = channel_image.naturalHeight;
  
  var ctx = canvas.getContext('2d');
  ctx.drawImage(channel_image,0,0);
  

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
    

    if( isPolygonClosed == false ){
      if(currentMode == 'polyDraw'){
        if( globalThis.plist != '' && globalThis.plist.length > 1){
            let first_pt = globalThis.plist[0];
            let dist = Math.sqrt(Math.pow(coords.x - first_pt.x,2) + Math.pow(coords.y - first_pt.y,2) );
            
            if( dist < 10 ){
              coords.x = first_pt.x;
              coords.y = first_pt.y;
              
              globalThis.plist.push(coords);
              /*
              ,
                'bottom': bottom_axis,
                'top': top_axis,
                'left': left_axis,
                'right': right_axis
              */
              let poly = {
                'coords': globalThis.plist,
                'type': 'poly'
              };
              
              
              Shiny.setInputValue('polygon', poly);
            }else{
              globalThis.plist.push(coords);
            }
          }else{
            globalThis.plist.push(coords);
          } // END of polygon drawing
          render(canvas, globalThis.plist);
        } // END of quadrant drawing
        
        // QUADRANT DRAWING
        if(currentMode == 'quadDraw'){
          globalThis.plist = new Array();
          globalThis.plist.push(coords);
          let mid_y = Math.abs(axis_bounds[2]-axis_bounds[3])/2;
          let mid_x = Math.abs(axis_bounds[1]-axis_bounds[0])/2;
          

          globalThis.plist.push( 
            {x: coords.x, y: axis_bounds[2]}
          );
            
          globalThis.plist.push( 
            {x: coords.x, y: axis_bounds[3]}
          );
            
          globalThis.plist.push( 
            {x: axis_bounds[0], y: coords.y}
          );
            
          globalThis.plist.push( 
            {x: axis_bounds[1], y: coords.y}
          );

          let poly = {
            'coords': globalThis.plist,
            'type': 'quadrant'
          };
              
              
          Shiny.setInputValue('polygon', poly);
          
          
          render_quadrant(canvas, coords, globalThis.plist.slice(1,5) );
        }
        
        if(currentMode == 'lineDraw'){
          globalThis.plist = new Array();
          globalThis.plist.push(coords);
          let mid_y = Math.abs(axis_bounds[2]-axis_bounds[3])/2;
          let mid_x = Math.abs(axis_bounds[1]-axis_bounds[0])/2;
          

          globalThis.plist.push( 
            {x: coords.x, y: axis_bounds[2]}
          );
            
          globalThis.plist.push( 
            {x: coords.x, y: axis_bounds[3]}
          );
            
          let poly = {
            'coords': globalThis.plist,
            'type': 'line'
          };
              
              
          Shiny.setInputValue('polygon', poly);
          
          
          render_quadrant(canvas, coords, globalThis.plist.slice(1,3) );
        }
      
    }
    
  }
  
  canvas.ontouchstart = onDown;
  canvas.onmousedown = onDown;
  //render(canvas, globalThis.plist);
  Shiny.setInputValue('remove_spinner', Math.random());
  
})