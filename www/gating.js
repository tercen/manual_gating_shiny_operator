globalThis.plist = new Array();


var currentButton = new Array();
var currentGroup = new Array();


var isSaveEnabled = false;

var currentMode  = 'none';
var isPolygonClosed = false;
var current_draw = 0;
var axis_bounds = [-1, -1, -1, -1];
var is_dragging = false;
var drag_idx = -1;

// =====================================================
// Render gate functions
// =====================================================
function add_poly_dot(ctx, coord){
  ctx.beginPath();
  ctx.fillStyle = rgbToHex(0, 0, 0);
  ctx.arc(coord.x, coord.y, 5, 0, Math.PI * 2, true);
  ctx.closePath();
  ctx.fill();
  
  ctx.beginPath();
  ctx.fillStyle = rgbToHex(255, 255, 255);
  ctx.arc(coord.x, coord.y, 3, 0, Math.PI * 2, true);
  ctx.closePath();
  ctx.fill();
  ctx.restore();
}

function add_poly_line(ctx, coord_from, coord_to){
  ctx.lineWidth = 4;
  ctx.strokeStyle = rgbToHex(0, 0, 0);
  
  ctx.moveTo(coord_from.x, coord_from.y);
  ctx.lineTo(coord_to.x, coord_to.y);
  ctx.stroke();
  
  ctx.lineWidth = 1;
  ctx.strokeStyle = rgbToHex(255, 255, 255);
  
  ctx.moveTo(coord_from.x, coord_from.y);
  ctx.lineTo(coord_to.x, coord_to.y);
  ctx.stroke();
  
  ctx.restore();
}

function render(canvas, points) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      
      for (let i = 0; i < points.length; i++) {
        if( i >= 1){
            add_poly_line(ctx, points[i-1], points[i]);
        }
      }
      for (let i = 0; i < points.length; i++) {
        add_poly_dot(ctx, points[i]);
        
      }
      ctx.restore();
} // END OF render

function render_ellipsoid(canvas, points) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      
      let center = points[0];
      let radiusX = Math.sqrt( Math.pow((points[2].y - points[0].y),2) + Math.pow((points[2].x - points[0].x),2) ) ;
      let radiusY = Math.sqrt( Math.pow((points[1].y - points[0].y),2) + Math.pow((points[1].x - points[0].x),2) ) ;
      
      //  Math.sqrt( Math.pow((points[2].y - points[0].y),2) + Math.pow((points[2].x - points[0].x),2) ) ;
      let dy = points[2].y - points[0].y;
      let dx = points[2].x - points[0].x;
      let rotation = Math.atan2(dy, dx); // range (-PI, PI]


      ctx.lineWidth = 5;
      ctx.strokeStyle = rgbToHex(0, 0, 0);
      ctx.ellipse(center.x, center.y, radiusX, radiusY, rotation, 0, Math.PI * 2);
      ctx.stroke();
      
      ctx.lineWidth = 3;
      ctx.strokeStyle = rgbToHex(255, 255, 255);
      ctx.ellipse(center.x, center.y, radiusX, radiusY, rotation, 0, Math.PI * 2);
      ctx.stroke();
      
      
      // Center dot
      add_poly_dot(ctx, center);

      ctx.lineWidth = 3;
      for (let i = 1; i <= 2; i++) {
        add_poly_dot(ctx, points[i]);
      }
      
      
      ctx.restore();
} // END OF render_ellipsoid


function render_quadrant(canvas, center, bounds) {
      let ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(channel_image,0,0)
      ctx.save();
      
      for (let i = 0; i < bounds.length; i++) {
        add_poly_line(ctx, bounds[i], center);
      }

      for (let i = 0; i < bounds.length; i++) {
        add_poly_dot( ctx, bounds[i] );  
      }
      
      add_poly_dot( ctx, center );
      
      ctx.restore();
} // END OF render_quadrant



function componentToHex(c) {
  var hex = c.toString(16);
  return hex.length == 1 ? '0' + hex : hex;
}

function rgbToHex(r, g, b) {
  return '#' + componentToHex(r) + componentToHex(g) + componentToHex(b);
}

// -----------------------------------------------------
// END OF Render gate functions
// -----------------------------------------------------


// Control events to remove the 'Loading' modal
$(document).on('shiny:sessioninitialized', function(event) {
  Shiny.setInputValue('pageLoaded', Math.random());
});

$(document).on('shiny:connected', function(event) {
  Shiny.setInputValue('connected', Math.random());
});



function save_gate(){
  if( isSaveEnabled == false ){
    return
  }
  
  var canvas = document.getElementById('gate_canvas');
  hide_all_btn();
  Shiny.setInputValue('save', canvas.toDataURL()); // Ask server to save 
}


function select_button(objId){
  clear_poly();

  if( objId != -1){
    let btn = document.getElementById(objId);
    let div = btn.parentElement;
    let grp = btn.getAttribute("data-group");

    if( currentButton.length > 0 ){
      
      let oldBtnId = get_active_button_id( grp );

      if( typeof oldBtnId == "string"  ){

        let oldBtn = document.getElementById(oldBtnId);
        let oldDiv = oldBtn.parentElement;

        oldBtn.classList.remove('btn-active');
        oldDiv.classList.remove('btn-active');
      }
      

    }
    
    update_active_button_list(objId, grp);

    btn.classList.add('btn-active');
    
    if( objId == 'polyDrawBtn'){
      currentMode = 'polyDraw';
    }
    
    if( objId == 'quadDrawBtn'){
      currentMode = 'quadDraw';
    }
    
    if( objId == 'circDrawBtn'){
      currentMode = 'circDraw';
    }
    
    if( objId == 'lineDrawBtn'){
      currentMode = 'lineDraw';
    }
  }
}

function get_active_button_id( btnGrp ){
  if( currentGroup.length == 0){
    return -1;
  }else{
    for( let i = 0; i < currentGroup.length; i++ ){
      if( currentGroup[i] === btnGrp ){
        return currentButton[i];
      }
    }
    
    return -1;
  }
}

function update_active_button_list( btnId, btnGrp ) {
  if( currentGroup.length == 0){
    currentButton.push(btnId);
    currentGroup.push(btnGrp);
  }else{
    let grp_idx = -1;
    for( let i = 0; i < currentGroup.length; i++ ){
      if( currentGroup[i] === btnGrp ){
        grp_idx = i;
        break;
      }
    }
    if( grp_idx >= 0 ){
      currentButton[grp_idx] = btnId;
      currentGroup[grp_idx] = btnGrp;
    }else{
      currentButton.push(btnId);
      currentGroup.push(btnGrp); 
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
  
  enable_save( false );
}

function hide_all_btn(){
  document.getElementById('saveBtn').style = 'visibility:hidden; display: none;';
  document.getElementById('eraseBtn').style = 'visibility:hidden; display: none;';
  document.getElementById('polyDrawBtn').style = 'visibility:hidden; display: none;';
  document.getElementById('quadDrawBtn').style = 'visibility:hidden; display: none;';
  document.getElementById('circDrawBtn').style = 'visibility:hidden; display: none;';
  document.getElementById('lineDrawBtn').style = 'visibility:hidden; display: none;';
}


function enable_save( enb ){
  if( enb === true){
    document.getElementById("saveDisabledDiv").style = 'opacity:100%;';  
    isSaveEnabled = true;
  }else{
    document.getElementById("saveDisabledDiv").style = 'opacity:25%;';  
    isSaveEnabled = false;
  }
  
}

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
  
  if(currentMode == 'circDraw'){
    render_ellipsoid(canvas, globalThis.plist );
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

Shiny.addCustomMessageHandler('setViewOnly', function(ignore){
  hide_all_btn();
  
  document.getElementById('tool_label').innerHTML = "Manual Gating (View Mode)";
})

Shiny.addCustomMessageHandler('set_data_mode', function(data_mode){
  if(data_mode == '1d'){
    document.getElementById('polyDrawBtn').style = 'visibility:hidden; display:none';
    document.getElementById('quadDrawBtn').style = 'visibility:hidden; display:none';
    document.getElementById('circDrawBtn').style = 'visibility:hidden; display:none';
    document.getElementById('lineDrawBtn').style = 'visibility:visible; display:inline';
  }
})

Shiny.addCustomMessageHandler('axis_bounds', function(bounds){
  axis_bounds = bounds;
  
  document.getElementById("connectGif").style = 'visibility:hidden';
  
  document.getElementById('saveBtnImg').style = 'visibility:visible';
  document.getElementById('eraseBtnImg').style = 'visibility:visible';
  document.getElementById('polyDrawBtnImg').style = 'visibility:visible';
  document.getElementById('quadDrawBtnImg').style = 'visibility:visible';
  document.getElementById('circDrawBtnImg').style = 'visibility:visible';
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
    
    is_dragging = false;
    drag_idx = -1;
    
    for (let i = 0; i < globalThis.plist.length; i++) {
      let dist = Math.sqrt(Math.pow(coords.x - globalThis.plist[i].x,2) + Math.pow(coords.y - globalThis.plist[i].y,2) );
      let is_closing_poly = currentMode == 'polyDraw' && i == 0;
      if( dist < 10 && !is_closing_poly){
        is_dragging = true;
        drag_idx = i;
        break;
      }
    }
    


    if( isPolygonClosed == false && is_dragging ==  false){ // Add new points
      if(currentMode == 'polyDraw'){
        if( globalThis.plist != '' && globalThis.plist.length > 1){
            let first_pt = globalThis.plist[0];
            let dist = Math.sqrt(Math.pow(coords.x - first_pt.x,2) + Math.pow(coords.y - first_pt.y,2) );
            
            if( dist < 10 ){
              coords.x = first_pt.x;
              coords.y = first_pt.y;
              globalThis.plist.push(coords);

              let poly = {
                'coords': globalThis.plist,
                'type': 'poly'
              };
              
              
              Shiny.setInputValue('polygon', poly);
              enable_save( true );
            }else{
              globalThis.plist.push(coords);
            }
          }else{
            globalThis.plist.push(coords);
          } 
          render(canvas, globalThis.plist);
        } // END of polygon drawing 
        
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
          enable_save( true );
          
          render_quadrant(canvas, coords, globalThis.plist.slice(1,5) );
        } // END of quadrant drawing
        
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
          enable_save( true );
          
          render_quadrant(canvas, coords, globalThis.plist.slice(1,3) );
        } // END if lineDraw
        
        if(currentMode == 'circDraw'){
          isPolygonClosed = true;
          globalThis.plist = new Array();
          globalThis.plist.push(coords);
          
          let canvas = document.getElementById('gate_canvas');
          

          globalThis.plist.push( 
            {x: coords.x, y: coords.y - (canvas.height*0.05)}
          );
            
          globalThis.plist.push( 
            {x: coords.x + (canvas.width*0.075), y: coords.y}
          );
            
          let poly = {
            'coords': globalThis.plist,
            'type': 'ellipsoid'
          };
              
          Shiny.setInputValue('polygon', poly);
          enable_save( true );

          render_ellipsoid(canvas, globalThis.plist );
        } // END if circDraw
      
    }
    
  }
  
  function onDrag(evt){
    if( is_dragging == true ){
      var coords = processEvent(evt);
      if(currentMode == 'polyDraw'){
        if( drag_idx == globalThis.plist.length-1){
          // Move first and last
          globalThis.plist[0] = coords;
        }
        globalThis.plist[drag_idx] = coords;
        render(canvas, globalThis.plist);
      }
      if(currentMode == 'circDraw'){
        if(drag_idx == 0){
          // Center point, drag the full polygon
          let old_center = globalThis.plist[0];
          let dx = coords.x - old_center.x;
          let dy = coords.y - old_center.y;
          
          globalThis.plist[drag_idx] = coords;
          globalThis.plist[1].x += dx;
          globalThis.plist[1].y += dy;
          globalThis.plist[2].x += dx;
          globalThis.plist[2].y += dy;
        }else {
          let dy =  globalThis.plist[drag_idx].y - globalThis.plist[0].y;
          let dx =  globalThis.plist[drag_idx].x - globalThis.plist[0].x;
          let old_rotation = Math.atan2(dy, dx); // range (-PI, PI]
          
          dy =  coords.y - globalThis.plist[0].y;
          dx =  coords.x - globalThis.plist[0].x;
          let new_rotation = Math.atan2(dy, dx); // range (-PI, PI]
          
          let rotation = (old_rotation - new_rotation)/1;
          let cos = Math.cos(rotation);
          let sin = Math.sin(rotation);
          
          let cx = globalThis.plist[0].x;   
          let cy = globalThis.plist[0].y;   
          if(drag_idx == 1){
            let el = globalThis.plist[2];
        
            globalThis.plist[2].x = (cos * (el.x - cx)) + (sin * (el.y - cy)) + cx;
            globalThis.plist[2].y = (cos * (el.y - cy)) - (sin * (el.x - cx)) + cy;
          }else{
            let el = globalThis.plist[1];
            globalThis.plist[1].x = (cos * (el.x - cx)) + (sin * (el.y - cy)) + cx;
            globalThis.plist[1].y = (cos * (el.y - cy)) - (sin * (el.x - cx)) + cy;
          }
          
          globalThis.plist[drag_idx] = coords;

        }
        
        render_ellipsoid(canvas, globalThis.plist );
      }
      
      
      if(currentMode == 'quadDraw'){
        // 1,2 y-axis only
        if( drag_idx == 1 || drag_idx == 2){
          globalThis.plist[drag_idx].x = coords.x;  
        }else if( drag_idx == 3 || drag_idx == 4){
          globalThis.plist[drag_idx].y = coords.y;  
        } else{
          globalThis.plist[drag_idx] = coords;  
        }
        
        
        render_quadrant(canvas, globalThis.plist[0], globalThis.plist.slice(1,5) );
      }

    }
    
  }
  
  function onUp(evt) {
    is_dragging = false;
    if(currentMode == 'polyDraw' && isPolygonClosed == true){
      let poly = {
        'coords': globalThis.plist,
        'type': 'poly'
      };
      
      
      Shiny.setInputValue('polygon', poly);
    }
    
    if(currentMode == 'circDraw'){
      let poly = {
        'coords': globalThis.plist,
        'type': 'ellipsoid'
      };
      Shiny.setInputValue('polygon', poly);
    }
    
    if(currentMode == 'quadDraw'){
      let poly = {
        'coords': globalThis.plist,
        'type': 'quadrant'
      };
      Shiny.setInputValue('polygon', poly);
    }
  }
  
  
  canvas.ontouchstart = onDown;
  canvas.onmousedown = onDown;
  canvas.onmouseup = onUp;
  canvas.onmousemove = onDrag;
  //render(canvas, globalThis.plist);
  Shiny.setInputValue('remove_spinner', Math.random());
  
})

