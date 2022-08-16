library(shiny)
library(tercen)
library(tercenApi)
library(tim)
library(dplyr)
library(tidyr)


library(ggplot2)
library("scattermore")
library(sp)
library(grid)

library(flowCore)
library(flowWorkspace)

library(jsonlite)
library(shinybusy)

library(base64enc)


# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8
options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
options("tercen.stepId"= "f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8")


source('plot_helpers.R')

server <- shinyServer(function(input, output, session) {
  
  df        <- reactiveValues( data=NULL, flag=NULL  )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  image <- reactiveValues( loaded=NULL, 
                           range_x=NULL, 
                           plot_lim_x=NULL,
                           range_y=NULL,
                           plot_lim_y=NULL)
  plot_mode <- reactiveValues( trans="linear" )
  
  
  output$image_div <- renderImage({
    query = parseQueryString(session$clientData$url_search)
    # op_mode <- query[["mode"]]
    op_mode <- 'run'
    if( op_mode == "run"){
      df$data <- get_data(session)
      ## Map densities to colors (VIRIDIS)
      cols <-  colorRampPalette(c("#440154", "#3b528b", "#21918c", 
                                  "#5ec962", "#fde725"))(256)

      lab_names <- names(df$data)
      
  
      
      if( plot_mode$trans == 'linear'){
        # Density
        x <- densCols(unlist(df$data[,1]), unlist(df$data[,2]), colramp=colorRampPalette(c("black", "white")),
                      nbin=256)
        dens <- col2rgb(x)[1,] + 1L

        colors <- cols[dens]
        
        
        
        
        xlim <- c(min(df$data[,1]), max(df$data[,1]*1.1))
        ylim <- c(min(df$data[,2]), max(df$data[,2]*1.1))
        # browser()
        # Adjust the plot ranges...
        #TODO - Check flag output
        # nearest_10k(xlim[1], label = FALSE)
        # xticks <- seq( 0, xlim[2], by=1e3)
        
        if( xlim[2] < 1.5e5){
          xticks <- seq( xlim[1], xlim[2], by=1e4 )
          fac<-3
        }else{
          xticks <- seq( xlim[1], xlim[2], by=5e4 )
          fac<-4
        }
        
        
        if( ylim[2] < 1.5e5){
          yticks <- seq( ylim[1], ylim[2], by=1e4 )
          fac<-3
        }else{
          yticks <- seq( ylim[1], ylim[2], by=5e4 )
          fac<-4
        }

        
        xtick_labels <- unlist(lapply(xticks, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
        ytick_labels <- unlist(lapply(yticks, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
        
        xticks <- unlist(lapply( xticks, function(x) nearest_factor10(x, FALSE, fac)))
        yticks <- unlist(lapply(yticks, function(x) nearest_factor10(x, label = FALSE, factor=fac)))
        
        imgfile <-  paste0(tempfile(), '.png') # '/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
        p <- ggplot() +
          geom_scattermost(
            as.matrix(df$data),
            pointsize=2,
            col=colors, 
            pixels=c(900,900)) +
          labs(x=lab_names[1], y=lab_names[2])  +
          scale_y_continuous(breaks = yticks, labels = ytick_labels) +
          scale_x_continuous(breaks = xticks, labels = xtick_labels) +
          theme(panel.background = element_rect(fill = 'white', colour = 'white'),
                axis.line.x=element_line(color="#07070F" ),
                axis.line.y=element_line(color="#07070F" ),
                panel.grid = element_line(color = "#AAAAAA",
                                          size = 0.1,
                                          linetype = 2),
                text = element_text(size=8)) + 
        annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
        annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
        
        
        # browser()
        pb <- ggplot_build(p)
        image$range_x <- pb$layout$panel_params[[1]]$x.range
        image$range_y <- pb$layout$panel_params[[1]]$y.range
        ggsave(imgfile, units='in', width=3, height=3, p ) 
        
      }else{
        # This is getting stuck...
        t_data <- df$data
        colnames(t_data)    <- c('.x','.y')
        
        custom_biexp_scale <- create_custom_biexp_scale(pos_decades = 5, 
                                                        neg_decades = -1.5, 
                                                        width_basis = -13)
        
        
        
        b_data <- t_data
        
        xt <- custom_biexp_scale$transform(x = t_data$.x  ) 
        xtn <- (xt - min(xt))/(max(xt)-min(xt))
        rd <- max(t_data$.x)-min(t_data$.x)
        breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd/5 )
        b_data[,1] <- (xtn * rd) + min(t_data$.x) 
        
        yt <- custom_biexp_scale$transform(x = t_data$.y  ) 
        ytn <- (yt - min(yt))/(max(yt)-min(yt))
        rd <- max(t_data$.y)-min(t_data$.y)
        breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd/5 )
        b_data[,2] <- (ytn * rd)+min(t_data$.y) 
        
        
        # Density
        x <- densCols(unlist(b_data[,1]), unlist(b_data[,2]), colramp=colorRampPalette(c("black", "white")),
                      nbin=256 )
        dens <- col2rgb(x)[1,] + 1L
        

        colors <- cols[dens]
        
        # "-10, 0, 1e1, 1e2, 1e3, 1e4"),
         # breaks.x <- c(0,  1e1, 1e2, 1e3, 1e4, 1e5)
        # breaks.y <- c(-1e2,  1e1, 1e2, 1e3, 1e4)
        breaks.x <-  break_transform(breaks = breaks.x, 
                                     transformation = "biexponential")
        breaks.y <-  break_transform(breaks = breaks.y, 
                                     transformation = "biexponential")
        
        #paste0(tempfile(), '.jpeg') #
        imgfile <-  paste0(tempfile(), '.png')
        
        p <- ggplot() +
          scale_x_continuous(limits = c( min(b_data$.x), max(b_data$.x) ),
                             breaks = breaks.x,
                             trans = custom_biexp_scale,
                             labels = custom_tick_labels(breaks.x),
                             sec.axis = dup_axis(labels=c())) +
          scale_y_continuous(limits = c( min(b_data$.y), max(b_data$.y) ),
                             breaks = breaks.y,
                             trans = custom_biexp_scale,
                             labels = custom_tick_labels(breaks.y),
                             sec.axis = dup_axis(labels = c())) +
          geom_scattermore(
            data=t_data,
            mapping=aes(x=.x, y=.y),
            pointsize=2,
            col=colors,
            pixels=c(900,900)) +
          labs(x=lab_names[1], y=lab_names[2])  +
          theme(panel.background = element_rect(fill = 'white', colour = 'white'),
                axis.line.x=element_line(color="#07070F" ),
                axis.line.y=element_line(color="#07070F" ),
                text = element_text(size=8),
                axis.ticks.x.top = element_blank(),
                axis.title.x.top = element_blank(),
                axis.ticks.y.right = element_blank(),
                axis.title.y.right = element_blank()) 
          
          # annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = 0, xend = 0, size = 1) 
          # annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
        
        
        p<- set_biexp_ticks(p, breaks.x) 
        # p
        # browser()
        
        pb <- ggplot_build(p)
        
        
        xt <- custom_biexp_scale$transform(x = pb$layout$panel_params[[1]]$x.range  ) 
        xtn <- (xt - min(xt))/(max(xt)-min(xt))
        rd <- max(b_data$.x)-min(b_data$.x)
        xr <- (xtn * rd) + min(b_data$.x) 
        
        xt <- custom_biexp_scale$transform(x = pb$layout$panel_params[[1]]$y.range  ) 
        xtn <- (xt - min(xt))/(max(xt)-min(xt))
        rd <- max(b_data$.y)-min(b_data$.y)
        yr <- (xtn * rd) + min(b_data$.y) 
        
        image$range_x <- xr
        image$range_y <- yr
        
        

        
        ggsave(imgfile, units='in', width=3, height=3, p ) 
      }
      
      
      
      
      
      
     }else{
    #   ctx <- getCtx(session)
    #   fout <- paste0('/tmp/', ctx$workflowId, '_', ctx$stepId, '.png')
    #   
    #   if( file.exists(fout) ){
    #     imgfile <- fout
        session$sendCustomMessage("setViewOnly", runif(1))
      # }
    }
    
    
    library('png')

    img <- readPNG(imgfile)
    # 7, 7, 15
    
    mark_pix <- c(7/255.0, 7/255.0, 15/255.0)
    # abs((x - mark_pix[1])) < 0.05
    pixels <- img[450, ,]
    redPx <- unlist(lapply(pixels[,1], function(x){
      return(  abs((x - mark_pix[1])) < 0.01 )
    }))
    greenPx <- unlist(lapply(pixels[,2], function(x){
      return(  abs((x - mark_pix[2])) < 0.01 )
    }))
    bluePx <- unlist(lapply(pixels[,3], function(x){
      return(  abs((x - mark_pix[3])) < 0.01 )
    }))
    
    
    marks.x <- which((redPx + greenPx + bluePx) == 3)
    
    
    k <- 1
    x.plot.lim <- c(marks.x[k])

    prev_marks <- marks.x[k]
    while( k <= length(marks.x) && (marks.x[k] - prev_marks)<=1 ){
      x.plot.lim[1] <- marks.x[k]
      prev_marks <- marks.x[k]
      k <- k + 1
    }
    k <- k + 1
    x.plot.lim[2] <- marks.x[k]
    
    pixels <- img[,450 ,]
    redPx <- unlist(lapply(pixels[,1], function(x){
      return(  abs((x - mark_pix[1])) < 0.01 )
    }))
    greenPx <- unlist(lapply(pixels[,2], function(x){
      return(  abs((x - mark_pix[2])) < 0.01 )
    }))
    bluePx <- unlist(lapply(pixels[,3], function(x){
      return(  abs((x - mark_pix[3])) < 0.01 )
    }))
    
    
    marks.y <- which((redPx + greenPx + bluePx) == 3)
    
    k <- 1
    y.plot.lim <- c(marks.y[k])
    prev_marks <- marks.y[k]
    while( k <= length(marks.y) && (marks.y[k] - prev_marks)<=1 ){
      y.plot.lim <- marks.y[k]
      prev_marks <- marks.y[k]
      k <- k + 1
    }
    k <- k + 1
    y.plot.lim[2] <- marks.y[k]
    # browser()
    image$plot_lim_x <- x.plot.lim
    image$plot_lim_y <- y.plot.lim
    
    image$loaded <- runif(1)
    
 
    
    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/png',
         alt = "Scatter plot failed to load.")
  }, deleteFile = FALSE)
  
  
  observeEvent( input$polygon, {
    # FIXED for 900 x 900 image
    # TODO Find a  quick and smart way to collect these values
    # axis.limits <- c(185, 765, 876, 100)
    axis.limits <- c(image$plot_lim_x[1], 
                     image$plot_lim_y[2],
                     image$plot_lim_x[2],
                     image$plot_lim_y[1])
    
    
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))

    
    
    coords.type <- input$polygon$type # For later use
    
    # axis.limits <- c(input$polygon$left, 
    #                 input$polygon$bottom,
    #                 input$polygon$right,
    #                 input$polygon$top) 
    
    point_cloud <- df$data
    range.x <- max(image$range_x) - min(image$range_x) #max( max(point_cloud[,1]*1.1 ) + 0 )
    range.y <- max(image$range_y) - min(image$range_y) #max( max(point_cloud[,2]*1.1 ) + 0 )
    
    im_rx <- image$range_x
    im_ry <- image$range_y
    
    save('axis.limits', 'coords.x', 'coords.y', 'point_cloud',
         'range.x', 'range.y', 'im_rx', 'im_ry',
         file='test.Rda')
    if( plot_mode$trans == 'biexp' ){
      t_data <- df$data
      colnames(t_data)    <- c('.x','.y')
      
      custom_biexp_scale <- create_custom_biexp_scale(pos_decades = 5, 
                                                      neg_decades = -1.5, 
                                                      width_basis = -13)
      
      
      
      b_data <- t_data
      
      xt <- custom_biexp_scale$transform(x = t_data$.x  ) 
      xtn <- (xt - min(xt))/(max(xt)-min(xt))
      rd <- max(t_data$.x)-min(t_data$.x)
      breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd/5 )
      b_data$.x <- (xtn * rd) + min(t_data$.x) 
      

      yt <- custom_biexp_scale$transform(x = t_data$.y  ) 
      ytn <- (yt - min(yt))/(max(yt)-min(yt))
      rd <- max(t_data$.y)-min(t_data$.y)
      breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd/5 )
      b_data[,2] <- (ytn * rd)+min(t_data$.y) 
      
      
      point_cloud <- b_data
    }
    # browser()
    # image$range_x

    # save('point_cloud', 'coords.x', 'coords.y', 'axis.limits', file="test.Rda")
    # Mapping from pixels to data space
    # These values must match the image output!
    
    
    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    # 0.06 -> default margin
    
    coords.x <- (coords.x- axis.limits[1])/range.plot.x + 0.06/2
    coords.y <- (1-0.06) - ((coords.y - axis.limits[4])/range.plot.y) + 0.06/2
    
    
    # OldRange = (OldMax - OldMin)  
    # NewRange = (NewMax - NewMin)  
    # NewValue = (((OldValue - OldMin) * NewRange) / OldRange) + NewMin
    NewValueX = (((coords.x - 0.06/2) * range.x) / 1) + min(image$range_x )
    NewValueY = (((coords.y - 0) * range.y) / 1) + min(image$range_y)
    poly_df <- data.frame("x"=(((coords.x - 0.06/2) * range.x) / 1) + min(image$range_x ),
                          "y"=(((coords.y - 0) * range.y) / 1) + min(image$range_y) )
    # poly_df <- data.frame("x"=(coords.x * range.x), "y"=(coords.y ) * range.y )

    # browser()
    fac_norm <-max( point_cloud[,1] )
    # xr <- c(min(point_cloud[,1]),max(point_cloud[,1]) )
    # yr <- c(min(point_cloud[,2]),max(point_cloud[,2]) )
    
    # point_cloud[,1] <- point_cloud[,1]/max( point_cloud[,1] )
    # point_cloud[,2] <- point_cloud[,2]/max( point_cloud[,2] )

    names(point_cloud) <- c('x', 'y')
    point_cloud <- point_cloud %>%
        mutate(flag = point.in.polygon( .$x, .$y, poly_df$x, poly_df$y, mode.checked = FALSE ))
    
    # browser()

    selected$pct <- 100*sum(point_cloud$flag == 1) / length(point_cloud$flag)
    selected$x <- mean( (coords.x )    * range.x )
    selected$y <- mean( (coords.y )    * range.y )
    selected$flag <- point_cloud$flag

    # df$flag <- point_cloud$flag
    session$sendCustomMessage("pct_selected",  paste0( format(selected$pct, digits = 3, scientific = FALSE), '%') )
  })
  
  observeEvent( input$pageLoaded, {
    session$sendCustomMessage("image_loaded", "Image has been loaded")
  })
  
  observeEvent( input$connected, {
    show_modal_spinner(spin="fading-circle", text = "Loading")
  })
  
  observeEvent( input$remove_spinner, {
    remove_modal_spinner()
  })
  
  session$onFlushed(function() {
    # session$sendCustomMessage("image_loaded", "Image has been loaded")
  })
  
  observeEvent( input$clearBtn, {
    session$sendCustomMessage("clear_poly", "Clear polygon")
  })
  
  observeEvent( input$transformSelected, {
    show_modal_spinner(spin="fading-circle", text = "Updating")
    plot_mode$trans <- input$transformSelected
    
  })
  
  observeEvent( input$save, {
    ctx <- getCtx(session)
    # Check the barplot operator --> Do the result plot
    show_modal_spinner(spin="fading-circle", text = "Saving")
    # SAVE this as specific file to be read if needed... 

    fout <- paste0( tempfile(), ".png") #paste0('/tmp/', ctx$workflowId, '_', ctx$stepId, '.png')
    raw <- base64enc::base64decode(what = substr(input$save[1], 23, nchar(input$save[1])))
    png::writePNG(png::readPNG(raw), fout)    

    img_df <- tim::png_to_df(fout, filename = "Gate.png")

    img_df$mimetype <- 'image/png'
    img_df$filename <- "Gate.png"


    img_df <- img_df %>%
      ctx$addNamespace() %>%
      as_relation() %>%
      as_join_operator(list(), list()) 
    
    ctx <- getCtx(session)
    
    xname <- tercen::remove.prefix( ctx$xAxis[[1]] )
    yname <- tercen::remove.prefix( ctx$yAxis[[1]] )

    flagDf <- df$data %>%
      cbind(.,data.frame("flag"=as.numeric(selected$flag))) %>%
      select(flag) %>%
      mutate(!! xname:=unlist(unname(df$data[,1])) ) %>%
      mutate(!! yname:=unlist(unname(df$data[,2])) ) %>%
      ctx$addNamespace() %>%
      as_relation() %>%
      as_join_operator(list(), list())
      
    
    # polyDf <-data.frame('x'=coords.x, 'y'=coords.y, '.ci'=as.integer(0*seq(1,length(coords.x))) ) %>%
      # ctx$addNamespace()
    # browser()
    # ctx$save(list(flagDf,img_df))
    save_relation(list(flagDf,img_df), ctx)
    
    remove_modal_spinner()
  })
      
  
})


get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Data")
  
  show_modal_spinner(spin="fading-circle", text = "Loading")
  
  df <- ctx %>%
    select(.x, .y)
  
  names(df) <- c(ctx$xAxis[[1]], ctx$yAxis[[1]])
  
  progress$close()
  # remove_modal_spinner()
  
  return(df)
  
}


getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}

nearest_factor10 <- function( num, label=TRUE, factor=4){
  # -67.5  4932.5  9932.5 14932.5 19932.5 24932.5 29932.5 34932.5 39932.5 44932.5 49932.5 54932.5 59932.5 64932.5 69932.5 74932.5 79932.5 84932.5
  # [19] 89932.5
  
  neg_fac <- 1
  if(num<0){
    neg_fac <- -1
  }
  num <- abs(num)
  
  if( num < 10^factor ){
    if(label == FALSE){
      return( neg_fac*(10^factor) )
    }else{
      num<-10^factor
      round_num <- paste0(
        format( neg_fac * ( num - num %% 10^factor )/1e3, scientific=FALSE),
        'K')
      return( round_num )
    }
  }
  
  if(label == FALSE){
    return(neg_fac * ( num - num %% 10^factor ) )
  }
  round_num <- paste0(
    format( neg_fac * ( num - num %% 10^factor )/1e3, scientific=FALSE),
    'K')
  
  
  return( round_num )
}


############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################