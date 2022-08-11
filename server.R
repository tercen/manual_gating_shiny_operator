library(shiny)
library(tercen)
library(tercenApi)
library(dplyr)
library(tidyr)


library(ggplot2)
library("scattermore")
library(sp)

library(flowCore)
library(flowWorkspace)

library(jsonlite)
library(shinybusy)

library(base64enc)


# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8")


source('plot_helpers.R')

server <- shinyServer(function(input, output, session) {
  df        <- reactiveValues( data=NULL, flag=NULL  )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  image <- reactiveValues( loaded=NULL  )
  plot_mode <- reactiveValues( trans="linear" )


  
  output$image_div <- renderImage({
    query = parseQueryString(session$clientData$url_search)
    # op_mode <- query[["mode"]]
    op_mode <- 'run'
    if( op_mode == "run"){
      df$data <- get_data(session)
      
      # Density
      x <- densCols(unlist(df$data[,1]), unlist(df$data[,2]), colramp=colorRampPalette(c("black", "white")))
      dens <- col2rgb(x)[1,] + 1L
      
      ## Map densities to colors (VIRIDIS)
      cols <-  colorRampPalette(c("#440154", "#3b528b", "#21918c", 
                                  "#5ec962", "#fde725"))(256)
      colors <- cols[dens]
      lab_names <- names(df$data)
      
      xlim <- c(0, max(df$data[,1]*1.1))
      ylim <- c(0, max(df$data[,2]*1.1))
      
      
      xticks <- seq( xlim[1], xlim[2], by=5e4 )
      yticks <- seq( ylim[1], ylim[2], by=5e4 )
      
      xtick_labels <- unlist(lapply(xticks, function(x) nearest_100k(x, label = TRUE)))
      ytick_labels <- unlist(lapply(yticks, function(x) nearest_100k(x, label = TRUE)))
      
      xticks <- unlist(lapply(xticks, function(x) nearest_100k(x, label = FALSE)))
      yticks <- unlist(lapply(yticks, function(x) nearest_100k(x, label = FALSE)))
      
      
      
      if( plot_mode$trans == 'linear'){
        imgfile <-  paste0(tempfile(), '.jpeg') # '/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
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
                axis.line.y=element_line(color="#07070F" )) #+ 
        # annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
        # annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
        
        
        
        ggsave(imgfile, units='in', width=3, height=3, p ) 
        
      }else{
        # This is getting stuck...
        t_data <- df$data
        colnames(t_data)    <- c('.x','.y')
        
        custom_biexp_scale <- create_custom_biexp_scale(pos_decades = 4.5, 
                                                        neg_decades = 0, 
                                                        width_basis = -13)
        
        
        
        b_data <- t_data
        
        xt <- custom_biexp_scale$transform(x = t_data$.x  ) 
        xtn <- (xt - min(xt))/(max(xt)-min(xt))
        rd <- max(t_data$.x)-min(t_data$.x)
        breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd/4.5 )
        b_data$.x <- (xtn * rd) + min(t_data$.x) 
        
        yt <- custom_biexp_scale$transform(x = t_data$.y  ) 
        ytn <- (yt - min(yt))/(max(yt)-min(yt))
        rd <- max(t_data$.y)-min(t_data$.y)
        breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd/4.5 )
        b_data[,2] <- (ytn * rd)+min(t_data$.y) 
        
        
        breaks.x <-  break_transform(breaks = breaks.x, 
                                     transformation = "biexponential")
        breaks.y <-  break_transform(breaks = breaks.y, 
                                     transformation = "biexponential")
        
        #paste0(tempfile(), '.jpeg') #
        imgfile <-  paste0(tempfile(), '.jpeg')
        
        p <- ggplot() +
          scale_x_continuous(limits = c( min(b_data$.x), max(b_data$.x) ),
                             breaks = breaks.x,
                             trans = custom_biexp_scale,
                             labels = custom_tick_labels(breaks.x)) +
          scale_y_continuous(limits = c( min(b_data$.y), max(b_data$.y) ),
                             breaks = breaks.y,
                             trans = custom_biexp_scale,
                             labels = custom_tick_labels(breaks.y)) +
          geom_scattermore(
            data=t_data,
            mapping=aes(x=.x, y=.y),
            pointsize=2,
            col=colors,
            pixels=c(900,900)) +
          labs(x=lab_names[1], y=lab_names[2])  +
          theme(panel.background = element_rect(fill = 'white', colour = 'white'),
                axis.line.x=element_line(color="#07070F" ),
                axis.line.y=element_line(color="#07070F" )) #+ 
        # annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
        # annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
        
        
        
        ggsave(imgfile, units='in', width=3, height=3, p ) 
      }
      
      
    }else{
      ctx <- getCtx(session)
      fout <- paste0('/tmp/', ctx$workflowId, '_', ctx$stepId, '.png')
      
      if( file.exists(fout) ){
        imgfile <- fout
        session$sendCustomMessage("setViewOnly", runif(1))
      }
    }
    
    
    image$loaded <- runif(1)
    
 
    
    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/jpeg',
         alt = "Scatter plot failed to load.")
  }, deleteFile = FALSE)
  
  
  observeEvent( input$polygon, {
    # FIXED for 900 x 900 image
    # TODO Find a  quick and smart way to collect these values
    axis.limits <- c(185, 765, 876, 100)
    
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))

    coords.type <- input$polygon$type # For later use
    
    # axis.limits <- c(input$polygon$left, 
    #                 input$polygon$bottom,
    #                 input$polygon$right,
    #                 input$polygon$top) 
    
    point_cloud <- df$data
    
    if( plot_mode$trans == 'biexp' ){
      t_data <- df$data
      colnames(t_data)    <- c('.x','.y')
      
      custom_biexp_scale <- create_custom_biexp_scale(pos_decades = 4.5, 
                                                      neg_decades = 0, 
                                                      width_basis = -13)
      
      b_data <- t_data
      
      xt <- custom_biexp_scale$transform(x = t_data$.x  ) 
      xtn <- (xt - min(xt))/(max(xt)-min(xt))
      rd <- max(t_data$.x)-min(t_data$.x)
      breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd/4.5 )
      b_data$.x <- (xtn * rd) + min(t_data$.x) 
      
      yt <- custom_biexp_scale$transform(x = t_data$.y  ) 
      ytn <- (yt - min(yt))/(max(yt)-min(yt))
      rd <- max(t_data$.y)-min(t_data$.y)
      breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd/4.5 )
      b_data[,2] <- (ytn * rd)+min(t_data$.y) 
      
      point_cloud <- b_data
    }
    
    # save('point_cloud', 'coords.x', 'coords.y', 'axis.limits', file="test.Rda")
    # Mapping from pixels to data space
    # These values must match the image output!
    range.x <- max( max(point_cloud[,1]*1.1 ) - 0 )
    range.y <- max( max(point_cloud[,2]*1.1 ) - 0 )
    
    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    # 0.06 -> default margin
    coords.x <- (coords.x- axis.limits[1])/range.plot.x + 0.06/2
    coords.y <- (1-0.06) - ((coords.y - axis.limits[4])/range.plot.y) + 0.06/2
    
    poly_df <- data.frame("x"=coords.x * range.x, "y"=coords.y * range.y  )
    
    fac_norm <-max( point_cloud[,1] )
    # xr <- c(min(point_cloud[,1]),max(point_cloud[,1]) )
    # yr <- c(min(point_cloud[,2]),max(point_cloud[,2]) )
    
    # point_cloud[,1] <- point_cloud[,1]/max( point_cloud[,1] )
    # point_cloud[,2] <- point_cloud[,2]/max( point_cloud[,2] )
    
    names(point_cloud) <- c('x', 'y')
    point_cloud <- point_cloud %>%
        mutate(flag = point.in.polygon( .$x, .$y, poly_df$x, poly_df$y, mode.checked = FALSE ))

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
    
    show_modal_spinner(spin="fading-circle", text = "Saving")
    # SAVE this as specific file to be read if needed... 
    # FIXME Likely will be collected by gc at some point...    
    fout <- paste0('/tmp/', ctx$workflowId, '_', ctx$stepId, '.png')
    raw <- base64enc::base64decode(what = substr(input$save[1], 23, nchar(input$save[1])))
    png::writePNG(png::readPNG(raw), fout)    


    ctx <- getCtx(session)
    
    
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))
    
    flagDf <- df$data %>%
      cbind(.,data.frame("flag"=selected$flag)) %>%
      select(flag) %>%
      mutate(.ci=as.integer(0*seq(1,nrow(df$data)))) %>%
      ctx$addNamespace() 
    
    polyDf <-data.frame('x'=coords.x, 'y'=coords.y, '.ci'=as.integer(0*seq(1,length(coords.x))) ) %>%
      ctx$addNamespace()
    

    ctx$save(list(flagDf,polyDf))
    
    remove_modal_spinner()
  })
      
  
})

# TODO: Check here how does it appear after saving
get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Data")
  
  show_modal_spinner(spin="fading-circle", text = "Processing Image")
  
  #TODO Try to get the computed relation --> how to ask tercen for it?
  # browser()
  # steps <- ctx$workflow$steps
  # current_step <- Find(function(p) identical(p$id, ctx$stepId), steps)
  # task_state <- current_step$state$taskState
  # current_step$state$taskId
  #  
  # crel <- current_step$computedRelation$mainRelation
  # ctx$client$taskService
  # 
  # 
  # # STate
  # task <- ctx$client$taskService$get( current_step$state$taskId )
  # crel <- computed_task$computedRelation
  # 
  # findSchemaByDataDirectory
  # comp_schema <- ctx$client$tableSchemaService$findByQueryHash(task$query$qtHash)
  # 
  # task$taskHash
  # 
  # ctx$client$persistentService
  # 
  # 
  # computed_relation$mainRelation$id  
  # browser()
  # cs <- ctx$client$tableSchemaService$get(crel$mainRelation$id )
  # 
  # # env_to_df <- function(env){
  # #   tson <- env$toTson()
  # #   n <- lapply( tson$columns, function(x){  x$name    } )
  # #   dff<-data.frame((lapply( tson$columns, function(x){    x$values    } )))
  # #   names(dff)<- n
  # #   
  # #   return(dff)
  # # }
  # 
  # ctx$client$tableSchemaService$select( computed_task$computedRelation$mainRelation$id  )
  # 
  # ctx$client$tableSchemaService$get( computed_task$computedRelation$id)
  # 
  # ctx$client$tableSchemaService$get( computed_task$computedRelation$id )
  # 
  # computed_schema <- ctx$client$tableSchemaService$get(computed_task$schemaIds[[3]])
  # ctx$client$tableSchemaService$selectSchema( computed_schema  )
  # 
  # 
  # # ctx$
  
  
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

nearest_100k <- function( num, label=TRUE){
  fac = 6


  # while( (num %% (10^fac)) != num  ){
  #   fac = fac + 1
  # }
  #
  rnum <- num / ( 10^(fac-3) ) 

  if(label == FALSE){
    return( rnum * ( 10^(fac-3) )  ) 
  }
  round_num <- paste0(
    format( rnum, scientific=FALSE),
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