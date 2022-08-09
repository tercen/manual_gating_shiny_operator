library(shiny)
library(tercen)
library(dplyr)
library(tidyr)


library(ggplot2)
library("scattermore")
library(sp)

library(jsonlite)
library(shinybusy)



# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8
options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
options("tercen.stepId"= "f5203f95-59d1-4e4a-899f-d9fcfb8c4cf8")


server <- shinyServer(function(input, output, session) {
  df        <- reactiveValues( data=NULL, flag=NULL  )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  image <- reactiveValues( loaded=NULL  )
  
  output$image_div <- renderImage({
    
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
    # + labs(x=lab_names[1], y=lab_names[2])  +
    #   ggtitle("Manual Gating") +
    #   scale_y_continuous(breaks = yticks, labels = ytick_labels) +
    #   scale_x_continuous(breaks = xticks, labels = xtick_labels) +
    # theme_bw() +
    imgfile <-  paste0(tempfile(), '.jpeg') # '/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
    p <- ggplot() +
      geom_scattermost(
        as.matrix(df$data),
        pointsize=2,
        col=colors,
        pixels=c(900,900)) +
       labs(x=lab_names[1], y=lab_names[2])  +
        ggtitle("Manual Gating") +
        scale_y_continuous(breaks = yticks, labels = ytick_labels) +
        scale_x_continuous(breaks = xticks, labels = xtick_labels) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
            axis.line.x=element_line(color="#07070F" ),
            axis.line.y=element_line(color="#07070F" )) + 
      annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
    
    
    # if( !is.null(selected$pct) ){
    #   p <- p +
    #     geom_label(aes(x = selected$x, y = selected$y,
    #                    label = paste0( format(selected$pct, digits = 3, scientific = FALSE), '%') ), fill = "#888888", alpha=0.6)
    # }
    
    ggsave(imgfile, units='in', width=3, height=3, p ) 
    

    image$loaded <- runif(1)

    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/jpeg',
         alt = "Scatter plot failed to load.")
  }, deleteFile = TRUE)
  
  
  observeEvent( input$polygon, {
    
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))
    
    coords.type <- input$polygon$type # For later use
    
    axis.limits <- c(input$polygon$left, 
                    input$polygon$bottom,
                    input$polygon$right,
                    input$polygon$top) # For later use
    
    point_cloud <- df$data
    
    # save('point_cloud', 'coords.x', 'coords.y', 'axis.limits', file="test.Rda")
    # Mapping from pixels to data space
    range.x <- max( point_cloud[,1] ) - min( point_cloud[,1] )
    range.y <- max( point_cloud[,2] ) - min( point_cloud[,2] )
    
    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    # 0.06 -> default margin
    coords.x <- (coords.x- axis.limits[1])/range.plot.x + 0.06
    coords.y <- (axis.limits[2]-coords.y)/range.plot.y - 0.06/2
    
    poly_df <- data.frame("x"=coords.x, "y"=coords.y  )
    
    fac_norm <-max( point_cloud[,1] )
    xr <- c(min(point_cloud[,1]),max(point_cloud[,1]) )
    yr <- c(min(point_cloud[,2]),max(point_cloud[,2]) )
    
    point_cloud[,1] <- point_cloud[,1]/max( point_cloud[,1] )
    point_cloud[,2] <- point_cloud[,2]/max( point_cloud[,2] )
    
    names(point_cloud) <- c('x', 'y')
    point_cloud$flag <- point_cloud %>%
        mutate(flag = point.in.polygon( .$x, .$y, poly_df$x, poly_df$y, mode.checked = FALSE ))

    selected$pct <- 100*sum(point_cloud$flag == 1) / nrow(point_cloud)
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
  
  observeEvent( input$saveBtn, {
    ctx <- getCtx(session)
    
    df$data %>%
      cbind(.,selected$flag) %>%
      select(flag) %>%
      ctx$addNamespace() %>%
      ctx$save()
  })
      
  
})


get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Data")
  
  show_modal_spinner(spin="fading-circle", text = "Processing Image")
  
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