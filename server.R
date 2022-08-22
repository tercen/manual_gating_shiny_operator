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
library('png')

library(flowCore)
library(flowWorkspace)

library(jsonlite)
library(shinybusy)

library(base64enc)


# CD4/CD8
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/95dd2038-15bb-4edf-ac91-1298a1189632
options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
options("tercen.stepId"= "95dd2038-15bb-4edf-ac91-1298a1189632")


# 1D
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/c1c6c35e-3e6f-436c-bcfa-9cb5689604df
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "c1c6c35e-3e6f-436c-bcfa-9cb5689604df")


source('plot_helpers.R')

server <- shinyServer(function(input, output, session) {
  
  df        <- reactiveValues( data=NULL, flag=NULL  )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  image <- reactiveValues( loaded=NULL, 
                           range_x=NULL, 
                           plot_lim_x=NULL,
                           range_y=NULL,
                           plot_lim_y=NULL)
  plot_mode <- reactiveValues( trans="linear", type="2d" )
  
  
  output$image_div <- renderImage({
    query = parseQueryString(session$clientData$url_search)
    op_mode <- query[["mode"]]
    # op_mode <- 'run'
    
    ## Map densities to colors (VIRIDIS)
    
    
    if( is.null(op_mode) || op_mode == "run"){
      tmp <-get_data(session)
      
      df$data <- tmp[[1]]
      plot_mode$type <- tmp[[2]]
      

      # lab_names <- names(df$data)
      
      if( plot_mode$type  == '2d' ){
        res <- create_plot_2d(df$data, plot_mode$trans)
        imgfile <- res[[1]]
        image$range_x <- res[[2]]
        image$range_y <- res[[3]]
        image$plot_lim_x <- res[[4]]
        image$plot_lim_y <- res[[5]]
      }
      

     }else{
      session$sendCustomMessage("setViewOnly", runif(1))
    }
    

    image$loaded <- runif(1)
    
    session$sendCustomMessage("axis_bounds",append(image$plot_lim_x, image$plot_lim_y ) )
    
    
    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/png',
         alt = "Scatter plot failed to load.")
  }, deleteFile = FALSE)
  

  # ===========================================================
  # EVENT triggered when a polygon drawing is finished
  # to calculate the % of data points inside of it
  # ===========================================================
  observeEvent( input$polygon, {
    axis.limits <- c(image$plot_lim_x[1], 
                     image$plot_lim_y[2],
                     image$plot_lim_x[2],
                     image$plot_lim_y[1])
    
    
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))

    # coords.x.px <- coords.x
    # coords.y.px <- coords.y
    
    coords.type <- input$polygon$type 
    
    if( coords.type == 'quadrant'){
      coords.x <- append( coords.x, image$plot_lim_x )
      coords.y <- append( coords.y, image$plot_lim_y )
    }

    
    point_cloud <- df$data[,1:2]
    range.x <- max(image$range_x) - min(image$range_x) #max( max(point_cloud[,1]*1.1 ) + 0 )
    range.y <- max(image$range_y) - min(image$range_y) #max( max(point_cloud[,2]*1.1 ) + 0 )
    
    im_rx <- image$range_x
    im_ry <- image$range_y
    
    # save('axis.limits', 'coords.x', 'coords.y', 'point_cloud',
         # 'range.x', 'range.y', 'im_rx', 'im_ry',
         # file='test.Rda')
    if( plot_mode$trans == 'biexp' ){
      t_data <- df$data[,1:2]
      colnames(t_data)    <- c('.x','.y')
      
      custom_biexp_scale <- create_custom_biexp_scale(pos_decades = 5, 
                                                      neg_decades = -1.5, 
                                                      width_basis = -13)
      
      
      point_cloud <- transform_xy( t_data, custom_biexp_scale)

    }
    
    if( plot_mode$trans == 'logicle' ){
      t_data <- df$data[,1:2]
      colnames(t_data)    <- c('.x','.y')
      
      custom_logicle_scale <- create_custom_logicle_scale()

      point_cloud <- transform_xy( t_data, custom_logicle_scale)

    }
    
    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    # 0.06 -> default margin
    
    coords.x <- (coords.x- axis.limits[1])/range.plot.x + 0.06/2
    coords.y <- (1-0.06) - ((coords.y - axis.limits[4])/range.plot.y) + 0.06/2
    

    poly_df <- data.frame("x"=(((coords.x - 0.06/2) * range.x) / 1) + min(image$range_x ),
                          "y"=(((coords.y - 0) * range.y) / 1) + min(image$range_y) )

    names(point_cloud) <- c('x', 'y')
    
    
    poly.px.x <- c()
    poly.px.y <- c()
    
    if( coords.type == 'poly'){
      point_cloud <- point_cloud %>%
          mutate(flag = point.in.polygon( .$x, .$y, poly_df$x, poly_df$y, mode.checked = FALSE ))  
      
      poly.px.x <-append( poly.px.x, coords.x*range.plot.x)
      poly.px.y <-append( poly.px.y, coords.y*range.plot.y)
    }
    
    if( coords.type == 'quadrant'){

      poly.quadrant <- data.frame( x=c(poly_df$x[6], poly_df$x[1], poly_df$x[1], poly_df$x[6], poly_df$x[6]),
                                   y=c(poly_df$y[6], poly_df$y[6], poly_df$y[4], poly_df$y[4], poly_df$y[6])
                                   )
      poly.px.x <-append( poly.px.x,
                       c(coords.x[6]*range.plot.x,
                       coords.x[1]*range.plot.x, 
                       coords.x[1]*range.plot.x, 
                       coords.x[6]*range.plot.x, 
                       coords.x[6]*range.plot.x))
      
      poly.px.y <-append( poly.px.y,
                          c(coords.y[6]*range.plot.y,
                            coords.y[6]*range.plot.y, 
                            coords.y[4]*range.plot.y, 
                            coords.y[4]*range.plot.y, 
                            coords.y[6]*range.plot.y))
      
      flag.top.left <- point.in.polygon( point_cloud$x, point_cloud$y,
                        poly.quadrant$x, poly.quadrant$y, mode.checked = FALSE )
      
      
      
      
      poly.quadrant <- data.frame( x=c(poly_df$x[1]+1, poly_df$x[5], poly_df$x[5], poly_df$x[1]+1, poly_df$x[1]+1),
                                   y=c(poly_df$y[6], poly_df$y[6], poly_df$y[4], poly_df$y[4], poly_df$y[6])
      )
      
      poly.px.x <-append( poly.px.x,
                          c((coords.x[1]+1)*range.plot.x,
                            coords.x[5]*range.plot.x, 
                            coords.x[5]*range.plot.x, 
                            (coords.x[1]+1)*range.plot.x, 
                            (coords.x[1]+1)*range.plot.x))
      
      poly.px.y <-append( poly.px.y,
                          c(coords.y[6]*range.plot.y,
                            coords.y[6]*range.plot.y, 
                            coords.y[4]*range.plot.y, 
                            coords.y[4]*range.plot.y, 
                            coords.y[6]*range.plot.y))
      
      flag.top.right <- point.in.polygon( point_cloud$x, point_cloud$y,
                                         poly.quadrant$x, poly.quadrant$y, mode.checked = FALSE )
      
      
      
      poly.quadrant <- data.frame( x=c(poly_df$x[6], poly_df$x[1], poly_df$x[1], poly_df$x[6], poly_df$x[6]),
                                   y=c(poly_df$y[4]-1, poly_df$y[4]-1, poly_df$y[3], poly_df$y[3], poly_df$y[4]-1)
      )
      
      
      poly.px.x <-append( poly.px.x,
                          c(coords.x[6]*range.plot.x,
                            coords.x[1]*range.plot.x, 
                            coords.x[1]*range.plot.x, 
                            coords.x[6]*range.plot.x, 
                            coords.x[6]*range.plot.x))
      
      poly.px.y <-append( poly.px.y,
                          c((coords.y[4]-1)*range.plot.y,
                            (coords.y[4]-1)*range.plot.y, 
                            coords.y[3]*range.plot.y, 
                            coords.y[3]*range.plot.y, 
                            (coords.y[4]-1)*range.plot.y))
      
      flag.bottom.left <- point.in.polygon( point_cloud$x, point_cloud$y,
                                         poly.quadrant$x, poly.quadrant$y, mode.checked = FALSE )
      
      
      poly.quadrant <- data.frame( x=c(poly_df$x[1], poly_df$x[5], poly_df$x[5], poly_df$x[1], poly_df$x[1]),
                                   y=c(poly_df$y[4]-1, poly_df$y[4]-1, poly_df$y[3], poly_df$y[3], poly_df$y[4]-1)
      )
      
      
      poly.px.x <-append( poly.px.x,
                          c((coords.x[1]+1)*range.plot.x,
                            coords.x[5]*range.plot.x, 
                            coords.x[5]*range.plot.x, 
                            (coords.x[1]+1)*range.plot.x, 
                            (coords.x[1]+1)*range.plot.x))
      
      poly.px.y <-append( poly.px.y,
                          c((coords.y[4]-1)*range.plot.y,
                            (coords.y[4]-1)*range.plot.y, 
                            coords.y[3]*range.plot.y, 
                            coords.y[3]*range.plot.y, 
                            (coords.y[4]-1)*range.plot.y))
      
      flag.bottom.right <- point.in.polygon( point_cloud$x, point_cloud$y,
                                          poly.quadrant$x, poly.quadrant$y, mode.checked = FALSE )
      
      
      flag.top.left[flag.top.left >= 1] <- 1
      flag.top.right[flag.top.right >= 1] <- 2
      flag.bottom.left[flag.bottom.left >= 1] <- 3
      flag.bottom.right[flag.bottom.right >= 1] <- 4
      
      flag <- flag.top.left + 
        flag.top.right + 
        flag.bottom.left + 
        flag.bottom.right
      
      point_cloud <- point_cloud %>%
        mutate(flag = flag)  
    }

    pcts <- c()
    xs <- c()
    ys <- c()
    
    flag_vals <- unique(point_cloud$flag)
    
    for( fl in flag_vals){
      if(fl > 0){
        pct <-  100*sum(point_cloud$flag == fl) / length(point_cloud$flag)
        pct <-  c(paste0( format(pct, digits = 3, scientific = FALSE), '%') )
        
        pcts <- append( pcts, pct)

        xs <- append( xs, image$plot_lim_x[1] + mean( (poly.px.x[fl] )) )
        ys <- append( ys, image$plot_lim_y[2] - mean( (poly.px.y[fl] )) + image$plot_lim_y[1] )
      }
    }
    
    # image$plot_lim_x[1]
    # browser()
    # selected$pct <- 100*sum(point_cloud$flag == 1) / length(point_cloud$flag)
    # selected$x <- mean( (coords.x )    * range.x )
    # selected$y <- mean( (coords.y )    * range.y )
    # selected$pct <- pcts
    # selected$x <- xs
    # selected$y <- ys
    selected$flag <- point_cloud$flag

    # df$flag <- point_cloud$flag
    # session$sendCustomMessage("pct_selected",  paste0( format(selected$pct, digits = 3, scientific = FALSE), '%') )
    coords.x.px <- coords.x
    coords.y.px <- coords.y
    session$sendCustomMessage("flag_info",
                               list(
                                 "pct"=as.list(pcts),
                                 "x"=as.list(xs),
                                 "y"=as.list(ys)
                               ))
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

    fout <- paste0( tempfile(), ".png") 
    raw <- base64enc::base64decode(what = substr(input$save[1], 23, nchar(input$save[1])))
    png::writePNG(png::readPNG(raw), fout)    

    img_df <- tim::png_to_df(fout, filename = "Gate.png")

    
    
    wkf<-ctx$client$workflowService$get(ctx$workflowId, ctx$stepId)
    stp <- Find(function(p) identical(p$id, ctx$stepId), wkf$steps)
    
    img_df$mimetype <- 'image/png'
    img_df$filename <- paste0( stp$name, '_Gate') 


    img_df <- img_df %>%
      ctx$addNamespace() %>%
      as_relation() #%>%
      #as_join_operator(list(), list()) 
    
    ctx <- getCtx(session)
    
    
    labs <- unname(as.list(ctx$rselect()))[[1]]

    xname <- tercen::remove.prefix( labs[1] )
    yname <- tercen::remove.prefix( labs[2] )

    # browser()
    flagDf <- data.frame("flag"=as.numeric(selected$flag)) %>%
      # mutate(!! xname:=unlist(unname(df$data[,1])) ) %>%
      # mutate(!! yname:=unlist(unname(df$data[,2])) ) %>%
      # mutate(.i = seq_len(nrow(.)) - 1L) %>%
      mutate(.i = unlist(unname(df$data[,3]))) %>%
      ctx$addNamespace() %>%
      as_relation() %>%
      left_join_relation(ctx$crelation, ".i", ctx$crelation$rids) %>%
      left_join_relation(img_df, list(), list()) %>%
      as_join_operator(ctx$cnames, ctx$cnames) %>%
      save_relation(ctx)
    
    
      
    
    # polyDf <-data.frame('x'=coords.x, 'y'=coords.y, '.ci'=as.integer(0*seq(1,length(coords.x))) ) %>%
      # ctx$addNamespace()
    # browser()
    # ctx$save(list(flagDf,img_df))
    # save_relation(list(flagDf,img_df), ctx)
    
    remove_modal_spinner()
  })
      
  
})


get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Data")
  
  show_modal_spinner(spin="fading-circle", text = "Loading")
  
  data_mode <- NULL
  
  if( nrow(ctx$rselect()) == 2 ){
    data_mode <- "2d"  
    # NOTE
    # If the column will always be rowId, then this step is unnecessary
    # and .ci can be used instead
    col_df <- ctx$cselect() %>%
      mutate( .ci = seq(0,nrow(.)-1) )

    chnames <- unname(as.list(ctx$rselect()))[[1]]
        
    tmp_df <- ctx$select( list(".y", ".ri", ".ci") )  %>%
      dplyr::left_join(., col_df, by=".ci")
    df <- data.frame( 
      x = tmp_df %>% 
        dplyr::filter( .ri == 0) %>% 
        dplyr::select(.y),
      y = tmp_df %>% 
        dplyr::filter( .ri == 1) %>% 
        dplyr::select(.y),
      rowid = tmp_df %>% 
        dplyr::filter(.ri == 1) %>% 
        dplyr::select(rowId))
    
    names(df) <- c(chnames[1], chnames[2], "rowId")
  }
  
  if( nrow(ctx$rselect()) == 1 ){
    data_mode <- "1d"  
    
    col_df <- ctx$cselect() %>%
      mutate( .ci = seq(0,nrow(.)-1) )
    
    chnames <- unname(as.list(ctx$rselect()))[[1]]

    
    tmp_df <- ctx$select( list(".y", ".ri", ".ci") )  %>%
      dplyr::left_join(., col_df, by=".ci")
    df <- data.frame( 
      y = tmp_df %>% 
        dplyr::filter( .ri == 0) %>% 
        dplyr::select(.y),
      rowid = tmp_df %>% 
        dplyr::filter(.ri == 0) %>% 
        dplyr::select(rowId))
    
    names(df) <- c(chnames[1], "rowId")
  }
  

  # df <- ctx %>%
  #   select(.x, .y)
    # names(df) <- c(ctx$xAxis[[1]], ctx$yAxis[[1]])
  
  progress$close()
  # remove_modal_spinner()
  
  return( list(df, data_mode))
  
}


getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
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