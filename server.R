library(shiny)
library(tercen)
library(tercenApi)
library(tim)
library(dplyr)
library(tidyr)

library(ggplot2)
library("scattermore")
library(grid)
library('png')


library(jsonlite)
library(shinybusy)

library(base64enc)
library(Rcpp) 


# CD4/CD8
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/e21fa40a-73e7-47c5-b84e-68c00d6e5738
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "e21fa40a-73e7-47c5-b84e-68c00d6e5738")


# 1D
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/c1c6c35e-3e6f-436c-bcfa-9cb5689604df
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "c1c6c35e-3e6f-436c-bcfa-9cb5689604df")

# SCatter
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/a3580d65-a077-4864-ace5-27716cc2dc55
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "a3580d65-a077-4864-ace5-27716cc2dc55")
server <- shinyServer(function(input, output, session) {

  source('plot_helpers.R')
  sourceCpp("polygon_test.cpp")
  
  df        <- reactiveValues( data=NULL, flag=NULL, data_obj=NULL  )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  image <- reactiveValues( loaded=NULL, 
                           range_x=NULL, 
                           plot_lim_x=NULL,
                           range_y=NULL,
                           plot_lim_y=NULL,
                           breaks_x=NULL,
                           breaks_x_rel=NULL,
                           breaks_y=NULL,
                           breaks_y_rel=NULL,)
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
      plot_mode$trans <- tmp[[3]]
      
      lab_names <- names(df$data)
      
      if( plot_mode$type  == '2d' ){
        res <- create_plot_2d(df$data, plot_mode$trans)
        imgfile <- res[[1]]
        image$range_x <- res[[2]]
        image$range_y <- res[[3]]
        image$plot_lim_x <- res[[4]]
        image$plot_lim_y <- res[[5]]
        image$breaks_x <- res[[6]]
        image$breaks_x_rel <- res[[7]]
        image$breaks_y <- res[[8]]
        image$breaks_y_rel <- res[[9]]
      }
      
      if( plot_mode$type  == '1d' ){
        res <- create_plot_1d(df$data, plot_mode$trans)
        imgfile <- res[[1]]
        image$range_x <- res[[2]]
        image$range_y <- res[[3]]
        image$plot_lim_x <- res[[4]]
        image$plot_lim_y <- res[[5]]
        image$breaks_x <- res[[6]]
        image$breaks_x_rel <- res[[7]]
        image$breaks_y <- res[[8]]
        image$breaks_y_rel <- res[[9]]
        session$sendCustomMessage("set_data_mode", '1d')
      }
      
      
    }else{
      session$sendCustomMessage("setViewOnly", runif(1))
    }
    
    
    image$loaded <- runif(1)
    
    session$sendCustomMessage("axis_bounds",append(image$plot_lim_x, image$plot_lim_y ) )
    
    
    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/png',
         alt = "Scatter plot failed to load.",
         width = "100%", height = "450")
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
    
    
    coords.type <- input$polygon$type 
    
    if( coords.type %in% c('quadrant', 'line')){
      # browser()
      # Be more lenient as the raycast might understimate size by a few
      coords.x <- append( coords.x, image$plot_lim_x )
      coords.y <- append( coords.y, image$plot_lim_y )
    }
    
    point_cloud <- df$data[,1:2]
    range.x <- max(image$range_x) - min(image$range_x) #max( max(point_cloud[,1]*1.1 ) + 0 )
    range.y <- max(image$range_y) - min(image$range_y) #max( max(point_cloud[,2]*1.1 ) + 0 )
    
    im_rx <- image$range_x
    im_ry <- image$range_y

    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    # browser()
    coords.x <- (coords.x- axis.limits[1])/range.plot.x 
    coords.y <- ((coords.y - axis.limits[4])/range.plot.y)
    
    coords.poly.x <- unlist(lapply( coords.x, function(x){
      get_converted_scale( x, image$breaks_x, image$breaks_x_rel )
    } ))
    
    coords.poly.y <- unlist(lapply( 1-coords.y, function(x){
      get_converted_scale( x, image$breaks_y, image$breaks_y_rel, decreasing=TRUE )
    } ))
    
    
    poly_df <- data.frame("x"=coords.poly.x,
                          "y"=coords.poly.y )

    names(point_cloud) <- c('x', 'y')
    
    
    poly.px.x <- c()
    poly.px.y <- c()
    
    
    if( coords.type == 'ellipsoid'){
      
      ce.x <- poly_df$x[1]
      ce.y <- poly_df$y[1]

      rx2 <- ( (poly_df$y[3]-ce.y)^2 + (poly_df$x[3]-ce.x)^2 )
      ry2 <- ( (poly_df$y[2]-ce.y)^2 + (poly_df$x[2]-ce.x)^2 )

      flag<-points_in_ellipsis(point_cloud$x, point_cloud$y,
                         ce.x, ce.y,
                         rx2, ry2)
      
      # point_cloud <- point_cloud %>%
      #   mutate(flag = flag)  
      
      
      poly.px.x <-append( poly.px.x, list(((coords.x[1])*range.plot.x)+image$plot_lim_x[1]*0.97))
      poly.px.y <-append( poly.px.y, list(((coords.y[1])*range.plot.y)+image$plot_lim_y[1]))
    }
    
    if( coords.type == 'poly'){
      # t0 <- Sys.time()
      # flag <- point.in.polygon( point_cloud$x,  point_cloud$y,
      #                            poly_df$x, poly_df$y, mode.checked = FALSE )
      # tf <- Sys.time()
      # 

      flag<-points_in_polygon( point_cloud$x,  point_cloud$y,
                                poly_df$x, poly_df$y)
      

      flag[flag>0] <- 1

      # point_cloud <- point_cloud %>%
      #   mutate(flag = flag)

      poly.px.x <-append( poly.px.x, list(((coords.x[1:(length(coords.x)-1)])*range.plot.x)+image$plot_lim_x[1]*0.97))
      poly.px.y <-append( poly.px.y, list(((coords.y[1:(length(coords.y)-1)])*range.plot.y)+image$plot_lim_y[1]))
    }
    
    if( coords.type == 'quadrant'){
      off <- 10
      
      poly_df$x[4] <- poly_df$x[4] - off
      poly_df$x[6] <- poly_df$x[6] - off
      poly_df$x[5] <- poly_df$x[5] + off
      poly_df$x[7] <- poly_df$x[7] + off
      
      poly_df$y[3] <- poly_df$y[3] - off
      poly_df$y[7] <- poly_df$y[7] - off
      poly_df$y[2] <- poly_df$y[2] + off
      poly_df$y[6] <- poly_df$y[6] + off
      
      quad <- create_quadrant(  poly_df, c(6,1), c(6,4), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.quadrant <- quad[[1]]
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))
      
  
      
      flag.top.left <- points_in_polygon( point_cloud$x,  point_cloud$y,
                                          poly.quadrant$x, poly.quadrant$y)
        # point.in.polygon( point_cloud$x, point_cloud$y,
        #                                  poly.quadrant$x, poly.quadrant$y, mode.checked = FALSE )
      
      
      
      quad <- create_quadrant(  poly_df, c(1,5), c(6,4), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.quadrant <- quad[[1]]
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))
      

      # 
      flag.top.right <- points_in_polygon( point_cloud$x,  point_cloud$y,
                                           poly.quadrant$x, poly.quadrant$y)
      
      
      quad <- create_quadrant(  poly_df, c(6,1), c(4,3), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.quadrant <- quad[[1]]
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))

      
      flag.bottom.left <- points_in_polygon( point_cloud$x,  point_cloud$y,
                                             poly.quadrant$x, poly.quadrant$y)
      
      
      quad <- create_quadrant(  poly_df, c(1,5), c(4,3), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.quadrant <- quad[[1]]
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))
      

      
      flag.bottom.right <- points_in_polygon( point_cloud$x,  point_cloud$y,
                                              poly.quadrant$x, poly.quadrant$y)
      
      # browser()
      

      
      flag.top.left[flag.top.left >= 1] <- 1
      flag.top.right[flag.top.right >= 1] <- 1
      flag.bottom.left[flag.bottom.left >= 1] <- 1
      flag.bottom.right[flag.bottom.right >= 1] <- 1
      
      # flag is used to plot percentages in the UI
      flag <- flag.top.left +
        flag.top.right*2 +
        flag.bottom.left*3 +
        flag.bottom.right*4

      # pref <- input$gateFlagPref
      # 
      # point_cloud <- point_cloud %>%
      #   mutate("{pref}_NegNeg" := flag.bottom.left)  %>%
      #   mutate("{pref}_PosNeg" := flag.bottom.right)  %>%
      #   mutate("{pref}_NegPos" := flag.top.left)  %>%
      #   mutate("{pref}_PosPos" := flag.top.right)  
      

    }
    
    if( coords.type == 'line'){
      # browser()
      quad <- create_quadrant(  poly_df, c(4,1), c(1,3), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))
      
      flag.left <- point_cloud$x < (poly_df$x[1])
      
      quad <- create_quadrant(  poly_df, c(1,5), c(1,3), coords.x, coords.y, range.plot.x, range.plot.y, image$plot_lim_x, image$plot_lim_y  )
      poly.px.x <-append( poly.px.x, list(quad[[2]]))
      poly.px.y <-append( poly.px.y, list(quad[[3]]))
      
      flag.right <- point_cloud$x >= (poly_df$x[1])
      
      flag.left[flag.left >= 1] <- 1
      flag.right[flag.right >= 1] <- 1
      
      flag <- flag.left + flag.right*2
      
      pref <- input$gateFlagPref

      # point_cloud <- point_cloud %>%
      #   mutate("{pref}_Neg" := flag.left)  %>%
      #   mutate("{pref}_Pos" := flag.right)  

    }
    
    pcts <- c()
    xs <- c()
    ys <- c()
    
    flag_vals <- unique(flag)
    
    for( fl in flag_vals){
      if(fl > 0){
        pct <-  100*sum(flag == fl) / length(flag)
        pct <-  c(paste0( format(pct, digits = 3, scientific = FALSE), '%') )
        
        pcts <- append( pcts, pct)
        
        xs <- append( xs, mean( (poly.px.x[[fl]] )) )
        ys <- append( ys,  mean( (poly.px.y[[fl]] )) )
      }
    }
    
    selected$flag <- flag
    
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
  

  observeEvent( input$clearBtn, {
    session$sendCustomMessage("clear_poly", "Clear polygon")
  })
  
  # TODO DELETE
  # observeEvent( input$transformSelected, {
  #   show_modal_spinner(spin="fading-circle", text = "Updating")
  #   plot_mode$trans <- input$transformSelected
  #   
  # })
  
  observeEvent( input$save, {
    ctx <- getCtx(session)
    # Check the barplot operator --> Do the result plot
    show_modal_spinner(spin="fading-circle", text = "Saving")

    fout <- paste0( tempfile(), ".png")
    raw <- base64enc::base64decode(what = substr(input$save, 23, nchar(input$save)))
    png::writePNG(png::readPNG(raw), fout)

    img_df <- tim::png_to_df(fout, filename = fout)


    img_df$mimetype <- 'image/png'
    img_df$filename <- fout #paste0( stp$name, '_Gate')
    
    img_df <- img_df %>%
      ctx$addNamespace() %>%
      as_relation()

    flag_vec <- as.numeric(selected$flag)
    pref <- input$gateFlagPref
    
    flags <- tibble("{pref}"=flag_vec) 
    
    coords.type <- input$polygon$type 
    if( coords.type == 'line'){
      flags <- tibble("{pref}_Neg" := ifelse(flag_vec==1, 1, 0)) %>%
        mutate("{pref}_Pos" := ifelse(flag_vec==2, 1, 0)   )
    }
    
    if( coords.type == 'quadrant'){
      flags <- tibble("{pref}_NegNeg" := ifelse(flag_vec==3, 1, 0)) %>%
        mutate("{pref}_PosNeg" := ifelse(flag_vec==4, 1, 0)   )  %>%
        mutate("{pref}_NegPos" := ifelse(flag_vec==1, 1, 0)   )  %>%
        mutate("{pref}_PosPos" := ifelse(flag_vec==2, 1, 0)   )  
    }
    

    
    flagDf <- flags %>%
      mutate(.i = unlist(unname(df$data["rowId"]))) %>%
      ctx$addNamespace() %>%
      as_relation() %>%
      left_join_relation(ctx$crelation, ".i", ctx$crelation$rids) %>%
      left_join_relation(img_df, list(), list()) %>%
      as_join_operator(ctx$cnames, ctx$cnames) %>%
      save_relation(ctx)

    
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
  
  
  wkf <- ctx$workflow
  stps <- wkf$steps

  current_step <- Find(function(p) identical(p$id, ctx$stepId), stps)

  y_data_name <- current_step$model$axis$xyAxis[[1]]$yAxis$graphicalFactor$factor$name
  
  
  prev_step_idx <- which( unlist( lapply( unlist(stps), function(x){
    tryCatch({
      return(y_data_name %in% unlist(x$computedRelation$joinOperators[[1]]$rightRelation$outNames))
    }, error=function(cond){
      return(FALSE)  
    })
  })))
  
  prev_step <- stps[[prev_step_idx]]
  op_repo <- prev_step$model$operatorSettings$operatorRef$url$uri
  
  data_trans <- 'linear'
  if( grepl( "biexponential_transform", op_repo, fixed = TRUE) ){
    data_trans <- 'biexp'
  }
  
  if( grepl( "logicle_transform", op_repo, fixed = TRUE) ){
    data_trans <- 'logicle'
  }
  
  if( grepl( "log_transform", op_repo, fixed = TRUE) ){
    data_trans <- 'log'
  }
  

  progress$close()
  # remove_modal_spinner()
  
  return( list(df, data_mode, data_trans))
  
}
#5c94e6bc3d934d0d2a245d410e02e701

getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}


#quad <- create_quadrant(  poly_df, c(4,1), c(1,3), coords.x, coords.y  )
create_quadrant <- function(  poly_df, x, y, coords.x, coords.y, range.plot.x, range.plot.y, plot_lim_x, plot_lim_y  ){
  poly.quadrant <- data.frame( x=c(poly_df$x[x[1]], poly_df$x[x[2]], poly_df$x[x[2]], poly_df$x[x[1]], poly_df$x[x[1]]),
                               y=c(poly_df$y[y[1]], poly_df$y[y[1]], poly_df$y[y[2]], poly_df$y[y[2]], poly_df$y[y[1]])
  )
  
  poly.px.x <-   c(coords.x[x[1]]*range.plot.x + plot_lim_x[1],
                   coords.x[x[2]]*range.plot.x + plot_lim_x[1], 
                   coords.x[x[2]]*range.plot.x + plot_lim_x[1], 
                   coords.x[x[1]]*range.plot.x + plot_lim_x[1], 
                   coords.x[x[1]]*range.plot.x + plot_lim_x[1])
  
  poly.px.y <-   c(coords.y[y[1]]*range.plot.y + plot_lim_y[1],
                   coords.y[y[1]]*range.plot.y + plot_lim_y[1], 
                   coords.y[y[2]]*range.plot.y + plot_lim_y[1], 
                   coords.y[y[2]]*range.plot.y + plot_lim_y[1], 
                   coords.y[y[1]]*range.plot.y + plot_lim_y[1])
  
  return(list(poly.quadrant, poly.px.x, poly.px.y))
}



get_converted_scale <- function( x, breaks, breaks_rel, decreasing=FALSE ){
  # breaks <- c(6000,  7000,  8000,  9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000)
  # breaks_rel <- c(0.014, 0.072, 0.123, 0.167, 0.206, 0.467, 0.619, 0.727, 0.811, 0.879, 0.937, 0.987)
  # x <-0
  # 
  
  if( x <= min(breaks_rel)){
    return(min(breaks))
  }else{
    if( decreasing == FALSE){
      low_mark <- which(unlist(lapply( breaks_rel, function(v){
        return(v < x)
      })))
      rng.x.idx <- c( low_mark[length(low_mark)], low_mark[length(low_mark)]+1)
    }else{
      # browser()
      low_mark <- which(unlist(lapply( breaks_rel, function(v){
        return(v >= x)
      })))
      rng.x.idx <- c( low_mark[1]-1, low_mark[1] )
      
    }
    
    
    rng.x     <- c( breaks[rng.x.idx[1]], breaks[rng.x.idx[2]] )
    rng.x.rel <- c( breaks_rel[rng.x.idx[1]], breaks_rel[rng.x.idx[2]] )
    
  }
  
  
  # NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
  
  xt <- (((x - rng.x.rel[1]) * (  rng.x[2]-rng.x[1] )) / (rng.x.rel[2]-rng.x.rel[1])) + rng.x[1]
  return(xt)
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