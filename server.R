library(shiny)
library(tercen)
library(tercenApi)
library(tim)
library(dplyr)
library(tidyr)
library(stringr)


library(ggplot2)
library("scattermore")
library(ggforce)
library(grid)
library('png')


library(jsonlite)
library(shinybusy)

library(base64enc)
library(Rcpp) 


# library(reactlog)
# reactlog_enable()
#reactlog_disable()

source('plot_helpers.R')
sourceCpp("polygon_test.cpp")

# CD4/CD8
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/e21fa40a-73e7-47c5-b84e-68c00d6e5738
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "e21fa40a-73e7-47c5-b84e-68c00d6e5738")


# Log CD4/CD8
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/54fcb8e5-d483-4997-9a0e-95269e597e76
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "54fcb8e5-d483-4997-9a0e-95269e597e76")

# 1D
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/52fae11a-10ee-4204-9164-3485fa5c1af5
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "52fae11a-10ee-4204-9164-3485fa5c1af5")

# SCatter
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/a3580d65-a077-4864-ace5-27716cc2dc55
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "a3580d65-a077-4864-ace5-27716cc2dc55")

# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/accfd52a-e2dd-47d8-9ede-15beaa23e034
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "accfd52a-e2dd-47d8-9ede-15beaa23e034")

#http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/c9f9c606-38bc-4284-940b-dd06dfe63ce5
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "c9f9c606-38bc-4284-940b-dd06dfe63ce5")

# Multifile CD4
# http://127.0.0.1:5402/admin/w/b68ce8bb9db1120cb526d82c5b32a6d2/ds/856c9b56-81a0-4c4b-b1e2-d361cb297b9b
# options("tercen.workflowId"= "b68ce8bb9db1120cb526d82c5b32a6d2")
# options("tercen.stepId"= "856c9b56-81a0-4c4b-b1e2-d361cb297b9b")


server <- shinyServer(function(input, output, session) {
  df        <- reactiveValues( data=NULL, file=NULL, files=NULL )
  selected <- reactiveValues( pct=NULL, x=NULL, y=NULL , flag=NULL   )
  
  # plot_mode <- reactiveValues( trans="linear", type="2d", file_mode=NULL )
  plot_transform <- 'linear'
  plot_type    <- '2d'
  
  op_file <- reactiveValues()
  
  image <- list( loaded=NULL, 
                 range_x=NULL, 
                 plot_lim_x=NULL,
                 range_y=NULL,
                 plot_lim_y=NULL,
                 breaks_x=NULL,
                 breaks_x_rel=NULL,
                 breaks_y=NULL,
                 breaks_y_rel=NULL)
  
  gates <- list( filenames=NULL, unlinked=NULL, pcts=NULL, 
                           xs=NULL, ys=NULL, flags=NULL, types=NULL,
                           poly_px_x=NULL, poly_px_y=NULL, poly_df=NULL)
  
  # Dynamic content does not work well with ignoreInit, so this is a counter used
  # to prevent multiple triggers of renderImage
  init_mgr <- list( fileChooser=0 )
  
  output$fileChooser<-renderUI({
    req(op_file$mode)

    if( !is.null(op_file$mode) && op_file$mode == "many"){
      filenames <- unname(unlist(unique( df$data['filename'])))
      
      session$sendCustomMessage("show_fileChooser", 1)
      selectInput("file", "Filename", filenames,   selectize = FALSE, selected = NULL)  
     
    }
  })
  
  observe({
    tmp <-get_data(session)
    df$data <- tmp[[1]]
    plot_type <<- tmp[[2]]
    plot_transform <<- tmp[[3]]
    op_file$mode <- tmp[[4]]  
  })
  
  output$image_div <- renderImage({
    
    query = parseQueryString(session$clientData$url_search)
    op_mode <- query[["mode"]]

    if( is.null(op_mode) || op_mode == "run"){
      # if( is.null( df$data) ){ # Only need to load the data once
      #   tmp <-get_data(session)
      #   df$data <- tmp[[1]]
      #   plot_type <- tmp[[2]]
      #   plot_transform <- tmp[[3]]
      #   op_file$mode <- tmp[[4]]  
      # }
      
      lab_names <- names(df$data)
      
      if( isolate(op_file$mode) == "many"){
        if(is.null(df$file)){
          # selectInput is not yet initialized, take the first name
          data<- df$data %>% 
            dplyr::filter( filename == df$data$filename[[1]])   

        }else{
          data<- df$data %>% 
            dplyr::filter( filename == df$file)   
        }
        
      }else{
        data<- df$data 
      }
      
      if( plot_type  == '2d' ){
        xlim <- c(min(df$data[,1]), max(df$data[,1]))  
        ylim <- c(min(df$data[,2]), max(df$data[,2]))
        
        res <- create_plot_2d(data, 
                              isolate(plot_transform),
                              xlim=xlim, 
                              ylim=ylim)
        imgfile <<- res[[1]]
        image$range_x <<- res[[2]]
        image$range_y <<- res[[3]]
        image$plot_lim_x <<- res[[4]]
        image$plot_lim_y <<- res[[5]]
        image$breaks_x <<- res[[6]]
        image$breaks_x_rel <<- res[[7]]
        image$breaks_y <<- res[[8]]
        image$breaks_y_rel <<- res[[9]]
      }
      
      if( plot_type  == '1d' ){
        res <- create_plot_1d(data, plot_transform)
        imgfile <<- res[[1]]
        image$range_x <<- res[[2]]
        image$range_y <<- res[[3]]
        image$plot_lim_x <<- res[[4]]
        image$plot_lim_y <<- res[[5]]
        image$breaks_x <<- res[[6]]
        image$breaks_x_rel <<- res[[7]]
        image$breaks_y <<- res[[8]]
        image$breaks_y_rel <<- res[[9]]
        session$sendCustomMessage("set_data_mode", '1d')
      }
      
      
    }else{
      session$sendCustomMessage("setViewOnly", runif(1))
    }
    

    # Send the plot bounds to the Client, which uses them to draw
    # quadrant gates
    session$sendCustomMessage("axis_bounds",append(image$plot_lim_x, image$plot_lim_y ) )
    
    if( op_file$mode == 'many' ){
      fnames <-  gates$filenames 
      sfile <- isolate( df$file )
      
      # Send polygon coordinates to the client so it can replicate the drawing
      # across them
      if( !is.null(fnames) && !is.null(sfile)){
        fidx <- which( sfile == fnames )
        if( length(fidx > 0 )){
          session$sendCustomMessage("poly_coords",
                                    list(
                                      "pct"=gates$pcts[[fidx]],
                                      "x"=gates$xs[[fidx]],
                                      "y"=gates$ys[[fidx]],
                                      "px"=gates$poly_px_x[[fidx]],
                                      "py"=gates$poly_px_y[[fidx]],
                                      "ptype"=gates$types[fidx]
                                    ))
          
          
        }
      }
    }
    

    list(src = imgfile,
         id = "channel_image",
         contentType = 'image/png',
         alt = "Scatter plot failed to load.",
         width = "100%", height = "600")
  }, deleteFile = TRUE)
  
  observeEvent( input$file, {
    if( init_mgr$fileChooser >= 1){
      show_modal_spinner(spin="fading-circle", text = "Loading")
      df$file <- input$file
    }else{
      init_mgr$fileChooser <<- init_mgr$fileChooser + 1
    }
  })
  
  # ===========================================================
  # EVENT triggered when a polygon drawing is finished
  # to calculate the % of data points inside of it
  # ===========================================================
  observeEvent( input$polygon, {
    axis.limits <- c(image$plot_lim_x[1], 
                     image$plot_lim_y[2],
                     image$plot_lim_x[2],
                     image$plot_lim_y[1])
    
    plot_margin <- 0.03
    coords.x <- unlist(lapply( input$polygon$coords, function(x) x$x ))
    coords.y <- unlist(lapply( input$polygon$coords, function(x) x$y ))
    
    
    coords.type <- input$polygon$type 
    
    px_coords.x <- coords.x
    px_coords.y <- coords.y
    
    # Gate limits extend to the edge of the plot figure, so we add those coordinates
    # to the gate
    if( coords.type %in% c('quadrant', 'line')){
      coords.x <- append( coords.x, image$plot_lim_x )
      coords.y <- append( coords.y, image$plot_lim_y )
    }
    
    range.x <- max(image$range_x) - abs(min(image$range_x)) #max( max(point_cloud[,1]*1.1 ) + 0 )
    range.y <- max(image$range_y) - abs(min(image$range_y)) #max( max(point_cloud[,2]*1.1 ) + 0 )
    
    im_rx <- image$range_x
    im_ry <- image$range_y

    range.plot.x <- abs(axis.limits[3] - axis.limits[1])
    range.plot.y <- abs(axis.limits[2] - axis.limits[4])
    
    coords.x <- (coords.x- axis.limits[1])/range.plot.x 
    coords.y <- (((coords.y) - axis.limits[4])/range.plot.y) 
    
    # Change range to 0 - 1, relative to the full axis area (plot_margin included)
    coords.x <- (((coords.x - plot_margin) * (1 - 0)) / ((1-plot_margin) - plot_margin)) + 0
    coords.y <- (((coords.y - plot_margin) * (1 - 0)) / ((1-plot_margin) - plot_margin)) + 0
    
    # Change from 0 - 1 to min(data) - max(data) coordiantes
    coords.poly.x <- (((coords.x - 0) * ( max(df$data[,1]) - min(df$data[,1]))) / (1 - 0)) + min(df$data[,1])
    coords.poly.y <- (((1-coords.y - 0) * (max(df$data[,2]) - min(df$data[,2]))) / (1 - 0)) + min(df$data[,2])

    poly_df <- data.frame("x"=coords.poly.x,
                          "y"=coords.poly.y )

    if( op_file$mode == 'many' ){
      if( is.null(df$files)){
        df$files <- sort(unique(unlist(df$data$filename)))
      }
      
      for( i in seq(1, length(df$files)) ){
        file <- df$files[[i]]
        point_cloud <- df$data[df$data$filename == file, 1:2]
        names(point_cloud) <- c('x', 'y')
        

        res <- calculate_poly_flags( poly_df, point_cloud, 
                                     coords.x, coords.y, coords.type, image,
                                     range.plot.x, range.plot.y )  

        
        # TODO NOTE
        # Changing a gate marked as linked will alter all the linked gates
        # In a future version, it will be possible to unlink gates and alter them individually
        if( file %in% gates$filenames ){
          fidx <- which(file == gates$filenames)
          gates$filenames[fidx] <<- list(file)
          gates$unlinked[fidx] <<- list(FALSE)
          gates$pcts[fidx] <<- res[4]
          gates$xs[fidx] <<- res[2]
          gates$ys[fidx] <<- res[3]
          gates$poly_px_x[fidx] <<- list(px_coords.x)
          gates$poly_px_y[fidx] <<- list(px_coords.y)
          gates$types[fidx] <<- list(coords.type)
          gates$flags[fidx] <<- res[1]
          gates$poly_df[fidx] <<- list(poly_df)
        }else{
          gates$filenames <<- append( gates$filenames, list(file) )
          gates$unlinked <<- append( gates$unlinked, list(FALSE))
          gates$pcts <<- append( gates$pcts, res[4])
          gates$xs <<- append( gates$xs, res[2])
          gates$ys <<- append( gates$ys, res[3])
          
          gates$poly_px_x <<- append( gates$poly_px_x, list(px_coords.x))
          gates$poly_px_y <<- append( gates$poly_px_y, list(px_coords.y))
          
          gates$types <<- append( gates$types, list(coords.type))
          
          gates$flags <<- append( gates$flags, res[1])
          gates$poly_df <<- append( gates$poly_df, list(poly_df) )
        }
        

        if( (is.null(df$file) && file == df$files[[1]]) ||
            (!is.null(df$file) && file == df$file) ){
          

          selected$flag <- res[[1]]
          
          pcts <- res[[4]][[1]]
          xs <- res[[2]][[1]]
          ys <- res[[3]][[1]]
          
          
          if( length( pcts ) == 1){
            pcts <- list(pcts)
            xs <- list(xs)
            ys <- list(ys)
          }
          session$sendCustomMessage("flag_info",
                                    list(
                                      "pct"=pcts,
                                      "x"=xs,
                                      "y"=ys
                                    ))
        }
        
      }
    }else{
      point_cloud <- df$data[,1:2]
      names(point_cloud) <- c('x', 'y')

      
      res <- calculate_poly_flags( poly_df, point_cloud, 
                                   coords.x, coords.y, coords.type, image,
                                   range.plot.x, range.plot.y )
      
      gates$filenames <<- list() 
      gates$unlinked <<- list()
      gates$pcts <<- res[4]
      gates$xs <<- res[2]
      gates$ys <<- res[3]
      
      gates$poly_px_x <<-  list(px_coords.x)
      gates$poly_px_y <<-  list(px_coords.y)
      
      gates$types <<- list(coords.type)
      
      gates$flags <<- res[1]
      gates$poly_df <<- list(poly_df)

      selected$flag <<- res[[1]]
      
      pcts <<- res[[4]][[1]]
      xs <<- res[[2]][[1]]
      ys <<- res[[3]][[1]]
      # browser()
      if( length( pcts ) == 1){
        pcts <- list(pcts)
        xs <- list(xs)
        ys <- list(ys)
      }
      session$sendCustomMessage("flag_info",
                                list(
                                  "pct"=pcts,
                                  "x"=xs,
                                  "y"=ys
                                ))
    }
  })
  

  observeEvent( input$remove_spinner, {
    remove_modal_spinner()
  })
  

  observeEvent( input$save, {
    ctx <- getCtx(session)
    show_modal_spinner(spin="fading-circle", text = "Saving")
    
    pref <- input$gateFlagPref
    flag_vec <- as.numeric(df$data$rowId)*0 # initialize flag
    flags <- tibble("{pref}":=flag_vec) 
    
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
    
    flags <- flags %>%
      dplyr::mutate(filename = df$data$filename)
    
    
      
    
    if(op_file$mode == "single"){
      flagDf <- flags %>%
        mutate(.i = unlist(unname(df$data["rowId"]))) 
      data<- df$data 
      gate_info <- tibble(pct=gates$pcts[[1]],
                          x=gates$xs[[1]],
                          y=gates$ys[[1]])  
      if( plot_type  == '2d' ){
        
        xlim <- c(min(df$data[,1]), max(df$data[,1]))  
        ylim <- c(min(df$data[,2]), max(df$data[,2]))
        
        res <- create_plot_2d(data, plot_transform, xlim, ylim, gates$poly_df[[1]], gates$types[[1]], gate_info)
        
        # res <- create_plot_2d(data, plot_transform, gates$poly_df[[1]], gates$types[[1]], gate_info)
      }
      
      if( plot_type  == '1d' ){
        res <- create_plot_1d(data, plot_transform, gates$poly_df[[1]], gates$types[[1]], gate_info)
      }
      
      img_df <- tim::png_to_df(res[[1]], filename = res[[1]])
      img_df$mimetype <- 'image/png'
      img_df$filename <- res[[1]]
      
      if( coords.type == 'quadrant'){
        flagDf[paste0(pref,"_NegNeg")] <- ifelse( gates$flags[[1]][[1]] == 3, 1, 0)
        flagDf[paste0(pref,"_PosNeg")] <- ifelse( gates$flags[[1]][[1]] == 4, 1, 0)
        flagDf[paste0(pref,"_NegPos")] <- ifelse( gates$flags[[1]][[1]] == 1, 1, 0)
        flagDf[paste0(pref,"_PosPos")] <- ifelse( gates$flags[[1]][[1]] == 2, 1, 0)

      }else if(coords.type == 'line'){
        flagDf[paste0(pref,"_Neg")] <- ifelse( gates$flags[[1]][[1]] == 1, 1, 0)
        flagDf[paste0(pref,"_Pos")] <- ifelse( gates$flags[[1]][[1]] == 2, 1, 0)
      }else{
        flagDf[pref] <- gates$flags[[1]][[1]] 
      }
      
    }else{
      for( fi in seq(1,length(df$files))){
        fname <- df$files[fi]
        
        rowId <- df$data %>%
          dplyr::filter( filename == fname) %>%
          dplyr::select(rowId)
        rowId <- unname(unlist(rowId))
        
        # print(fi)
        flagDf_tmp <- flags %>%
          dplyr::filter( filename == fname)   %>%
          mutate(".i" = rowId) 

        
        data <- df$data %>% 
            dplyr::filter( filename == fname)   
          
        gate_info <- tibble(pct=gates$pcts[[fi]],
                            x=gates$xs[[fi]],
                            y=gates$ys[[fi]])
        if( plot_type  == '2d' ){
          xlim <- c(min(df$data[,1]), max(df$data[,1]))  
          ylim <- c(min(df$data[,2]), max(df$data[,2]))
          
          # res <- create_plot_2d(data, plot_transform,
          #                       xlim=xlim, 
          #                       ylim=ylim)
          
          res <- create_plot_2d(data, plot_transform, xlim, ylim, gates$poly_df[[fi]], gates$types[[fi]], gate_info)
        }
        
        if( plot_type  == '1d' ){
          # browser()
          res <- create_plot_1d(data, plot_transform, gates$poly_df[[fi]], gates$types[[fi]], gate_info)
        }
        
        

        if( coords.type == 'quadrant'){
          flagDf_tmp[paste0(pref,"_NegNeg")] <- ifelse( gates$flags[[fi]][[1]] == 3, 1, 0)
          flagDf_tmp[paste0(pref,"_PosNeg")] <- ifelse( gates$flags[[fi]][[1]] == 4, 1, 0)
          flagDf_tmp[paste0(pref,"_NegPos")] <- ifelse( gates$flags[[fi]][[1]] == 1, 1, 0)
          flagDf_tmp[paste0(pref,"_PosPos")] <- ifelse( gates$flags[[fi]][[1]] == 2, 1, 0)
        }else if(coords.type == 'line'){
          flagDf_tmp[paste0(pref,"_Neg")] <- ifelse( gates$flags[[fi]][[1]] == 1, 1, 0)
          flagDf_tmp[paste0(pref,"_Pos")] <- ifelse( gates$flags[[fi]][[1]] == 2, 1, 0)
        }else{
          flagDf_tmp[pref] <- gates$flags[[fi]][[1]] 
        }
        

        img_df_tmp <- tim::png_to_df(res[[1]], filename = res[[1]])
        img_df_tmp$mimetype <- 'image/png'
        img_df_tmp$filename <- fname
        
        if( fi == 1){
          img_df <- img_df_tmp
          flagDf <- flagDf_tmp
        }else{
          img_df <- rbind(img_df, img_df_tmp)
          flagDf <- rbind( flagDf, flagDf_tmp)
        }
      }
    }
    
    
    flagDf <- flagDf %>%
      ctx$addNamespace() %>%
      as_relation() %>%
      left_join_relation(ctx$crelation, ".i", ctx$crelation$rids) %>%
      left_join_relation(img_df, list(), list()) %>%
      as_join_operator(ctx$cnames, ctx$cnames) %>%
      save_relation(ctx)
    

    for( fname in img_df$filename){
      unlink(fname)
    }
    
    
    remove_modal_spinner()
  })
  
  
})


get_data <- function( session ){
  ctx <- getCtx(session)
  progress <- Progress$new(session, min=1, max=1)
  progress$set(message="Loading Data")
  
  show_modal_spinner(spin="fading-circle", text = "Loading")
  
  data_mode <- NULL
  
  # Check the number of files present
  cnames <- unlist( ctx$cnames)
  
  is_fname <- unlist(lapply( cnames, function(x){ 
    sp <- str_split_fixed(x, "[.]", Inf)
    if( length(sp) > 1 ){
      sp <- sp[-1]
    }
    sp == "filename" 
  }))
  

  rdata <- ctx$rselect()
  rdata_rows <- nrow(rdata)
  
  if( rdata_rows == 2 ){
    data_mode <- "2d"  
    # NOTE

    col_df <- ctx$cselect() %>%
      mutate( .ci = seq(0,nrow(.)-1) )
    
    chnames <- unname(as.list(rdata))[[1]]
    # cnames <- unname(as.list(ctx$cnames))[[1]]

    tmp_df <- ctx$select( list(".y", ".ri", ".ci") )  %>%
      dplyr::left_join(., col_df, by=".ci")
    
    # Check if different number of columns were removed from the channels (e.g. due to being NaN)
    ch0 <- tmp_df %>% 
      dplyr::filter( .ri == 0)
    
    ch1 <- tmp_df %>% 
      dplyr::filter( .ri == 1)
    
    if( length(ch0$.ri) != length(ch1$.ri)){
      miss_cols <- union( setdiff(ch0$rowId, ch1$rowId), 
                            setdiff(ch1$rowId, ch0$rowId) )
      
      ch0 <- ch0 %>%
        filter( !rowId %in% miss_cols)
      
      ch1 <- ch1 %>%
          filter( !rowId %in% miss_cols)
      
      
      
      
      
      session$sendCustomMessage("show_removedData", paste0(length(miss_cols), " non-finite data points removed." ))
    }
    
    df <- data.frame( 
      x = ch0 %>% 
        dplyr::select(.y),
      y = ch1 %>% 
        dplyr::select(.y),
      rowid = ch1 %>% 
        dplyr::select(all_of(cnames[which(!is_fname)])))
    
    names(df) <- c(chnames[1], chnames[2], "rowId")
    if( any(is_fname) ){
      df <- cbind( df,
                   ch0 %>% 
                     dplyr::select(cnames[which(is_fname)]))
      names(df) <- c(chnames[1], chnames[2], "rowId", "filename")
      
      file_mode <- "many"
      

    }else{
      file_mode <- "single"
    }
    
  }
  
  if( rdata_rows == 1 ){
    data_mode <- "1d"  
    
    col_df <- ctx$cselect() %>%
      mutate( .ci = seq(0,nrow(.)-1) )
    
    chnames <- unname(as.list(rdata))[[1]]
    cnames <- unname(as.list(ctx$cnames))[[1]]
    
    
    tmp_df <- ctx$select( list(".y", ".ri", ".ci") )  %>%
      dplyr::left_join(., col_df, by=".ci")
    df <- data.frame( 
      y = tmp_df %>% 
        dplyr::filter( .ri == 0) %>% 
        dplyr::select(.y),
      rowid = tmp_df %>% 
        dplyr::filter(.ri == 0) %>% 
        dplyr::select(all_of(cnames)))
    
    names(df) <- c(chnames[1], "rowId")
    
    if( any(is_fname) ){
      df <- cbind( df,
                   tmp_df %>% 
                     dplyr::filter( .ri == 0) %>% 
                     dplyr::select(cnames[which(is_fname)]))
      names(df) <- c(chnames[1], "rowId", "filename")
      file_mode <- "many"
      
    }else{
      file_mode <- "single"
    }
  }
  
  #Data transform
  trans <- ctx$op.value("Data transform")

  if( is.null(trans) || trans == 'Linear' ){
    data_trans <- 'linear'  
  }else if( trans == 'Biexponential'){
    data_trans <- 'biexp'
  }else if( trans == 'Log'){
    data_trans <- 'log'
  }else if( trans == 'Logicle'){
    data_trans <- 'logicle'
  }
  
  cols <- ctx$colors
  
  if( length(cols) == 0){
    cols <- NULL
  }else{
    col_df <- ctx$select(c(".ci", ".ri", cols[[1]]))
    colors <- unname(unlist((col_df[,3])))
    
    
    pallete <- colorRampPalette(c("#7b3294","#c2a5cf", "#A7A7A7","#a6dba0", "#008837"))(256)  
    
    df <- df %>%
      mutate(color = pallete[cut(colors[col_df$.ri==0], 256)])
 
  }

  
  progress$close()

  
  return( list(df, data_mode, data_trans, file_mode))
  
}


getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}


calculate_poly_flags <- function( poly_df, point_cloud, 
                                  coords.x, coords.y, coords.type, image,
                                  range.plot.x, range.plot.y ){
  
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
    

    poly.px.x <-append( poly.px.x, list(((coords.x[1])*range.plot.x)+image$plot_lim_x[1]*0.97))
    poly.px.y <-append( poly.px.y, list(((coords.y[1])*range.plot.y)+image$plot_lim_y[1]))
  }
  
  if( coords.type == 'poly'){
    flag<-points_in_polygon( point_cloud$x,  point_cloud$y,
                             poly_df$x, poly_df$y)
    
    
    flag[flag>0] <- 1
    
    t_data_in <- tibble(point_cloud$x[flag==1], point_cloud$y[flag==1])
    t_data_out <- tibble(point_cloud$x[flag==0], point_cloud$y[flag==0])
    
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
    
    
    
    flag.top.left[flag.top.left >= 1] <- 1
    flag.top.right[flag.top.right >= 1] <- 1
    flag.bottom.left[flag.bottom.left >= 1] <- 1
    flag.bottom.right[flag.bottom.right >= 1] <- 1
    
    
    
    
    # flag is used to plot percentages in the UI
    flag <- flag.top.left +
      flag.top.right*2 +
      flag.bottom.left*3 +
      flag.bottom.right*4
    
    
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
    
    # pref <- input$gateFlagPref
    
  }
  
  pcts <- c()
  xs <- c()
  ys <- c()
  
  flag_vals <- sort(unique(flag))
  
  for( fl in flag_vals){
    if(fl > 0){
      pct <-  100*sum(flag == fl) / length(flag)
      pct <-  c(paste0( format(pct, digits = 3, scientific = FALSE), '%') )
      
      pcts <- append( pcts, pct)
      
      xs <- append( xs, mean( (poly.px.x[[fl]] )) )
      ys <- append( ys,  mean( (poly.px.y[[fl]] )) )
    }
  }

  return(list(list(flag), list(xs), list(ys), list(pcts)))
  # selected$flag <- flag
  
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