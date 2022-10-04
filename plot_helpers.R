# Finish up reviewing and cleaning up the code
create_plot_1d <- function( data, trans , gate_coords=NULL , gate_type=NULL, gate_info=NULL ){
  # Create 1d density plot
  xt <- data[,1]
  imgfile <-  paste0(tempfile(), '.png')
  lab_names <- names(data)
  
  xlim <- c(min(xt), max(xt))
  
  xs <- (xlim[2] - xlim[1])/5
  xticks <- seq( xlim[1], xlim[2], by=xs )

  if( trans == 'linear'){
    # if( xlim[2] < 1.5e5){
    #   xticks <- seq( xlim[1], xlim[2], by=1e4 )
    #   fac<-3
    # }else{
    #   xticks <- seq( xlim[1], xlim[2], by=5e4 )
    #   fac<-4
    # }

    xtick_labels <- get_breaks(xticks, label=TRUE)# unlist(lapply(xticks, function(x) get_breaks(x, label = TRUE, factor=fac)))
    
    xticks <- get_breaks(xticks, label=FALSE) #unlist(lapply( xticks, function(x) get_breaks(x, FALSE, fac)))
    
    p <- ggplot( 
      data=data.frame(x=xt),
      mapping=aes(x=x),
      pointsize=1.5,
      col=colors,
      pixels=c(600,600)) +
      labs(x=lab_names[1], y='Density')  +
      geom_density(fill = '#eff5fffd') +
      scale_x_continuous(breaks = xticks, 
                         labels = xtick_labels,
                         expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
             axis.line.x=element_line(color="#07070F" ),
             axis.line.y=element_line(color="#07070F" ),
             text = element_text(size=6)) + 
      annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
    
    pb <- ggplot_build(p)
    range_x <- pb$layout$panel_params[[1]]$x.range
    range_y <- pb$layout$panel_params[[1]]$y.range
    
    breaks_x <- append(append( range_x[1], pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user ), range_x[2])
    breaks_x.rel <-append(append(0, pb$layout$panel_params[[1]]$x.sec$break_info$major), 1)
    
    breaks_y <- append(append( range_y[1], pb$layout$panel_params[[1]]$y.sec$break_info$major_source_user ), range_y[2])
    breaks_y.rel <-append(append(0, pb$layout$panel_params[[1]]$y.sec$break_info$major), 1)
    
  }
  
  if( trans %in% c('biexp','logicle', 'log')){
    breaks.x <- xticks
    breaks.x.t <-  break_transform(breaks = breaks.x, 
                                 transformation = "biexp")
    
    # Data range is too narrow to sensibly display at the log scale if there is
    # 1 or 0 ticks
    fac <- 5
    mfac <- max(xt)
    while( mfac < 5 * 10^fac && fac > -3){ 
      fac <- fac - 1
    }
    fac <- fac - 1
    
    labs_x <- unlist(lapply(breaks.x, function(x) get_breaks(x, label = TRUE, factor=fac)))
    if( sum(breaks.x.t >= min(xt) & breaks.x.t <= max(xt)) > 10 ){
      breaks.x <- breaks.x.t
      labs_x <- custom_tick_labels(breaks.x)
    }

    xtd <- density(xt)
    p <- ggplot( 
      data=data.frame(x=xt),
      mapping=aes(x=x),
      col=colors,
      pixels=c(600,600)) +
      geom_density(fill = '#eff5fffd') +
      labs(x=lab_names[1], y='Density')  +
      scale_x_continuous(limits = c( min(xt), max(xt) ),
                         breaks = breaks.x,
                         labels = labs_x,
                         expand=c(0,0),
                         sec.axis = dup_axis(labels=c())) +
      scale_y_continuous(expand=c(0,0),
        sec.axis = dup_axis(labels = c())) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
             axis.line.x=element_line(color="#07070F" ),
             axis.line.y=element_line(color="#07070F" ),
             text = element_text(size=6)) + 
      annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
    
    
    p<- set_biexp_ticks(p, breaks.x)
    pb <- ggplot_build(p)
    
    # ad <- data.frame(.x = pb$layout$panel_params[[1]]$x.range)
    # br.x <- transform_x(pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user,
                        # custom_scale, df_range = xt )

    range_x <- pb$layout$panel_params[[1]]$x.sec$get_limits()
    range_y <- pb$layout$panel_params[[1]]$y.sec$get_limits()
    
    breaks_x <- append(append( range_x[1], pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user ), range_x[2])
    breaks_x.rel <-append(append(0, pb$layout$panel_params[[1]]$x.sec$break_info$major), 1)
    
    breaks_y <- append(append( range_y[1], pb$layout$panel_params[[1]]$y.sec$break_info$major_source_user ), range_y[2])
    breaks_y.rel <-append(append(0, pb$layout$panel_params[[1]]$y.sec$break_info$major), 1)
  }
  
  if( !is.null(gate_coords)){
    
    if( gate_type == 'line'){
      yr <-  pb$layout$panel_params[[1]]$y.sec$get_limits()
      
      xs <- c( gate_coords$x[4] + (gate_coords$x[2] - gate_coords$x[4])/2,
               gate_coords$x[2] + (gate_coords$x[5] - gate_coords$x[2])/2)
      ys <- c( yr[1] + (yr[2] - yr[1])/2, 
               yr[1] + (yr[2] - yr[1])/2)

      
      p <- p +
        geom_path(aes(x=gate_coords$x[c(2,3)],
                      y=yr ), data=tibble(c(1,1)),
                  size=0.5, color="black") +
        geom_path(aes(x=gate_coords$x[c(2,3)],
                      y=yr), data=tibble(c(1,1)),
                  size=0.25, color="white") +
        geom_label(aes(x=xs, y=ys), data=tibble(c(1,1)),
                   label=gate_info$pct[[1]],
                   alpha=0.5, fill="#AAAAAA",
                   size=2, fontface="bold",
                   label.padding = unit(0.12, "lines"))
    }
  }

  ggsave(imgfile, units='px', width=600, height=600, p )
  lims <- get_axis_plot_lims(imgfile)
  
  return(list(imgfile, range_x, range_y, lims[[1]], lims[[2]], breaks_x, breaks_x.rel, breaks_y, breaks_y.rel ))
}




# x_lims and y_lims are used to keep axis ranges consistent across different files
create_plot_2d <- function( data, trans, 
                            xlim = NULL, ylim=NULL,
                            gate_coords=NULL , gate_type=NULL, gate_info=NULL ){
  # Viridis pallete
  cols <-  colorRampPalette(c("#440154", "#3b528b", "#21918c", 
                              "#5ec962", "#fde725"))(256)
  lab_names <- names(data)
  
  
  if(nrow(data) < 50000 ){
    point_size <- 3
  }else{
    point_size <- 1.5
  }
  
  if( "color" %in% lab_names ){
    colors <- data$color
  }else{
    x <- densCols(unlist(data[,1]), unlist(data[,2]), colramp=colorRampPalette(c("black", "white")),
                  nbin=128)
    dens <- col2rgb(x)[1,] + 1L


    colors <- cols[dens]  
  }
  
  if( is.null(xlim) ){
    xlim <- c(min(data[,1]), max(data[,1]))  
  }
  
  if( is.null(ylim) ){
    ylim <- c(min(data[,2]), max(data[,2]))
  }
  
  xs <- (xlim[2] - xlim[1])/5
  ys <- (ylim[2] - ylim[1])/5
  
  
  xticks <- seq( xlim[1], xlim[2], by=xs )
  yticks <- seq( ylim[1], ylim[2], by=ys )
  
  imgfile <-   paste0(tempfile(), '.png')
  if( trans == 'linear'){

    labs_x <- get_breaks(xticks, label=TRUE)
    labs_y <- get_breaks(yticks, label=TRUE)
    
    breaks.x <- get_breaks(xticks, label=FALSE)
    breaks.y <- get_breaks(yticks, label=FALSE)
    
     #'/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
    # t_data <- tibble( data[,1], data[, 2])
    

    p <- ggplot() +
      geom_scattermost(
        as.matrix( data ),
        pointsize=point_size,
        col=colors,
        pixels=c(600,600)) +
      labs(x=lab_names[1], y=lab_names[2])  +
      scale_y_continuous(limits = ylim,
                         breaks = breaks.y,
                         expand=c(0.03,0.03),
                         labels = labs_y) +
      scale_x_continuous(limits = xlim,
                         breaks = breaks.x,
                         expand=c(0.03,0.03),
                         labels = labs_x) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
            axis.line=element_line(color="#07070F" ),
            text = element_text(size=6)) +
      annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)


    pb <- ggplot_build(p)

    range_x <- pb$layout$panel_params[[1]]$x.sec$get_limits()
    range_y <- pb$layout$panel_params[[1]]$y.sec$get_limits()

    breaks_x <- append(append( range_x[1], pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user ), range_x[2])
    breaks_x.rel <-append(append(0, pb$layout$panel_params[[1]]$x.sec$break_info$major), 1)

    breaks_y <- append(append( range_y[1], pb$layout$panel_params[[1]]$y.sec$break_info$major_source_user ), range_y[2])
    breaks_y.rel <-append(append(0, pb$layout$panel_params[[1]]$y.sec$break_info$major), 1)
    
    # # Pre-compute density lines in case the user wants to use this method
    # 
    # res<-contourLines(data[,1], data[,2])
    # 
    # m <- ggplot(b_data, aes(x = .x, y = .y)) +
    #   geom_density_2d_filled()
    # 
    # 
    # pbl <- ggplot_build(m)
    # contour_data <- pbl$data[[1]]
    # subgroup_idx <- as.numeric(rownames(unique(contour_data["group"])))
    # 
    # x_contour <- unname(unlist(contour_data["x"]))
    # y_contour <- unname(unlist(contour_data["y"]))
    # 
    # x_contour[1]
    # y_contour[1]
    # # NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
    # 
    # xt <- (((x_contour[1] - range_x[1]) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
    
  }

  if( trans %in% c('biexp','logicle', 'log')){
    # TODO 
    # Check a data with range large enough to test if ticks are being properly placed
    breaks.x.t <-  break_transform(breaks = xticks,
                                 transformation = "biexp")
    breaks.y.t <-  break_transform(breaks = yticks,
                                 transformation = "biexp")

    
    # These two are used when drawing the gate down below (to match the linear transform names)
    breaks.x <- breaks.x.t
    breaks.y <- breaks.y.t
    # Data range is too narrow to sensibly display at the log scale if there is
    labs_x <- get_breaks(breaks.x.t, label = TRUE)
    labs_y <- get_breaks(breaks.y.t, label = TRUE)
      
    p <- ggplot() +
      scale_x_continuous(limits = xlim, 
                              breaks = breaks.x.t,
                              expand=c(0.03,0.03),
                              # trans = custom_scale,
                              labels = labs_x,
                              sec.axis = dup_axis(labels=c())) +
        scale_y_continuous(limits = ylim,
                           breaks = breaks.y.t,
                           expand=c(0.03,0.03),
                           # trans = custom_scale,
                           labels = labs_y,
                           sec.axis = dup_axis(labels = c())) +
      geom_scattermost(
        data.matrix(data),
        pointsize=point_size,
        col=colors,
        pixels=c(600,600)) +
      labs(x=lab_names[1], y=lab_names[2])  +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
            axis.line=element_line(color="#07070F" ),
            text = element_text(size=8),
            axis.ticks.x.top = element_blank(),
            axis.ticks.y.right = element_blank())


    p<- set_biexp_ticks(p, breaks.x.t)

  
    pb <- ggplot_build(p)
  
    range_x <- pb$layout$panel_params[[1]]$x.sec$get_limits()
    range_y <- pb$layout$panel_params[[1]]$y.sec$get_limits()

    breaks_x <- append(append( range_x[1], pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user ), range_x[2])
    breaks_x.rel <-append(append(0, pb$layout$panel_params[[1]]$x.sec$break_info$major), 1)

    breaks_y <- append(append( range_y[1], pb$layout$panel_params[[1]]$y.sec$break_info$major_source_user ), range_y[2])
    breaks_y.rel <-append(append(0, pb$layout$panel_params[[1]]$y.sec$break_info$major), 1)
  }
  

  if( !is.null(gate_coords)){
    # browser()
    if( gate_type == 'poly'){
      cx <- mean(gate_coords$x)
      cy <- mean(gate_coords$y)
      
      # change plot lims for cases where parts of the gate were
      # outside the plot area
      min_plot_x <- min( min(gate_coords$x), xlim[1] )
      max_plot_x <- max( max(gate_coords$x), xlim[2] )
      min_plot_y <- min( min(gate_coords$y), ylim[1] )
      max_plot_y <- max( max(gate_coords$y), ylim[2] )
      
      p <- p +
        scale_y_continuous(limits = c(min_plot_y, max_plot_y) ,
                           breaks = breaks.y,
                           expand=c(0.03,0.03),
                           labels = labs_y) +
        scale_x_continuous(limits = c(min_plot_x, max_plot_x) ,
                           breaks = breaks.x,
                           expand=c(0.03,0.03),
                           labels = labs_x) +
        geom_path(aes(x=gate_coords$x, y=gate_coords$y), data=tibble(gate_coords$x),
                  size=1.5, color="black") +
        geom_path(aes(x=gate_coords$x, y=gate_coords$y), data=tibble(gate_coords$x),
                   size=0.25, color="white") +
        geom_label(aes(x=cx,
                       y=cy),
                   label=gate_info$pct[[1]],
                   alpha=0.5, fill="#AAAAAA",
                   size=2, fontface="bold",
                   label.padding = unit(0.12, "lines"))

      
    }
    
    if( gate_type == 'quadrant'){
      x_coords <- c()
      y_coords <- c()
      
      yr <- pb$layout$panel_scales_y[[1]]$get_limits()
      xr <- pb$layout$panel_scales_x[[1]]$get_limits()
      
      
      # To account for plot margins, replace the min/max in gate data by the
      # plotted minimum and maximum values
      
      gate_coords$x[c(4,6)] <- xr[1]+abs(xr[1]*0.005)
      gate_coords$x[c(5,7)] <- xr[2]-abs(xr[2]*0.005)

      gate_coords$y[c(3,7)] <- yr[1]+abs(yr[1]*0.005)
      gate_coords$y[c(2,6)] <- yr[2]-abs(yr[2]*0.005)
      
      # top left, top right, bottom-left, bottom right
      xs <- c(  gate_coords$x[4] + (gate_coords$x[2] - gate_coords$x[4])/2,
                gate_coords$x[2] + (gate_coords$x[5] - gate_coords$x[2])/2,
                gate_coords$x[4] + (gate_coords$x[2] - gate_coords$x[4])/2,
                gate_coords$x[2] + (gate_coords$x[5] - gate_coords$x[2])/2
                )
      
      ys <- c( gate_coords$y[4] + (gate_coords$y[6] - gate_coords$y[4])/2,
               gate_coords$y[4] + (gate_coords$y[6] - gate_coords$y[4])/2,
               gate_coords$y[3] + (gate_coords$y[4] - gate_coords$y[3])/2,
               gate_coords$y[3] + (gate_coords$y[4] - gate_coords$y[3])/2
               
               )
      
    
      for( i in seq(2, length(gate_coords$x)-2)){
        x_coords <- append(x_coords, gate_coords$x[1])
        x_coords <- append(x_coords, gate_coords$x[i])
        
        y_coords <- append(y_coords, gate_coords$y[1])
        y_coords <- append(y_coords, gate_coords$y[i])
      }

      
      min_plot_x <- min( min(gate_coords$x), xlim[1] )
      max_plot_x <- max( max(gate_coords$x), xlim[2] )
      min_plot_y <- min( min(gate_coords$y), ylim[1] )
      max_plot_y <- max( max(gate_coords$y), ylim[2] )
      
      p <- p +
        scale_y_continuous(limits = c(min_plot_y, max_plot_y) ,
                           breaks = breaks.y,
                           expand=c(0.03,0.03),
                           labels = labs_y) +
        scale_x_continuous(limits = c(min_plot_x, max_plot_x) ,
                           breaks = breaks.x,
                           expand=c(0.03,0.03),
                           labels = labs_x) +
        geom_path(aes(x=x_coords, 
                      y=y_coords),
                  size=0.5, color="black") +   
        geom_path(aes(x=x_coords, 
                      y=y_coords),
                  size=0.25, color="white") + 
        geom_label(aes(x=xs, y=ys), data=tibble(c(1,1,1,1)),
                   label=gate_info$pct[[1]],
                   alpha=0.5, fill="#AAAAAA",
                   size=2, fontface="bold",
                   label.padding = unit(0.12, "lines"))

    }
    
    if( gate_type == 'ellipsoid'){
      radius_a <- sqrt( (gate_coords$y[3]-gate_coords$y[1])**2 + (gate_coords$x[3]-gate_coords$x[1])**2)
      radius_b <- sqrt( (gate_coords$y[2]-gate_coords$y[1])**2 + (gate_coords$x[2]-gate_coords$x[1])**2)
      
      dy <- gate_coords$y[3] - gate_coords$y[1]
      dx <- gate_coords$x[3] - gate_coords$x[1]
      
      rot <- atan2(dy, dx)
      

      min_plot_x <- min( min(gate_coords$x), xlim[1] )
      max_plot_x <- max( max(gate_coords$x), xlim[2] )
      min_plot_y <- min( min(gate_coords$y), ylim[1] )
      max_plot_y <- max( max(gate_coords$y), ylim[2] )
      
      p <- p +
        scale_y_continuous(limits = c(min_plot_y, max_plot_y) ,
                           breaks = breaks.y,
                           expand=c(0.03,0.03),
                           labels = labs_y) +
        scale_x_continuous(limits = c(min_plot_x, max_plot_x) ,
                           breaks = breaks.x,
                           expand=c(0.03,0.03),
                           labels = labs_x) +
        geom_ellipse(aes(x0 = gate_coords$x[1], 
                         y0 = gate_coords$y[1], a = radius_a, b = radius_b, angle = rot),
                     color="black", size=0.5) + 
        geom_ellipse(aes(x0 = gate_coords$x[1], 
                         y0 = gate_coords$y[1], a = radius_a, b = radius_b, angle = rot),
                     color="white", size=0.25) + 
        geom_label(aes(x=gate_coords$x[1],
                       y=gate_coords$y[1]),
                   label=gate_info$pct[[1]],
                   alpha=0.5, fill="#AAAAAA",
                   size=2, fontface="bold",
                   label.padding = unit(0.14, "lines"))
    }
    
    
  }

  ggsave(imgfile, units='px', width=600, height=600, device='png', p )


  lims <- get_axis_plot_lims(imgfile)
  return(list(imgfile, range_x, range_y, lims[[1]], lims[[2]], breaks_x, breaks_x.rel, breaks_y, breaks_y.rel ))
}

get_axis_plot_lims <- function( imgfile ){
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
  plot_lim_x <- x.plot.lim
  plot_lim_y <- y.plot.lim
  
  return(list(plot_lim_x, plot_lim_y))
}

get_breaks <- function( num_list, label=TRUE){
  stp <- mean(diff(num_list))
  
  
  ndigits <- nchar(as.character(as.integer(abs(stp))))
  
  digits <- c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 )
  
  lbls   <- c( '', '', '', 'k', 'k', 'k', 'mi.', 'mi.', 'mi.',
               'bi.', 'bi.', 'bi.')
  
  facs <- c(0, 0, 0, 3, 3, 3, 6, 6, 6, 9, 9, 9)
  
  
  breaks <- c()
  # browser()
  for( num in num_list ){
    if( ndigits < 2){
      num <- as.numeric( format(num, digits=ndigits+1), scientif=FALSE )
    }else{
      
      num <- as.numeric( format(num, digits=ndigits), scientif=FALSE )
      
    }  

    if(label == TRUE){
      fac <- facs[which(digits==ndigits)]
      num <- num / 10^( fac  )
      if( ndigits < 2){
        num <- as.numeric( format(num, digits=ndigits-fac+1), scientif=FALSE )
      }else{
        num <- as.numeric( format(num, digits=ndigits-fac), scientif=FALSE )  
      }
      
      num <- paste0(
        as.character(num),
        lbls[ndigits])
    }

    breaks <- append(breaks,  num )
  }
  

  return( breaks )
}



custom_logicle_breaks <- function(x) {
  
  rng.raw <- range(x, na.rm = TRUE)
  
  v_min_max = c(minimum = x[1], 
                maximum = x[length(x)])
  
  log_min_max = c()
  for (i in c("minimum", "maximum")) {
    value = v_min_max[i]
    if ( i == "minimum" && sign(value) == 1 ) {
      log_min_max[i] = sign(value)*floor(log10(abs(value)))
    } else {
      log_min_max[i] = sign(value)*ceiling(log10(abs(value)))
    }
  }
  power = log_min_max["minimum"]:log_min_max["maximum"]
  sn = sign(power)
  pow = abs(power)
  
  decades = sn*10^pow; # node that decades includes 0. e.g.: -100, -10, -1, 0, 1, 10.
  n_decades = length(decades)
  n_ticks = (n_decades-1)*9 + 1 + 2*sum(decades==0) #if we have 0 included in our decades, add 2 to the number of ticks because we will tick at -1 and 1
  obj.Tick = rep(0, n_ticks)
  tick_index = 1
  previous_labeled_decade=NA
  
  for(k in 1:n_decades) {
    if (k == n_decades) {
      # write Tick for final decade
      obj.Tick[tick_index] = decades[k]
      # Fill any subsequent ticks
      # which may be labelled '' so that the Tick vector is
      # monotonically increasing;
      if (tick_index < n_ticks) {
        obj.Tick[(tick_index+1):length(obj.Tick)] = seq(obj.Tick[tick_index]+0.1, 
                                                        obj.Tick[tick_index]+0.2, 
                                                        length.out = n_ticks - tick_index)
      }
      break;
    }
    
    # write Tick for this decade in 9 increments if the ends of
    # the decades are powers of 10 increments if the right hand
    # end of the gap is 0 (i.e.10^{-inf})
    if (decades[k+1] == 0) {
      n_increments = 11
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else if (decades[k]==0) {
      n_increments = 9
      lhs = 1
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else {
      n_increments = 9
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    }
    #obj.Tick[tick_index:(tick_index+n_increments-1)] = trans.fun(seq(from = lhs, to = rhs, length.out = n_increments))
    obj.Tick[tick_index:(tick_index+n_increments-1)] = seq(from = lhs, to = rhs, length.out = n_increments)
    
    tick_index = tick_index + n_increments;
  }
  
  return(obj.Tick)
}

break_transform <- function(breaks, transformation)
{
  if ((transformation == "biexponential") || (transformation == "logicle")
      || (transformation == "log")) {
    x.breaks <- custom_logicle_breaks(breaks)
  } else {
    x.breaks <- breaks
  }
  return(x.breaks)
}

custom_log10 = function(tick) {
  if ( tick == 0) {
    tick.lbl = "0"
  } else { 
    pow <- log10(abs(tick))
    lbl <- paste0(sign(tick)*10, "^", pow)
    tick.lbl = parse(text = lbl)
  }
  return(tick.lbl)
}

custom_tick_labels <- function(breaks) {
  
  labelled_ticks <- c(sapply(seq(14, 1, -1), function(x) -10^x), 0, sapply(seq(1, 14, 1), function(x) 10^x))
  lbls <- sapply(breaks, custom_log10)
  lbls[! breaks %in% labelled_ticks] = ""
  
  return(lbls)
}


is_ten <- function(x){
  x<-abs(x)
  while( x > 10 ){
    x <- x %/% 10
  }
  
  return((x %% 10)==0)
}


is_five <- function(x){
  x<-abs(x)
  while( x > 10 ){
    x <- x %/% 10
  }
  
  return((x %% 5)==0)
}



set_biexp_ticks <- function(plt, breaks){
  bld     <- ggplot_build(plt)
  tickPos <- bld$layout$panel_params[[1]]$x$break_positions()
  
  longBr  <- list()
  medBr   <- list()
  shortBr <- list()
  
  for (i in seq_along(breaks)) {
    if(is_ten(breaks[i])  ){
      longBr <- append(longBr, tickPos[i])
    }else if(is_five(breaks[i]) ){
      medBr <- append(medBr, tickPos[i])
    }else{
      shortBr <- append(shortBr, tickPos[i])
    }
  }
  # browser()
  for(i in seq_along(longBr)){
    plt <- plt +
      annotation_custom(linesGrob(x = unit(c(longBr[i], longBr[i]), 'native'),y = unit(c(0,-0.3), 'cm'),
                                  gp=gpar(col='black', fill=NA, lwd=1.5) ) )
  }
  
  for(i in seq_along(medBr)){
    plt <- plt +
      annotation_custom(linesGrob(x = unit(c(medBr[i], medBr[i]), 'native'),y = unit(c(0,-0.2), 'cm'),
                                  gp=gpar(col='black',fill=NA, lwd=1) ) )
  }
  
  for(i in seq_along(shortBr)){
    plt <- plt +
      annotation_custom(linesGrob(x = unit(c(shortBr[i], shortBr[i]), 'native'),y = unit(c(0,-0.1), 'cm'),
                                  gp=gpar(col='black',fill=NA, lwd=1) ) )
  }
  
  # Remove original ticks
  plt <- plt + theme(axis.ticks.x=element_blank())
  
  plt <- plt +  coord_cartesian(clip = "off")
  return(plt)
}


custom_logicle_breaks <- function(x) {
  
  rng.raw <- range(x, na.rm = TRUE)
  
  v_min_max = c(minimum = x[1], 
                maximum = x[length(x)])
  
  log_min_max = c()
  for (i in c("minimum", "maximum")) {
    value = v_min_max[i]
    if ( i == "minimum" && sign(value) == 1 ) {
      log_min_max[i] = sign(value)*floor(log10(abs(value)))
    } else {
      log_min_max[i] = sign(value)*ceiling(log10(abs(value)))
    }
  }
  power = log_min_max["minimum"]:log_min_max["maximum"]
  sn = sign(power)
  pow = abs(power)
  
  decades = sn*10^pow; # node that decades includes 0. e.g.: -100, -10, -1, 0, 1, 10.
  n_decades = length(decades)
  n_ticks = (n_decades-1)*9 + 1 + 2*sum(decades==0) #if we have 0 included in our decades, add 2 to the number of ticks because we will tick at -1 and 1
  obj.Tick = rep(0, n_ticks)
  tick_index = 1
  previous_labeled_decade=NA
  
  for(k in 1:n_decades) {
    if (k == n_decades) {
      # write Tick for final decade
      obj.Tick[tick_index] = decades[k]
      # Fill any subsequent ticks
      # which may be labelled '' so that the Tick vector is
      # monotonically increasing;
      if (tick_index < n_ticks) {
        obj.Tick[(tick_index+1):length(obj.Tick)] = seq(obj.Tick[tick_index]+0.1, 
                                                        obj.Tick[tick_index]+0.2, 
                                                        length.out = n_ticks - tick_index)
      }
      break;
    }
    
    # write Tick for this decade in 9 increments if the ends of
    # the decades are powers of 10 increments if the right hand
    # end of the gap is 0 (i.e.10^{-inf})
    if (decades[k+1] == 0) {
      n_increments = 11
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else if (decades[k]==0) {
      n_increments = 9
      lhs = 1
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else {
      n_increments = 9
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    }
    #obj.Tick[tick_index:(tick_index+n_increments-1)] = trans.fun(seq(from = lhs, to = rhs, length.out = n_increments))
    obj.Tick[tick_index:(tick_index+n_increments-1)] = seq(from = lhs, to = rhs, length.out = n_increments)
    
    tick_index = tick_index + n_increments;
  }
  
  return(obj.Tick)
}