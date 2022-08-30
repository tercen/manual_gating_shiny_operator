library(ggplot2)
library("scattermore")
library(grid)
library('png')

library(flowCore)
library(flowWorkspace)

cols <-  colorRampPalette(c("#440154", "#3b528b", "#21918c", 
                            "#5ec962", "#fde725"))(256)

create_plot_1d <- function( data, trans ){
  # Create 1d density plot
  xt <- data[,1]
  imgfile <-  paste0(tempfile(), '.png')
  lab_names <- names(data)
  
  if( trans == 'linear'){
    #FROM HERE
    xlim <- c(min(xt), max(xt*1.1))
    if( xlim[2] < 1.5e5){
      xticks <- seq( xlim[1], xlim[2], by=1e4 )
      fac<-3
    }else{
      xticks <- seq( xlim[1], xlim[2], by=5e4 )
      fac<-4
    }

    xtick_labels <- unlist(lapply(xticks, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
    
    xticks <- unlist(lapply( xticks, function(x) nearest_factor10(x, FALSE, fac)))
    
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
  
  if( trans %in% c('biexp','logicle')){
    xt <- data[,1]
    
    if( trans == 'biexp'){
      trans_name <- 'biexponential'  
      custom_scale <- create_custom_biexp_scale(pos_decades = 5, 
                                                neg_decades = -1.5, 
                                                width_basis = -13)
    }else{
      trans_name <- 'logicle'  
      custom_scale <- create_custom_logicle_scale()
      
    }
    
    rd <- max(xt)-min(xt)
    xt <- transform_x( xt, custom_scale)
    
    breaks.x <- seq(min(xt), max(xt), by=rd/5 )
    breaks.x.t <-  break_transform(breaks = breaks.x, 
                                 transformation = trans_name)
    
    # Data range is too narrow to sensibly display at the log scale if there is
    # 1 or 0 ticks
    fac <- 5
    mfac <- max(xt)
    while( mfac < 5 * 10^fac && fac > 2){ 
      fac <- fac - 1
    }
    fac <- fac - 1
    
    labs_x <- unlist(lapply(breaks.x, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
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
                         # trans = custom_scale,
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
  

  ggsave(imgfile, units='px', width=600, height=600, p )
  lims <- get_axis_plot_lims(imgfile)
  
  return(list(imgfile, range_x, range_y, lims[[1]], lims[[2]], breaks_x, breaks_x.rel, breaks_y, breaks_y.rel ))
}





create_plot_2d <- function( data, trans  ){
  lab_names <- names(data)

  if( trans == 'linear'){
    x <- densCols(unlist(data[,1]), unlist(data[,2]), colramp=colorRampPalette(c("black", "white")),
                  nbin=256)
    dens <- col2rgb(x)[1,] + 1L

    colors <- cols[dens]


    xlim <- c(min(data[,1]), max(data[,1]*1.1))
    ylim <- c(min(data[,2]), max(data[,2]*1.1))

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

    # paste0(tempfile(), '.png') #
    imgfile <-   paste0(tempfile(), '.png') #'/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
    p <- ggplot() +
      geom_scattermost(
        as.matrix(data),
        pointsize=1.5,
        col=colors,
        pixels=c(600,600)) +
      labs(x=lab_names[1], y=lab_names[2])  +
      scale_y_continuous(breaks = yticks,
                         expand=c(0,0),
                         labels = ytick_labels) +
      scale_x_continuous(breaks = xticks,
                         expand=c(0,0),
                         labels = xtick_labels) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
            axis.line.x=element_line(color="#07070F" ),
            axis.line.y=element_line(color="#07070F" ),
            text = element_text(size=6)) +
      annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)


    # browser()
    pb <- ggplot_build(p)
    # range_x <- pb$layout$panel_params[[1]]$x.range
    # range_y <- pb$layout$panel_params[[1]]$y.range
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

  if( trans %in% c('biexp','logicle')){
    t_data <- data
    colnames(t_data)    <- c('.x','.y')

    if( trans == 'biexp'){
      trans_name <- 'biexponential'
      custom_scale <- create_custom_biexp_scale(pos_decades = 5,
                                                neg_decades = -1.5,
                                                width_basis = -13)
    }else{
      trans_name <- 'logicle'
      custom_scale <- create_custom_logicle_scale()

    }

    b_data <- transform_xy( t_data, custom_scale)
    rd.x <- max(b_data$.x)-min(b_data$.x)
    rd.y <- max(b_data$.y)-min(b_data$.y)
    breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd.x/5 )
    breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd.y/5 )

    # Density
    x <- densCols(unlist(b_data[,1]), unlist(b_data[,2]), colramp=colorRampPalette(c("black", "white")),
                  nbin=256 )
    dens <- col2rgb(x)[1,] + 1L
    colors <- cols[dens]

    breaks.x.t <-  break_transform(breaks = breaks.x,
                                 transformation = trans_name)
    breaks.y.t <-  break_transform(breaks = breaks.y,
                                 transformation = trans_name)

    
    # Data range is too narrow to sensibly display at the log scale if there is
    # 1 or 0 ticks
    fac <- 5
    mfac <- max(b_data$.x)
    while( mfac < 5 * 10^fac && fac > 2){ 
      fac <- fac - 1
    }
    fac <- fac - 1

    labs_x <- unlist(lapply(breaks.x, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
    if( sum(breaks.x.t >= min(b_data$.x) & breaks.x.t <= max(b_data$.x)) > 10 ){
      breaks.x <- breaks.x.t
      labs_x <- custom_tick_labels(breaks.x)
    }
    
    fac <- 5
    mfac <- max(b_data$.y)
    while( mfac < 5 * 10^fac && fac > 2){ 
      fac <- fac - 1
    }
    fac <- fac - 1

    labs_y <- unlist(lapply(breaks.y, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
    if( sum(breaks.y.t >= min(b_data$.y) & breaks.y.t <= max(b_data$.y)) > 10 ){
      breaks.y <- breaks.y.t
      labs_y <- custom_tick_labels(breaks.y)
    }
    
    imgfile <-  paste0(tempfile(), '.png')
    # browser()
    p <- ggplot() +
      scale_x_continuous(limits = c( min(b_data$.x), max(b_data$.x) ),
                         breaks = breaks.x,
                         expand=c(0,0),
                         # trans = custom_scale,
                         labels = labs_x,
                         sec.axis = dup_axis(labels=c())) +
      scale_y_continuous(limits = c( min(b_data$.y), max(b_data$.y) ),
                         breaks = breaks.y,
                         expand=c(0,0),
                         # trans = custom_scale,
                         labels = labs_y,
                         sec.axis = dup_axis(labels = c())) +
      geom_scattermore(
        data=b_data,
        mapping=aes(x=.x, y=.y),
        pointsize=1.5,
        col=colors,
        pixels=c(600,600)) +
      labs(x=lab_names[1], y=lab_names[2])  +
      theme(panel.background = element_rect(fill = 'white', colour = 'white'),
            axis.line.x=element_line(color="#07070F" ),
            axis.line.y=element_line(color="#07070F" ),
            text = element_text(size=8),
            axis.ticks.x.top = element_blank(),
            axis.title.x.top = element_blank(),
            axis.ticks.y.right = element_blank(),
            axis.title.y.right = element_blank())

    p<- set_biexp_ticks(p, breaks.x)

  
    pb <- ggplot_build(p)
  
    range_x <- pb$layout$panel_params[[1]]$x.sec$get_limits()
    range_y <- pb$layout$panel_params[[1]]$y.sec$get_limits()

    breaks_x <- append(append( range_x[1], pb$layout$panel_params[[1]]$x.sec$break_info$major_source_user ), range_x[2])
    breaks_x.rel <-append(append(0, pb$layout$panel_params[[1]]$x.sec$break_info$major), 1)

    breaks_y <- append(append( range_y[1], pb$layout$panel_params[[1]]$y.sec$break_info$major_source_user ), range_y[2])
    breaks_y.rel <-append(append(0, pb$layout$panel_params[[1]]$y.sec$break_info$major), 1)
  }

  ggsave(imgfile, units='px', width=600, height=600, p )


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

# if( trans == 'linear'){
#   # Density
#   x <- densCols(unlist(data[,1]), unlist(data[,2]), colramp=colorRampPalette(c("black", "white")),
#                 nbin=256)
#   dens <- col2rgb(x)[1,] + 1L
#   
#   colors <- cols[dens]
#   
#   
#   xlim <- c(min(data[,1]), max(data[,1]*1.1))
#   ylim <- c(min(data[,2]), max(data[,2]*1.1))
#   
#   if( xlim[2] < 1.5e5){
#     xticks <- seq( xlim[1], xlim[2], by=1e4 )
#     fac<-3
#   }else{
#     xticks <- seq( xlim[1], xlim[2], by=5e4 )
#     fac<-4
#   }
#   
#   
#   if( ylim[2] < 1.5e5){
#     yticks <- seq( ylim[1], ylim[2], by=1e4 )
#     fac<-3
#   }else{
#     yticks <- seq( ylim[1], ylim[2], by=5e4 )
#     fac<-4
#   }
#   
#   
#   xtick_labels <- unlist(lapply(xticks, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
#   ytick_labels <- unlist(lapply(yticks, function(x) nearest_factor10(x, label = TRUE, factor=fac)))
#   
#   xticks <- unlist(lapply( xticks, function(x) nearest_factor10(x, FALSE, fac)))
#   yticks <- unlist(lapply(yticks, function(x) nearest_factor10(x, label = FALSE, factor=fac)))
#   
#   imgfile <-  paste0(tempfile(), '.png') # '/home/rstudio/projects/manual_gating_shiny_operator/scatter.jpeg'
#   p <- ggplot() +
#     geom_scattermost(
#       as.matrix(data),
#       pointsize=1.5,
#       col=colors, 
#       pixels=c(600,600)) +
#     labs(x=lab_names[1], y=lab_names[2])  +
#     scale_y_continuous(breaks = yticks, labels = ytick_labels) +
#     scale_x_continuous(breaks = xticks, labels = xtick_labels) +
#     theme(panel.background = element_rect(fill = 'white', colour = 'white'),
#           axis.line.x=element_line(color="#07070F" ),
#           axis.line.y=element_line(color="#07070F" ),
#           panel.grid = element_line(color = "#AAAAAA",
#                                     size = 0.1,
#                                     linetype = 2),
#           text = element_text(size=8)) + 
#     annotate(geom = 'segment', y = Inf, yend = Inf, color ="#07070F", x = -Inf, xend = Inf, size = 1) +
#     annotate(geom = 'segment', y = -Inf, yend = Inf, color = "#07070F", x = Inf, xend = Inf, size = 1)
#   
#   
#   # browser()
#   pb <- ggplot_build(p)
#   image$range_x <- pb$layout$panel_params[[1]]$x.range
#   image$range_y <- pb$layout$panel_params[[1]]$y.range
#   
# }
# 
# if( trans %in% c('biexp','logicle')){
#   t_data <- data
#   colnames(t_data)    <- c('.x','.y')
#   
#   if( trans == 'biexp'){
#     trans_name <- 'biexponential'  
#     custom_scale <- create_custom_biexp_scale(pos_decades = 5, 
#                                               neg_decades = -1.5, 
#                                               width_basis = -13)
#   }else{
#     trans_name <- 'logicle'  
#     custom_scale <- create_custom_logicle_scale()
#     
#   }
#   
#   b_data <- transform_xy( t_data, custom_scale)
#   rd.x <- max(t_data$.x)-min(t_data$.x)
#   rd.y <- max(t_data$.y)-min(t_data$.y)
#   breaks.x <- seq(min(b_data$.x), max(b_data$.x), by=rd.x/5 )
#   breaks.y <- seq(min(b_data$.y), max(b_data$.y), by=rd.y/5 )
#   
#   # Density
#   x <- densCols(unlist(b_data[,1]), unlist(b_data[,2]), colramp=colorRampPalette(c("black", "white")),
#                 nbin=256 )
#   dens <- col2rgb(x)[1,] + 1L
#   colors <- cols[dens]
#   
#   breaks.x <-  break_transform(breaks = breaks.x, 
#                                transformation = trans_name)
#   breaks.y <-  break_transform(breaks = breaks.y, 
#                                transformation = trans_name)
#   
#   imgfile <-  paste0(tempfile(), '.png')
#   
#   p <- ggplot() +
#     scale_x_continuous(limits = c( min(b_data$.x), max(b_data$.x) ),
#                        breaks = breaks.x,
#                        trans = custom_scale,
#                        labels = custom_tick_labels(breaks.x),
#                        sec.axis = dup_axis(labels=c())) +
#     scale_y_continuous(limits = c( min(b_data$.y), max(b_data$.y) ),
#                        breaks = breaks.y,
#                        trans = custom_scale,
#                        labels = custom_tick_labels(breaks.y),
#                        sec.axis = dup_axis(labels = c())) +
#     geom_scattermore(
#       data=t_data,
#       mapping=aes(x=.x, y=.y),
#       pointsize=1.5,
#       col=colors,
#       pixels=c(600,600)) +
#     labs(x=lab_names[1], y=lab_names[2])  +
#     theme(panel.background = element_rect(fill = 'white', colour = 'white'),
#           axis.line.x=element_line(color="#07070F" ),
#           axis.line.y=element_line(color="#07070F" ),
#           text = element_text(size=8),
#           axis.ticks.x.top = element_blank(),
#           axis.title.x.top = element_blank(),
#           axis.ticks.y.right = element_blank(),
#           axis.title.y.right = element_blank()) 
#   
#   p<- set_biexp_ticks(p, breaks.x)
#   
#   pb <- ggplot_build(p)
#   
#   ad <- data.frame(.x = pb$layout$panel_params[[1]]$x.range,
#                    .y = pb$layout$panel_params[[1]]$y.range)
#   br <- transform_xy( ad, custom_scale, df_range = b_data)
#   
#   image$range_x <- br$.x
#   image$range_y <- br$.y
#   
#   
# }
# 
# 
# ggsave(imgfile, units='in', width=3, height=3, p ) 
# 
# }else{
#   session$sendCustomMessage("setViewOnly", runif(1))
# }
# 
# 
# 
# 
nearest_factor10 <- function( num, label=TRUE, factor=4){
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

transform_x <- function( x, custom_scale, df_range=NULL){
 
  
  if( is.null(df_range)){
    df_range <- x
  }
  xt <- custom_scale$transform(x = x)
  # xtn <- (xt - min(xt))/(max(xt)-min(xt))
  # rd <- max(df_range)-min(df_range)
  
  # return( (xtn * rd) + min(df_range) )
  return( xt )
}

transform_xy <- function( t_data, custom_scale, df_range=NULL){
  b_data <- t_data
  if( is.null(df_range)){
    df_range <- t_data
  }
  # NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
  xt <- custom_scale$transform(x = t_data$.x  )
  # xtn <- (xt - min(xt))/(max(xt)-min(xt))
  # rd <- max(df_range$.x)-min(df_range$.x)
  b_data[,1] <- xt #(xtn * rd) + min(df_range$.x)
  
  yt <- custom_scale$transform(x = t_data$.y  )
  # ytn <- (yt - min(yt))/(max(yt)-min(yt))
  # rd <- max(df_range$.y)-min(df_range$.y)
  b_data[,2] <- yt #(ytn * rd)+min(df_range$.y)
  
  return(b_data)
  
}
create_custom_logicle_scale <- function(w=0.5, t=262144, m=4.5, a=0.1) {
  
  custom_logicle_trans <- logicleTransform(w = w,
                                           t = t,
                                           m = m,
                                           a = a)
  
  custom_logicle_inv_trans <- inverseLogicleTransform(trans = custom_logicle_trans)
  
  custom_logicle_scale <- scales::trans_new(name = 'custom logicle',
                                            transform = custom_logicle_trans,
                                            inverse = custom_logicle_inv_trans)
  
  return(custom_logicle_scale)
}



create_custom_biexp_scale <- function(pos_decades, neg_decades, width_basis) {
  custom_biexp_trans <- flowjo_biexp(pos = pos_decades,
                                     neg = neg_decades, maxValue = 260000,
                                     widthBasis = width_basis)
  custom_biexp_inv_trans <- flowjo_biexp(pos = pos_decades, 
                                         neg = neg_decades, 
                                         widthBasis = width_basis,  maxValue = 260000,
                                         inverse = TRUE)
  
  custom_biexp_scale <- scales::trans_new(name = 'custom biexponential',
                                          transform = custom_biexp_trans,
                                          inverse = custom_biexp_inv_trans)
  
  return(custom_biexp_scale)
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
  if ((transformation == "biexponential") || (transformation == "logicle")) {
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

theme_fcs <- function() {
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1),
    axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = .3, unit = "cm"))
  )
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