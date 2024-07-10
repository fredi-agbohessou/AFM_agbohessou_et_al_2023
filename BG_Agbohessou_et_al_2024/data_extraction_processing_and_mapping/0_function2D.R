# different functions for STEP
# author: y. agbohessou
# Date: 16/05/2021

# 2D STEP plot for Sahel------------------------------
map_sahel= function (data, var, 
                     title = NULL,
                     subtitle = NULL,
                     shape,col.reg = (rev(brewer.pal(10,"RdBu"))), 
                     label = FALSE){
  
  lib_names=c("raster","ggplot2","unikn","mapview",
              "gridExtra","rgdal","fields","extrafont",
              "RColorBrewer","ncdf4","rasterVis","grid",
              "rcartocolor","pacman","purrr","moments","tictoc", 
              "sf", "sp", "exactextractr","readxl", "rgeos",
              "snow","future.apply","parallel","ggraph")
  invisible(suppressMessages
            (suppressWarnings
              (lapply
                (lib_names,require, character.only = T))))
  
  #loadfonts(device = "win")   # for "extrafont" pkg
   
  colnames(data) = c("Long", "Lat","varname") 
  
  # import senegal shapefile
  shape = shape
  
  #convert data to raster
  data_raster <- rasterFromXYZ(data)  #Convert first two columns as lon-lat and third as value      
  
  #crop the raster 
  data_raster_crop = crop(data_raster, extent(shape))  
  
  #Mask rasters using senegal polygon
  data_raster_mask_sen = raster::mask(data_raster_crop,shape) 
  
  # ReTransformation of raster to x-y-z format. 
  data1 = as.data.frame(as(data_raster_mask_sen, "SpatialPixelsDataFrame")) # Spatial dataframe of SM data
  colnames(data1) = c("varname","Long", "Lat") 
  data1$coord = paste0(round(data1$Long,1),"_",round(data1$Lat,1))
  
  
  data_raster2 <- rasterFromXYZ(data)  #Convert first two columns as lon-lat and third as value
  data_raster_crop2 = crop(data_raster2, extent(consen))  
  data_raster_mask_sen2 = raster::mask(data_raster_crop2,consen) 
  data2 = as.data.frame(as(data_raster_mask_sen2, "SpatialPixelsDataFrame")) # Spatial dataframe of SM data
  colnames(data2) = c("varname","Long", "Lat") 
  data2$coord = paste0(round(data2$Long,1),"_",round(data2$Lat,1))
  
  data3 = merge(data2,data1,by="coord",all=TRUE)
  data3 = data3[,c("varname.y","Long.x","Lat.x")]
  colnames(data3) = c("varname","Long", "Lat")
  
  sm_shapeegal=ggplot() +                               # Initialize the plot
    geom_tile(data=data3,                    # Add soil moisture data
              aes(x=Long, y=Lat, fill=varname))+       # Provide X, Y and Z data
    scale_fill_gradientn(colors=col.reg,           # Use user-defined colormap
                         breaks = seq(min(data3$varname),max(data3$varname), length.out = 5),
                         labels = seq(min(data3$varname),max(data3$varname), length.out = 5) %>% round(1),
                         name = var, 
                         na.value = "gray93"
    )+                            
    scale_x_continuous(breaks=seq(-17,20,5), 
                       labels = paste0(seq(-17, 20, 5), "°"),
                       expand = c(0.01, 0.01)
                       )+
    scale_y_continuous(breaks=seq(13,18,1), 
                       labels = paste0(seq(13, 18, 1), "°")
    )+labs(title=title,subtitle = subtitle,x="Longitude",y="Latitude")+
    # coord_fixed(xlim = c(-17.5,-11.5),     # Add extent for CONUS
    #             ylim = c(12.5,16.6))+ 
    theme_minimal()+
    theme(#axis.title=element_text(size=8),
      panel.grid.major = element_line(color = "grey80", size = 0.25,linetype="dotted"),
      panel.grid.minor = element_blank(),
      #panel.grid.minor = element_line(linetype="dotted"),
      text = element_text(family="Candara"),
      plot.title = element_text(hjust = 0.5,size=17,color = "grey25"),
      plot.subtitle = element_text(hjust = 0,size=12,color = "grey25"),
      legend.text=element_text(size=14.5,face = "bold",hjust = 0.5),
      axis.text.x=element_text(size = 12),
      axis.text.y=element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.key.height = unit(0.6, "cm"),# unit(0.8, "cm"),
      legend.key.width = unit(2,"cm"),#unit(3,"cm"),
      # legend.background = element_rect(),
      legend.title=element_text(size=11,color = "grey25"),#17
      legend.position ="bottom",# c(0.15,0.40), #c(1,1)
      # legend.justification = c(1,1),
      legend.direction = "horizontal",
      # legend.box.just = "center",
      # legend.title.align = 0.5,
      plot.background = element_rect(fill = "grey100",
                                     color = NA),
      panel.background = element_rect(fill = "grey100",
                                      color = NA),
      legend.background = element_rect(fill = "grey100",
                                       color = NA)
    )+ 
    
    # Add senegal region borders   
    geom_path(data=shape,                  
              aes(x=long, y=lat, group=group),
              color='black',size=.2) #+
    #theme(plot.title = element_text(size=9,face = "italic"))
  
  #add tower pic
  img = png::readPNG("3-spatialisation/tower.png")
  
  sm_shapeegal = sm_shapeegal + 
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -15.80000, xmax = -15.10000,
                      ymin = 15.20000, ymax = 16.00000)+
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -16.85370, xmax = -16.10000,
                      ymin = 14.29580, ymax = 15.10000) 
  
  if (label == FALSE){
    return(sm_shapeegal)
  }else{
    
    #tidy up the data, creating a dataframe using NAME_1 as the region
    shape_df=broom::tidy(shape)
    # Recover row name 
    temp_df <- data.frame(shape@data$ADM0_NAME)
    names(temp_df) <- c("region")
    # Create and append "id"
    temp_df$id <- seq(0,nrow(temp_df)-1)
    shape_df <- plyr::join(shape_df, temp_df, by="id")
    shape_df$id<-shape_df$region
    lapply(shape_df,class)
    
    # aggregate region names
    cnames = aggregate(cbind(long, lat) ~ id, data=shape_df, FUN=mean)

    #plot with region names
    sm_shapeegal = sm_shapeegal + 
      #geom_text(data=cnames,aes(x=long,y=lat,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Mali"|id=="Niger"|id=="Burkina Faso"),aes(x=long,y=lat,label=id), size=4,family="Candara")+
      geom_text(data=subset(cnames,id=="Gambia"),aes(x=long,y=lat,label=id), size=3,family="Candara")+
      geom_text(data=subset(cnames,id=="Nigeria"),aes(x=long-6,y=lat,label=id), size=4,family="Candara")+
      geom_text(data=subset(cnames,id=="Mauritania"),aes(x=long+3.5,y=lat+1,label=id), size=4,family="Candara")+
      geom_text(data=subset(cnames,id=="Chad"|id=="Senegal"),aes(x=long+2.5,y=lat,label=id), size=4,family="Candara")+
      annotate("text",x= -15.58,y=15.27,label="Dahra",size=1.5,family="Candara")+
      annotate("text",x= -16.59,y=14.42,label="Niakhar",size=1.5,family="Candara")
    return(sm_shapeegal)
  }
  
}

# plot all years and means--------------------

multi_map_sahel = function (data, var,shape,col.reg = (rev(brewer.pal(10,"RdBu"))), label = FALSE){
  
  lib_names=c("raster","sp","gstat","readr","terra","rasterVis","png",
              "fields","interp","mgcv","automap", "patchwork", "viridis","RColorBrewer",
              "tidyr","rstep",'maptools',"tidyverse","grid")
  invisible(suppressMessages
            (suppressWarnings
              (lapply
                (lib_names,require, character.only = T))))
  
  
  # create a dataframe
  coords <- xyFromCell(data, seq_len(ncell(data)))
  data <- stack(as.data.frame(getValues(data)))
  names(data) <- c('value', 'variable')
  
  data <- cbind(coords, data)
  plot =ggplot(data) + 
    geom_tile(aes(x, y, fill = value)) +
    facet_wrap(~ variable,ncol=2) +
    scale_fill_gradientn(colours = col.reg,
                         breaks = seq(min(data$value,na.rm = T),max(data$value,na.rm = T), length.out = 5),
                         labels = seq(min(data$value,na.rm = T),max(data$value,na.rm = T), length.out = 5) %>% round(1),
                         name = var,
                         na.value = "transparent") +
    scale_x_continuous(breaks=seq(-17,20,5), 
                       labels = paste0(seq(-17, 20, 5), "°"),
                       expand = c(0.01, 0.01)
    )+
    scale_y_continuous(breaks=seq(13,18,1), 
                       labels = paste0(seq(13, 18, 1), "°")
    )+
    xlab("")+    #Longitude        
    ylab("")+# Latitude
    coord_equal()+
    theme_minimal()+
    theme(panel.grid.minor = element_line(linetype="dotted"),
          text = element_text(family="Candara"),
          plot.title = element_text(hjust = 0.5,size=17,face = "bold"),
          legend.text=element_text(size=14.5,face = "bold",hjust = 0.5),
          strip.text = element_text(size = 14.5, margin = margin()),
          axis.text.x=element_text(size = 15),
          axis.text.y=element_text(size = 15),
          axis.title = element_text(size = 12),
          legend.key.height = unit(0.8, "cm"),
          legend.key.width = unit(3,"cm"),
          legend.title=element_text(size=16),
          legend.position ="bottom",
          legend.direction = "horizontal"
    )
  
  plot = plot+
    # Add senegal region borders
    geom_path(data=shape,                  
              aes(x=long, y=lat, group=group),
              color='black',size=.2) 
  #theme(plot.title = element_text(size=9,face = "italic")) +
  
  #add tower pic
  img = png::readPNG("3-spatialisation/tower.png")
  
  plot = plot + 
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -15.80000, xmax = -15.10000,
                      ymin = 15.20000, ymax = 16.00000)+
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -16.85370, xmax = -16.10000,
                      ymin = 14.29580, ymax = 15.10000) 
  
  if (label == FALSE){
    return(plot)
  }else{
    #tidy up the data, creating a dataframe using NAME_1 as the region
    shape_df=broom::tidy(shape) # ,region="ADM0_NAME"
    # Recover row name 
    temp_df <- data.frame(shape@data$ADM0_NAME)
    names(temp_df) <- c("region")
    # Create and append "id"
    temp_df$id <- seq(0,nrow(temp_df)-1)
    shape_df <- plyr::join(shape_df, temp_df, by="id")
    shape_df$id<-shape_df$region
    lapply(shape_df,class)
    
    
    # aggregate region names
    cnames = aggregate(cbind(long, lat) ~ id, data=shape_df, FUN=mean)
    
    #plot with region names
    plot = plot + 
      #geom_text(data=cnames,aes(x=long,y=lat,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Gambia"|id=="Mali"|id=="Niger"|id=="Burkina Faso"),aes(x=long,y=lat,label=id),size=4,family="Candara")+
      geom_text(data=subset(cnames,id=="Nigeria"),aes(x=long-6,y=lat,label=id), size=3,family="Candara")+
      geom_text(data=subset(cnames,id=="Mauritania"),aes(x=long+3.5,y=lat+1,label=id), size=4,family="Candara")+
      geom_text(data=subset(cnames,id=="Chad"|id=="Senegal"),aes(x=long+2.5,y=lat,label=id),size=4,family="Candara")+
      annotate("text",x= -15.58,y=15.27,label="Dahra",size=1.5,family="Candara")+
      annotate("text",x= -16.59,y=14.42,label="Niakhar",size=1.5,family="Candara")
    return(plot)
  }
}

# plot animated map-----------------------------------------------
anim_map_sahel= function (stack, var, 
                         title = NULL,
                         shape,col.reg = (rev(brewer.pal(10,"RdBu"))), 
                         label = FALSE){
  
  data <- purrr::map_dfr(
    as.list(stack), 
    ~setNames(as.data.frame(as(., "SpatialPixelsDataFrame")), c('value', 'x', 'y')), 
    .id = 'year'
  )  
  sm_shapeegal=ggplot(data, aes(x = x, y = y)) +                               # Initialize the plot
    geom_raster(aes(fill = value)) +
    transition_manual(year) +
    scale_fill_gradientn(colors=col.reg,           # Use user-defined colormap
                         breaks = seq(min(data$value),max(data$value), length.out = 5),
                         labels = seq(min(data$value),max(data$value), length.out = 5) %>% round(1),
                         name = var, 
                         na.value = "gray82"
    )+                            
    scale_x_continuous(breaks=seq(-17,20,5), expand = c(0.01, 0.01))+
    xlab("")+    #Longitude        
    ylab("")+
    ggtitle(paste0(title," {current_frame}"))+
    theme_minimal()+
    theme(#axis.title=element_text(size=8),
      text = element_text(family="Comic Sans MS"),
      plot.title = element_text(hjust = 0.5,size=17,face = "bold"),
      legend.text=element_text(size=14.5,face = "bold",hjust = 0.5),
      axis.text.x=element_text(size = 15),
      axis.text.y=element_text(size = 15),
      legend.key.height = unit(0.8, "cm"),
      legend.key.width = unit(3,"cm"),
      # legend.background = element_rect(),
      legend.title=element_text(size=16),#17
      legend.position ="bottom",# c(0.15,0.40), #c(1,1)
      # legend.justification = c(1,1),
      legend.direction = "horizontal",
      # legend.box.just = "center",
      # legend.title.align = 0.5,
    )+ 
    
    # Add senegal region borders
    geom_path(data=consen,                  
              aes(x=long, y=lat, group=group),
              color='black',size=.6) +
    #theme(plot.title = element_text(size=9,face = "italic")) +
    
    geom_path(data=shape,                  
              aes(x=long, y=lat, group=group),
              color='black',size=.2) #+
  #theme(plot.title = element_text(size=9,face = "italic"))
  
  #add tower pic
  img = png::readPNG("3-spatialisation/tower.png")
  
  sm_shapeegal = sm_shapeegal + 
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -15.80000, xmax = -15.10000,
                      ymin = 15.20000, ymax = 16.00000)+
    annotation_custom(rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -16.85370, xmax = -16.10000,
                      ymin = 14.29580, ymax = 15.10000) 
  
  if (label == FALSE){
    return(sm_shapeegal)
  }else{
    
    #tidy up the data, creating a dataframe using NAME_1 as the region
    shape_df=broom::tidy(shape)
    # Recover row name 
    temp_df <- data.frame(shape@data$ADM0_NAME)
    names(temp_df) <- c("region")
    # Create and append "id"
    temp_df$id <- seq(0,nrow(temp_df)-1)
    shape_df <- plyr::join(shape_df, temp_df, by="id")
    shape_df$id<-shape_df$region
    lapply(shape_df,class)
    
    # aggregate region names
    cnames = aggregate(cbind(long, lat) ~ id, data=shape_df, FUN=mean)
    
    #plot with region names
    sm_shapeegal = sm_shapeegal + 
      #geom_text(data=cnames,aes(x=long,y=lat,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Mali"|id=="Niger"|id=="Burkina Faso"),aes(x=long,y=lat,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Gambia"),aes(x=long,y=lat,label=id), size=3)+
      geom_text(data=subset(cnames,id=="Nigeria"),aes(x=long-6,y=lat,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Mauritania"),aes(x=long+3.5,y=lat+1,label=id), size=4)+
      geom_text(data=subset(cnames,id=="Chad"|id=="Senegal"),aes(x=long+2.5,y=lat,label=id), size=4)+
      annotate("text",x= -15.58,y=15.27,label="Dahra",size=1.5)+
      annotate("text",x= -16.59,y=14.42,label="Niakhar",size=1.5)
    return(sm_shapeegal)
  }
  
}

# The function that produces the colour matrix
# based on scbrown86's script bivarRasterPlot.R on GitHub "https://gist.github.com/scbrown86/2779137a9378df7b60afd23e0c45c188?permalink_comment_id=3264288"

colmat <- function(nbreaks = 3, breakstyle = "quantile",
                   upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#ffe70f",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,labx=labx,
                   saveLeg = FALSE) {
  # - replace any tidyr, dplyr etc. functions with data.table #
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  if (breakstyle == "sd") {
    warning("SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"}
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nbreaks,
                                       style = breakstyle,
  )
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  ## need to convert this to data.table at some stage.
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>% 
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_tile() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      
      scale_x_continuous(breaks=seq(1,5,1), 
                         labels = labx)+
      scale_y_continuous(breaks=seq(1,5,1), 
                         labels = seq(20, 100, 20),
                         position = "right", sec.axis = sec_axis(~., labels = NULL))+
      theme_minimal() +
      theme(panel.grid.major = element_line(color = "grey80", size = 0.25),
            panel.grid.minor = element_blank(),
            text = element_text(family="Candara"),
            axis.text.x=element_text(size = 9),
            axis.text.y=element_text(size = 9),
            plot.background = element_rect(fill = "grey100",color = NA),
            panel.background = element_rect(fill = "grey100",color = NA),
            aspect.ratio = 1,
            axis.title = element_text(size = 9.5, colour = "black",hjust = 0.5,vjust = 1),
            axis.title.y = element_text(size = 12,angle = 90, hjust = 0.5)) +
      xlab(xlab) +
      ylab(ylab)
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  return(list(col.matrix=col.matrix,plot=p)) # return(col.matrix) and p
  
}

# Function to assign colour-codes to raster data
# As before, by default assign tercile breaks
bivariate.map <- function(rasterx, rastery, colourmatrix = col.matrix,
                          export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  # TO DO - replace raster with terra #
  require(raster)
  require(classInt)
  quanx <- getValues(rasterx)
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(tempx, classIntervals(quanx,
                                     n = attr(colourmatrix, "nbreaks"),
                                     style = attr(colourmatrix, "breakstyle"))$brks)
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- cut(quanx,
                                      breaks = brks,
                                      labels = 2:length(brks),
                                      include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  quany <- getValues(rastery)
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brksy <- with(tempy, classIntervals(quany,
                                      n = attr(colourmatrix, "nbreaks"),
                                      style = attr(colourmatrix, "breakstyle"))$brks)
  brksy[-1] <- brksy[-1] + seq_along(brksy[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- cut(quany,
                                      breaks = brksy,
                                      labels = 2:length(brksy),
                                      include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colourmatrix
  cn <- unique(colourmatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }

  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colourmatrix),
      t(col2rgb(as.vector(colourmatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}

#########plot bivariate map-------------
bivariate_map_sh = function(raster_x,
                            raster_y,
                            xlab_elem,
                            xlab_legend,
                            ylab_legend,
                            title,
                            subtitle){
  col.matrix <- colmat(nbreaks = 5, breakstyle = "quantile",
                       xlab = xlab_legend,#expression(atop(paste("Soil N"[2]*"O"),paste(" (kg N-N"[2]*"O ha"^"-2","yr"^"-1",")"))),
                       ylab = ylab_legend, #"% of SPS pixel", 
                       bottomleft =  "#E8F4F3", bottomright = "#9874A1",upperleft ="#FEF287", upperright ="#21908D",
                       saveLeg = FALSE, plotLeg = TRUE,
                       labx= xlab_elem# seq(0, 3.4,0.85) #0,max_x, max_x/4
  )
  col.matrixQ=col.matrix$col.matrix
  col.matrix_plot=col.matrix$plot
  # create the bivariate raster
  bivmapQ <- bivariate.map(rasterx = raster_x, 
                           rastery = raster_y,
                           export.colour.matrix = FALSE,
                           colourmatrix = col.matrixQ)
  # Convert to dataframe for plotting with ggplot
  bivMapDFQ <- setDT(as.data.frame(bivmapQ, xy = TRUE))
  colnames(bivMapDFQ)[3] <- "BivValue"
  bivMapDFQ <- melt(bivMapDFQ, id.vars = c("x", "y"),
                    measure.vars = "BivValue",
                    value.name = "bivVal",
                    variable.name = "Variable")
  # Make the map using ggplot
  map_q <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
    geom_raster(aes(fill = bivVal)) +
    scale_x_continuous(breaks=seq(-17,20,5), 
                       labels = paste0(seq(-17, 20, 5), "°"),
                       expand = c(0.01, 0.01))+
    scale_y_continuous(breaks=seq(13,18,1), 
                       labels = paste0(seq(13, 18, 1), "°"))+
    scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    #borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = c(-17.5,20), ylim = c(13,18)) +
    labs(title = title,
         subtitle = subtitle,
         x = "Longitude", y = "Latitude")+
    #ggtitle(title)+
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "grey80", size = 0.25,linetype="dotted"),
          plot.title = element_text(hjust = 0.5,size=17,color = "grey25"),
          plot.subtitle = element_text(hjust = 0,size=12,color = "grey25"),
          panel.grid.minor = element_blank(),
          text = element_text(family="Candara"),
          axis.text.x=element_text(size = 10),
          axis.text.y=element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.background = element_rect(fill = "grey100",
                                         color = NA),
          panel.background = element_rect(fill = "grey100",
                                          color = NA)
    )+
    # Add senegal region borders
    geom_path(data=consen,                  
              aes(x=long, y=lat, group=group),
              color='black',size=.2) 
  #theme(plot.title = element_text(size=9,face = "italic")) +
  #add tower pic
  img = png::readPNG("3-spatialisation/tower.png")
  map_q= map_q + 
    annotation_custom(grid::rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -15.80000, xmax = -15.10000,
                      ymin = 15.20000, ymax = 16.00000)+
    annotation_custom(grid::rasterGrob(img, width = 0.7, height = 0.5),
                      xmin = -16.85370, xmax = -16.10000,
                      ymin = 14.29580, ymax = 15.10000) 
  
  shape_df=broom::tidy(consen)
  # Recover row name 
  temp_df <- data.frame(consen@data$ADM0_NAME)
  names(temp_df) <- c("region")
  # Create and append "id"
  temp_df$id <- seq(0,nrow(temp_df)-1)
  shape_df <- plyr::join(shape_df, temp_df, by="id")
  shape_df$id<-shape_df$region
  lapply(shape_df,class)
  # aggregate region names
  cnames = aggregate(cbind(long, lat) ~ id, data=shape_df, FUN=mean)
  #plot with region names
  map_q = map_q + 
    #geom_text(data=cnames,aes(x=long,y=lat,label=id), size=4)+
    geom_text(data=subset(cnames,id=="Mali"|id=="Niger"|id=="Burkina Faso"),aes(x=long,y=lat,label=id), size=4,family="Candara")+
    geom_text(data=subset(cnames,id=="Gambia"),aes(x=long,y=lat,label=id), size=3,family="Candara")+
    geom_text(data=subset(cnames,id=="Nigeria"),aes(x=long-6,y=lat,label=id), size=4,family="Candara")+
    geom_text(data=subset(cnames,id=="Mauritania"),aes(x=long+3.5,y=lat+1,label=id), size=4,family="Candara")+
    geom_text(data=subset(cnames,id=="Chad"|id=="Senegal"),aes(x=long+2.5,y=lat,label=id), size=4,family="Candara")+
    annotate("text",x= -15.58,y=15.27,label="Dahra",size=1.5,family="Candara")+
    annotate("text",x= -16.59,y=14.42,label="Niakhar",size=1.5,family="Candara")
  t= (map_q+col.matrix_plot) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
    plot_layout(widths = c(8,1))
  return(list(t,map_q,col.matrix_plot))
  
}

####Other themes

theme_lat = theme_minimal()+
  theme(panel.grid.major = element_line(color = "grey80", size = 0.25,linetype="dotted"),
        panel.grid.minor = element_blank(),
        text = element_text(family="Candara"),
        axis.text.x=element_text(size = 9.5),
        axis.text.y=element_text(size = 12),
        axis.title = element_text(size = 9.5),
        plot.background = element_rect(fill = "grey100",
                                       color = NA),
        panel.background = element_rect(fill = "grey100",
                                        color = NA),
        legend.background = element_rect(fill = "grey100",
                                         color = NA)
        )

thm2 =theme_minimal()+
  theme(panel.grid.minor = element_line(linetype="dotted"),
        plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"),
        text = element_text(family="Candara"),
        axis.title = element_text(size = 8),
        axis.line = element_blank(),
        legend.title.align =0,
        legend.position = c(1.08,0.5),
        legend.title=element_text(size=7),
        legend.key.height = unit(0.58, "cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.text = element_text(size=6),
        axis.text = element_text(size=7,colour = "black")
       
  )
