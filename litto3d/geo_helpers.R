library(dplyr)
library(raster)
library(purrr)
library(plotly)

all.asc.5 <-
  rev(list.files(pattern = ".*FRA.*_MNT5_.*\\.asc", recursive = TRUE))
all.asc.1 <-
  rev(list.files(pattern = ".*FRA.*_MNT_.*\\.asc", recursive = TRUE))


read.file.center.coord <- function(file) {
  coordinates <-
    read.csv(file,
             skip = 1,
             nrows = 2,
             sep = "") %>% dplyr::select(2) %>% t(.) %>% as.data.frame(.)
  colnames(coordinates) <- c('x', 'y')
  
  name.file <- tail(strsplit(file, '/')[[1]], 1)
  
  coordinates$name <- name.file
  coordinates$path <- file
  
  return (coordinates)
  
}

select.area <- function(resolution, lon, lat, factor) {
  if (resolution == 1) {
    all.asc <- all.asc.1
  }
  else{
    all.asc <- all.asc.5
  }
  df <- all.asc %>% map_dfr(read.file.center.coord) %>% arrange(x, y)
  scale <- factor * 1000
  df <-
    df %>% filter((x > lon - scale) &
                    (x < lon + scale) & (y > lat - scale) & (y < lat + scale))
  
  df$resolution <- resolution
  return(df)
}

set.coordinates <- function( x, y, name, path,resolution) {
  
  df <- read.csv(
    path,
    skip = 6,
    
    header = F,
    nrows = -1,
    sep = ""
  )
  x.cols <- seq(x - 500, x + 500 - 1, resolution)
  y.rows <- rev(seq(y - 500, y + 500 - 1, resolution))
  
  colnames(df) <- x.cols
  df$y <- y.rows
  df <- df %>% arrange(desc(y))
  return (df)
  
}


stack.lon.tiles <- function(df.lon.data){
  df <-pmap_dfr(df.lon.data,set.coordinates)
  return(df)
  
}


select.lon.index <- function(lon,data){
  df.per.lon <- data$metadata %>% filter(x==lon) 
  
  return(df.per.lon)
}

merge.all.lon.stacks <- function(data){
  
  
  df.all <- data.frame(y=numeric())
 
  for (longitude in data$all.lon){
    
    df.lon <- stack.lon.tiles(select.lon.index(longitude,data))
    df.all <- merge(df.all,df.lon,by='y',all=T)
    
  }
  
 
  colnames(df.all)<- make.names(colnames(df.all))
  
  return (df.all)
}

rasterize.df <- function(data, factor=1){
  mat <- data$df.all %>% dplyr::select(-y) %>% as.matrix(.)
  
  mat[mat ==head(unique(sort(mat)))[1]] <- head(unique(sort(mat)))[2]
  print(min(mat))
  
  rasterized <- raster(mat)
  
  rasterized.agg <- aggregate(rasterized, fact=factor)
  
  writeRaster(rasterized.agg,'test.tif',overwrite=T)
  
  return (rasterized.agg )
  
}

plot.3d <- function(data){
  
  area.width.meter <- max(data$metadata$x)-min(data$metadata$x)+1000
  
  
  
  area.size <- extent(data$raster)
  print(area.size)
  
  mat <- raster::as.matrix(data$raster)
  
  range.z <- (max(mat)-min(mat))
  
  z.zoom.factor <-  area.width.meter /1000 /2
  #area.size <- extent(data$raster)/100
  print(paste0("ratio z zoom factor : ",z.zoom.factor))
  print(z.zoom.factor)
  z.min.plot <- min(mat)* z.zoom.factor
  z.max.plot <- max(mat)*z.zoom.factor
  
  print(z.min.plot)
  print(z.max.plot)
  #mat <- mat*area.width.meter/range.z
  
  #mat1 <- apply(mat.agg, 1, rev)
  axz <- list(
    nticks = 4,
    range = c(z.min.plot,z.max.plot)
  )
  
  ax <- list(title = "",
             zeroline = FALSE,
             showline = FALSE,
             showticklabels = FALSE,
             showgrid = FALSE
  )
  
  
  p <- plot_ly(z = ~mat) %>% add_surface()  %>%  layout(scene = list(xaxis=ax,yaxis=ax,zaxis=axz))
  return(p)
  
}
