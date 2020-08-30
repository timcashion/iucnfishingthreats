

####Static values####
crs_string <- "+init=epsg:4326"
#api_version <- paste("version_", jsonlite::fromJSON(httr::content(httr::GET(url = "http://apiv3.iucnredlist.org/api/v3/version"), "text"), flatten=TRUE)[[1]], sep="")
api_version <- "version_2019-2"

####Functions####
#ipak: https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#non-min-min and non-max-max
#Takes the penultimate minimum/maximum value instead of the actual minimum/maximum. Helping for removing -Inf/+Inf values or 0 values when not wanted. 
non_min_min <- function(x, na.rm=T){
  if(na.rm){
    if(length(x)>2){
      min_value <- min(x)
      x <- x[x!=min_value]
      return(min(x))
    } else {
      return(min(x))
    }
  }
}

non_max_max <- function(x=NA, na.rm=TRUE){
  #Returns value closest to max value that isn't the max if there is more than two values 
  if(length(na.omit(x))==0){
    return(max(na.omit(x), 0))
  } else if(length(unique(x))>2){
    max_value <- max(x)
    x <- x[x!=max_value]
    return(max(na.omit(x)))
  } else {
    return(max(na.omit(x)))
  }
}


#cat_msg from common_fxns.R from https://github.com/oharac/spp_risk_dists 
cat_msg <- function(x, ...) {
  if(is.null(knitr:::.knitEnv$input.dir)) {
    ### not in knitr environment, so use cat()
    cat(x, ..., '\n')
  } else {
    ### in knitr env, so use message()
    message(x, ...)
  }
  return(invisible(NULL))
}


####Spatial functions####
#Original convert to centroid function:
convert_to_centroid <- function(df){
  df <- df %>% 
    mutate(lat = (lat_bin/100) + 0.01,
           lon = (lon_bin/100) + 0.01)
  return(df)
}

convert_to_new_grid <- function(df, res=NA){
  df <- df %>% 
    mutate(lat = round((lat + res) * 2) / 2 - res,
           lon = round((lon + res) * 2) / 2 - res)
  return(df)
}

convert_to_sau <- function(df, cell_res=0.5){
  res <- cell_res/2
  df <- df %>% 
    mutate(lat = round((lat + res) * 2) / 2 - res,
           lon = round((lon + res) * 2) / 2 - res) 
  return(df)
}

#GFW Paper uses following formula to align to 0.5 degree grid:
gfw_to_new_grid <- function(df=NA, res=NA){
  df <- df %>% 
    dplyr::mutate(
      lon = (floor((lon_bin/100)/res) * res) + res/2, 
      lat = (floor((lat_bin/100)/res) * res) + res/2
      )
  return(df)
}

summarize_gfw <- function(file, cell_res="all") {
  dat <- data.table::fread(file.path(effort_dir,file))
  #dat <- read_csv(file.path(effort_dir,file), col_types = "Dddccddd")
  
  #df <- convert_to_centroid(dat)
  if(cell_res=="all"){
    df <- dat
    df$sau_lon <- floor((df$lon)/0.5) * 0.5 #plyr::round_any(df$lon, 0.25, f=round) #Fit GFW grid to coarser SAU grid
    df$sau_lat<- floor((df$lat)/0.5) * 0.5 #plyr::round_any(df$lat, 0.25, f=round) #Fit GFW grid to coarser SAU grid 
    df <- df %>%
      group_by(date, geartype, lat_bin, lon_bin, lat, lon, sau_lon, sau_lat) %>% 
      summarize(vessel_hours = sum(vessel_hours),
                fishing_hours = sum(fishing_hours),
                mmsi_present = sum(mmsi_present))
  }else if(cell_res==0.5){
    df <- convert_to_centroid(dat)
    df <- convert_to_sau(df)
    df <- df %>%
      group_by(date, lat, lon, geartype) %>% 
      summarize(vessel_hours = sum(vessel_hours),
                fishing_hours = sum(fishing_hours))
  } else {
    df <- dat
    df <- gfw_to_new_grid(df=df, res=cell_res)
    df <- df %>%
      group_by(date, lat, lon, geartype) %>% 
      summarize(vessel_hours = sum(vessel_hours),
                fishing_hours = sum(fishing_hours))
  }
  return(df)
}


#clip_to_globe from common_fxns.R from https://github.com/oharac/spp_risk_dists 
clip_to_globe <- function(x) {
  ### for SF features, transform to wgs84, clip to +-180 and +-90
  epsg <- st_crs(x)$epsg
  if(epsg != 4326 | is.na(epsg)) {
    message('Original EPSG = ', epsg, '; Proj4 = ', st_crs(x)$proj4string,
            '\n...converting to EPSG:4326 WGS84 for clipping')
    x <- st_transform(x, 4326)
  }
  x_bbox <- st_bbox(x)
  if(x_bbox$xmin < -180 | x_bbox$xmax > +180 |
     x_bbox$ymin <  -90 | x_bbox$ymax >  +90) {
    message('Some bounds outside +-180 and +-90 - clipping')
    z <- st_crop(x, y = c('xmin' = -180,
                          'ymin' =  -90,
                          'xmax' = +180,
                          'ymax' =  +90)) %>%
      st_cast('MULTIPOLYGON')
  } else {
    message('All bounds OK, no clipping necessary')
    z <- x
  }
  return(z)
}

#valid_check from common_fxns.R from https://github.com/oharac/spp_risk_dists 
valid_check <- function(spp_shp) {
  valid <- st_is_valid(spp_shp)
  ### can return a vector if multiple polygons with same ID
  if(any(!valid)) {
    cat_msg('Found invalid geometries')
    
    bbox_shp <- st_bbox(spp_shp)
    if(bbox_shp$xmin < -180 | bbox_shp$xmax > 180) {
      cat_msg('Bounding box outside +/- 180; buffering with dist = 0')
      ### check area before and after buffering to make sure no loss
      area_pre  <- st_area(spp_shp) %>% as.numeric() / 1e6
      spp_shp   <- st_buffer(spp_shp, dist = 0) %>%
        st_cast('MULTIPOLYGON')
      area_post <- st_area(spp_shp) %>% as.numeric() / 1e6
      
      area_check <- all.equal(area_pre, area_post)
      area_ratio <- max(sum(area_pre) / sum(area_post),
                        sum(area_post) / sum(area_pre))
      
      
      ### error check to make sure the buffer didn't lose polygons
      if(area_check == FALSE | 
         (class(area_check) != 'logical' & area_ratio > 1.001)) {
        ### use all.equal() for near equality, and for comparing all 
        ### elements in case of a vector.  If a difference, choose an arbitrary
        ### threshold for "close enough".
        cat_msg('Error: area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; not equal!')
        stop('Area_pre and area_post not equal!')
      } else {
        cat_msg('Area check good!  area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; all equal!')
      }
    } else {
      cat_msg('bbox not exceeded; no need to fix polygon with buffer')
    }
  }
  return(spp_shp)
}

