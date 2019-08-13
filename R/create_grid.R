
source("./R/common_fxns.R")



packages <- c("tidyverse", 
              "sf", 
              "raster")
ipak(packages)

sau_cells <- read.csv(file.path(dir_spatial, "cell_ids.txt"), header=F , col.names = c("cell_id", "x", "y"))

#Make grids with 0.1 and 0.01 resolution too for higher resolution mapping. 

grid_size <- 0.1
longitudes <- seq(-180+(grid_size/2), 180-(grid_size/2), grid_size)
latitudes <- seq(-90+(grid_size/2), 90-(grid_size/2), grid_size)
grid_01 <- expand.grid(x = longitudes, 
            y = latitudes)
grid_01$cell_id <- seq(1,nrow(grid_01), 1) 
grid_01 <- grid_01 %>% 
  dplyr::select(cell_id, x, y)
write_csv(grid_01, file.path(dir_spatial, "cell_ids_01.txt"))

grid_01 <- grid_01 %>% 
  rename(lon=x,
         lat=y) %>% 
  mutate(lon=round((lon + 0.25) * 2) / 2 - 0.25,
         lat=round((lat + 0.25) * 2) / 2 - 0.25)

grid_01 <- grid_01 %>% 
  left_join(sau_cells, by=c("lon"="x", "lat"="y"))
grid_01 <- grid_01 %>% 
  rename(grid_01_id = cell_id.x,
         grid_05_id = cell_id.y)
write_csv(grid_01, file.path(dir_spatial, "cell_ids_01_05.txt"))

#This crashed my computer:
grid_size <- 0.01
longitudes <- seq(-180+(grid_size/2), 180-(grid_size/2), grid_size)
latitudes <- seq(-90+(grid_size/2), 90-(grid_size/2), grid_size)
# grid_001 <- expand.grid(x = longitudes, 
#                        y = latitudes)
grid_001$cell_id <- seq(1,nrow(grid_001), 1) 
grid_001 <- grid_001 %>% 
  dplyr::select(cell_id, x, y)
write_csv(grid_001, file.path(dir_spatial, "cell_ids_001.txt"))

grid_001 <- grid_001 %>% 
  rename(lon=x,
         lat=y) %>% 
  mutate(lon=plyr::round_any(lon, 0.25, f=round),
         lat=plyr::round_any(lat, 0.25, f=round))

grid_001 <- grid_001 %>% 
  left_join(sau_cells, by=c("lon"="x", "lat"="y"))