

source(here::here("R", "common_fxns.R"))

packages <- c("tidyverse", 
              "bookdown",
              "knitr"
)
ipak(packages)

iucn_species <- read.csv(file.path(dir_data, paste("iucn_species_list_", api_version, ".csv", sep="")))
squalus_ids <- iucn_species %>% filter(result.scientific_name=="Squalus acanthias") %>% pull (result.taxonid)

squalus_spat_files_exist <- list.files(paste(dir_rasters, "_05", sep=""))[grepl(list.files(paste(dir_rasters, "_05", sep="")), pattern=paste("_", squalus_ids, sep="",collapse="|"))]
squalus_spat_files <- file.path(paste(dir_rasters, "_05", sep=""), squalus_spat_files_exist)

squalus_spat_list <- lapply(squalus_spat_files, data.table::fread)
squalus_spat <- bind_rows(squalus_spat_list)
x <- squalus_spat %>% group_by(cell_id) %>% 
  mutate(n=n()) %>% 
  filter(n>1)

#squalus_spat <- data.table::fread(file.path(paste(dir_rasters, "_05", sep=""), paste("iucn_sid_", "91209505", ".csv", sep="")))
squalus_spat <- squalus_spat %>% 
  mutate(category = if_else(is.na(rgn_category), global_category, rgn_category))



cell_ids <- data.table::fread(file.path(dir_spatial_csvs, "cell_ids_05.txt"), header=F, col.names = c("cell_id", "lon", "lat"))

squalus_spat <- squalus_spat %>% 
  left_join(cell_ids)



#Set up basic world map. 
#World <- readOGR(file.path(dir_spatial, "ne_10m_coastline/ne_10m_coastline.shp"))
World <- rgdal::readOGR(file.path(dir_spatial, "ne_10m_coastline/ne_10m_land_no_casp.shp"), verbose=FALSE) #Switched to using without Caspian Sea for better visuals.

# fortify.shape <- function(x){
#   x@data$id <- rownames(x@data)
#   x.f = fortify(x, region = "id")
#   x.join <- inner_join(x.f, x@data, by = "id")
# }

Forty_World<- fortify(World)

base_map <- ggplot() +
  geom_path(data = Forty_World, aes(x = long,
                                    y = lat,
                                    group = group
  ),
  color = "black",
  size = 0.25) +
  theme_classic() + 
  labs(list(title = "",
            x = "Longitude",
            y = "Latitude")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(fill="") + 
  NULL 

squalus_map_occ <- base_map + 
  geom_tile(data=squalus_spat, aes(x=lon, y=lat, fill=presence)) +
  NULL
squalus_map_occ
library(IUCNpalette)

squalus_map_threat <- base_map + 
  geom_tile(data=squalus_spat, aes(x=lon, y=lat, fill=category)) +
  labs(x="", y="", fill="Threat status") + 
  scale_fill_manual(values=IUCNpalette::iucn_palette(category = c("VU", "EN"))) + 
  theme(legend.position = "none") +
  NULL
squalus_map_threat
ggsave("./figs/Squalus_acanthias_category.png", dpi=300, width=6, height = 4)






