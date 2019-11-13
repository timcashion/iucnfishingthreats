#### High seas categorization


#Load it!
#World <- readOGR(file.path(dir_spatial, "ne_10m_coastline/ne_10m_coastline.shp"))
World <- readOGR(file.path(dir_spatial, "ne_10m_coastline/ne_10m_land_no_casp.shp"), verbose=FALSE) #Switched to using without Caspian Sea for better visuals.

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
  scale_fill_gradientn(colours = pal) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(fill="") + 
  NULL 


sau_sacar <- read.csv(file.path(dir_spatial_csvs, "sau_cell_id_eez_fao_area.csv"))
sau_cells_all <- read.csv(file.path(dir_spatial_csvs, "cell_id_lon_lat_area.csv"))
sau_cells <- left_join(sau_cells_all, sau_sacar, by="cell_id") #%>% filter(water_area>0)

files <- list.files(dir_output)[grepl(list.files(dir_output), pattern="_SAU_IUCN_threat_05.csv")]

files <- file.path(dir_output, files)
harmonized_list <- lapply(files, data.table::fread)
harmonized_threats <- bind_rows(harmonized_list)
harmonized_threats <- harmonized_threats %>% group_by(lon, lat) %>% 
  summarize(profit = sum(profit, na.rm=TRUE),
            catch=sum(catch, na.rm=TRUE),
            n=sum(n, na.rm=TRUE),
            weighted_score=sum(weighted_score, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(weighted_score_norm = 100*(weighted_score/max(weighted_score)))
harmonized_threats <- bind_rows(harmonized_list)

cell_size <- "05"
eez_cell_ids <- read_csv(file.path(dir_spatial_csvs, paste("eez_cell_ids_", cell_size, ".csv", sep="")))
eez_ids <- read.csv(file.path(dir_spatial_csvs, "eez_ids.csv")) %>% 
  rename(eez_id = EEZID, eez = Name, entity=Entity) %>% 
  dplyr::select(eez_id, eez, entity)

eez_ids <- eez_ids %>% 
  mutate(eez = gsub(eez, pattern="ô", replacement="o"),
         entity = gsub(entity, pattern="ô", replacement="o"),
         eez = gsub(eez, pattern="é", replacement="e"),
         entity = gsub(entity, pattern="é", replacement="e"))


cell_ids_eez <- read.csv(file.path(dir_spatial_csvs, "cell_ids.txt"), header=F, col.names = c("cell_id", "lon", "lat"))
cell_ids_area <- read.csv(file.path(dir_spatial_csvs, paste("cell_id_area_", cell_size,".csv", sep="")))
cell_ids_eez <- cell_ids_eez %>% left_join(cell_ids_area) %>% left_join(eez_cell_ids) %>% left_join(eez_ids)

cell_ids_eez <- cell_ids_eez %>% 
  mutate(eez = ifelse(is.na(eez) & water_area>0, "High seas", eez),
         entity = ifelse(is.na(entity) & water_area>0, "High seas", entity))


harmonized_threats <- harmonized_threats %>% left_join(sau_cells)
harmonized_threats <- harmonized_threats %>% 
  mutate(catch_km2 = catch/water_area.x,
         profit_km2 = profit/water_area.x)


mean_profit_global <- mean(harmonized_threats %>% filter(catch_km2>0) %>% pull(profit_km2))
mean_wts <- mean(harmonized_threats %>% filter(weighted_score>0) %>% pull(weighted_score))

median_profit_global <- median(harmonized_threats %>% filter(catch_km2>0) %>% pull(profit_km2))
median_wts <- median(harmonized_threats %>% filter(weighted_score>0) %>% pull(weighted_score))



harmonized_threats$category <- NA
harmonized_threats <- harmonized_threats %>% 
  mutate(category = if_else(profit_km2 > median_profit_global, 
                            if_else(weighted_score > median_wts, "Fishery competition", "Fishery prioritization"), 
                            if_else(weighted_score > median_wts, "Easy wins", "Area of low concern")))

harmonized_threats$category <- fct_relevel(as.factor(harmonized_threats$category), c("Fishery competition", "Fishery prioritization",  "Area of low concern", "Easy wins"))
harmonized_threats <- harmonized_threats %>% filter(is.na(category)==F)

base_map + 
  geom_tile(data= harmonized_threats %>% filter(area_id==0), aes(x=lon, y=lat, fill=category)) +
  scale_fill_manual(values=c(zissou5[5], zissou5[4], zissou5[2], zissou5[1])) +
  xlab("") + 
  ylab("") + 
  NULL
ggsave(file.path(dir_supplemental_figs, "map_categorization_hsonly.png"), height=4, width=8, dpi=900)

harmonized_threats %>% 
  filter(eez_id==0) %>% 
  filter(catch>0) %>% 
  ggplot() + 
  geom_tile(aes(x=lon, y=lat, fill=catch)) +
  #scale_fill_manual(values=c(zissou5[5], zissou5[4], zissou5[2], zissou5[1])) +
  NULL
