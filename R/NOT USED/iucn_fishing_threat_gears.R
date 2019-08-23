
knitr::opts_chunk$set(echo = FALSE, message=F, warning=F)
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages <- c("tidyverse", 
              # "maptools", 
              "rgdal",
              # "sf", 
              "wesanderson",
              # "sp",
              # "raster"
              "parallel",
              "bookdown",
              "knitr"
)
ipak(packages)

dir_data <- "./data"
dir_spatial <- "./spatial"
dir_spatial_csvs <- "./spatial/csvs"
dir_rasters <- "./spatial/spp_rasters"
dir_output <- "./output"
dir_figs <- "./figs"

#Establish list of marine species (using O'Hara's at present)
marine_species <- read.csv(file.path(dir_data, "spp_marine_from_api_2018-1.csv"))
#### Align with threats and narrative text: ####
species_threats <- read.csv(file.path(dir_data, "species_fishing_threats.csv"), stringsAsFactors = F)
colnames(species_threats) <- gsub(colnames(species_threats), pattern="result\\.", replacement="")
risk_codes <- read.csv(file.path(dir_data, "risk_code_lookup.csv"), stringsAsFactors = F)
species_threats <- species_threats %>% left_join(risk_codes, by=c("category"="code"))
species_threats <- species_threats %>%
  dplyr::select(iucn_sid, class_name, scientific_name, code_current, cat_score) %>%
  unique()

# species_threats <- species_threats %>% filter(code_current %in% threatened_status)
text_threats <- read.csv(file.path(dir_data, "narrative_threats_output.csv")) %>% 
  rename(iucn_sid=scientific_name)
text_threats$trawlers <- ifelse(text_threats$bottom_trawl==1|text_threats$pelagic_trawl==1|text_threats$trawl_unspec==1, 1, NA)
text_threats <- text_threats %>% 
  gather(key="super_code", value="threat", -iucn_sid) %>% 
  filter(is.nan(threat)==F) %>% 
  filter(is.na(threat)==F) %>% 
  mutate(super_code = gsub(super_code, pattern="_", replacement=" "))



#I want to compare how many species match at least one gear when they have fishing listed as a threat in their Threat Assessment. 

length(marine_species %>% pull(iucn_sid) %>% unique())
#12913 unique species. 

length(species_threats %>% pull(iucn_sid) %>% unique())
#4560 have fishing identified as a threat. 

length(text_threats %>% pull(iucn_sid) %>% unique())
#2671 have at least one fishing gear identified 

length(text_threats %>% filter(iucn_sid %in% species_threats$iucn_sid) %>% pull(iucn_sid) %>% unique())
#All 2671 species in the text threat having fishing listed as a threat. 

#Therefore, 1889 species don't have a gear related to their fishing threat. 
no_gear_species <- species_threats %>% filter(!iucn_sid %in% text_threats$iucn_sid) %>% dplyr::select(iucn_sid, class_name, scientific_name) %>% distinct()


#What I am finding is that species with regional assessments (and thus unique iucn_id numbers, are missing some of their gear threats)
#Need to solve this. 
squalus_ids <- species_threats %>% filter(scientific_name=="Squalus acanthias") %>% pull(iucn_sid) %>% unique()
squalus_gears <- text_threats %>% filter(iucn_sid %in% squalus_ids)




