#Created by AST on July 6 2023
####housekeeping####
library(sf)
library(dplyr)
library(sp)
library(ggplot2)
library(terra)
library(raster)
library(fasterize)
library(geodata)
library(tidyterra)
library(scales)
library(viridis)  

###setup

setwd("~/Documents/GitHub/preservation-review-2024/")

dir.create("04_image_creation_grid/output_oryza_example/")
setwd("04_image_creation_grid/output_oryza_example//")
###create raster for whole data####

#loads "r" dataframe
  results <- read.csv(file="../../02_download_rbien_sp_list/example_output_Oryza.csv")
#sum(duplicated((results))) #none of the results contain duplicate rows
just_coords <- results %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
just_coords <- just_coords[!is.na(just_coords$latitude),]
just_coords <- just_coords[!is.na(just_coords$longitude),]
#records are now cleaned for missing lat/long data - you may want to remove outliers resulting from misreported sampling location
#species names have to be in factor format for raster
just_coords$scrubbed_species_binomial <- as.factor(just_coords$scrubbed_species_binomial)
##https://gis.stackexchange.com/questions/435996/create-multiple-rasters-from-single-spatialpixeldataframe-by-unique-id
coords <- just_coords
coordinates(coords) <- c("longitude", "latitude")
#https://rspatial.org/cases/3-speciesdistribution.html
wrld <- world(path=".")
sp <- vect(coords, crs="+proj=longlat +datum=WGS84")
r <- rast(wrld)
ymax(r) <- 90
#resolution, in lat/long grids:
res(r) <- 4
results$count <- 1
#https://github.com/rspatial/terra/issues/553
rich_whole <- rasterize(sp, r, "scrubbed_species_binomial", function(x, ...) 
 length(unique(na.omit(x))))
r_1 <- r
res(r_1) <- 1
rich_whole_lat_1 <- rasterize(sp, r_1, "scrubbed_species_binomial", function(x, ...) 
  length(unique(na.omit(x))))
minmax(rich_whole)
plot_rich <- function(rich, name){
  ggplot(rich) +
    #geom_spatraster(data = rich, aes(fill = scrubbed_species_binomial)) +
    #if we desire NA to be the same as zero
    #https://blogs.uoregon.edu/rclub/2016/10/21/dealing-with-missingout-of-bounds-values-in-heatmaps/
    geom_tile(aes(x = x, y = y, fill = scrubbed_species_binomial))+
    scale_fill_viridis(option="mako", direction=-1,na.value = 'white')+
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          
    )+
    geom_spatvector(data = wrld, fill=NA)+
    labs(
      fill = "Number of species"
    )
  
  ggsave(paste(name, ".pdf", sep=""), width = 20, height = 10, units = "in")
  #https://stackoverflow.com/questions/60990276/why-does-my-plot-of-a-raster-in-r-blur-in-saved-file
}
plot_rich(rich_whole, "richness_whole_orzya_example_4")
plot_rich(rich_whole_lat_1, "richness_whole_orzya_example_1")


#####create raster for sra data####
##loads "compiled_results" dataframe
#uniq <- read.csv(file="../../03_compile_species_observations/output_orzya_example_checked_sra_names_in_results.csv") #if starting with species list
#latlong_only_sra <- results[results$scrubbed_species_binomial %in% uniq$x,]
#length(unique(latlong_only_sra$scrubbed_species_binomial)) 
latlong_only_sra <-read.csv(file="../../03_compile_species_observations/output_orzya_example_checked_sra_names_in_results.csv")
colnames(latlong_only_sra)
colnames(latlong_only_sra)[4] <- "scrubbed_species_binomial"
sra_just_coords <- latlong_only_sra %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values.
sra_just_coords <- sra_just_coords[!is.na(sra_just_coords$latitude),]
sra_just_coords <- sra_just_coords[!is.na(sra_just_coords$longitude),]
sra_just_coords$scrubbed_species_binomial <- as.factor(sra_just_coords$scrubbed_species_binomial)
##https://gis.stackexchange.com/questions/435996/create-multiple-rasters-from-single-spatialpixeldataframe-by-unique-id
sra_coords <- sra_just_coords
coordinates(sra_coords) <- c("longitude", "latitude")
##https://rspatial.org/cases/3-speciesdistribution.html
wrld <- world(path=".")
##spatial vector for the needed coords:
sra_sp <- vect(sra_coords, crs="+proj=longlat +datum=WGS84")
#using same r and r_1

rich_just_sra_4 <- rasterize(sra_sp, r, "scrubbed_species_binomial", function(x, ...) 
  length(unique(na.omit(x))))
rich_just_sra_1 <- rasterize(sra_sp, r_1, "scrubbed_species_binomial", function(x, ...) 
  length(unique(na.omit(x))))
#if you get "Error in .External(list(name = "CppMethod__invoke_notvoid", address = <pointer: (nil)>,  : 
#NULL value passed as symbol address" then redeclare r and r_1
#
##https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html#ref-usage
##
#####plotting####

plot_rich(rich_just_sra_4, "rich_just_sra_4")
plot_rich(rich_just_sra_1, "rich_just_sra_1")

#manipulation: https://rspatial.org/spatial/8-rastermanip.html

#######manipulation, percent completion#####

s <- rich_just_sra_4
s[is.na(s)] <- 0
g <- rich_whole

rs <- c(s, g)
w <- terra::lapp(rs, fun=function(x, y){ x / y } )
ggplot(w) +
  #geom_spatraster(data = rich, aes(fill = scrubbed_species_binomial)) +
  geom_tile(aes(x = x, y = y, fill = lyr1))+
  scale_fill_viridis(option="rocket", direction=-1,na.value = 'white')+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
  )+
  geom_spatvector(data = wrld, fill=NA)+
  labs(
    fill = "Completeness of sampling in SRA"
  )

ggsave(paste("percent_completion", ".pdf", sep=""), width = 20, height = 10, units = "in")


#save for external manipulation if desired
#writeRaster(rich_whole, file="rich_whole_4.tif")
#writeRaster(rich_just_sra_1, file="rich_just_sra_1.tif")
#writeRaster(rich_just_sra_4, file="rich_just_sra_4.tif")
#writeRaster(rich_whole_lat_1, file="rich_whole_1.tif")
#writeRaster(w, file="adj_rich_4.tif")
