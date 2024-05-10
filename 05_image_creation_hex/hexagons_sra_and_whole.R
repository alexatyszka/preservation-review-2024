#run in R 4.3.2
#using https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html as reference
#https://observablehq.com/@nrabinowitz/h3-tutorial-heatmap-rendering
########setup########
library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
library(sp)
library(terra)
library(raster)
library(fasterize)
library(geodata)
library(tidyterra)
library(scales)
#library(maptools) #error
library(viridis)
library(svglite)
# for R < 4, since H3 addresses are handled as strings
options(stringsAsFactors = FALSE)
setwd("~")
sra <- read.csv(file ="~/Documents/GitHub/preservation-review-2024/03_compile_species_observations/output_orzya_example_checked_sra_names_in_results.csv")
colnames(sra)
colnames(sra)[4] <- "scrubbed_species_binomial"
just_coords_sra <- sra %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
just_coords_sra <- just_coords_sra[!is.na(just_coords_sra$latitude),]
just_coords_sra <- just_coords_sra[!is.na(just_coords_sra$longitude),]
#https://r-spatial.github.io/sf/reference/sf.html
sample_st<- st_as_sf(just_coords_sra, coords = c("longitude", "latitude"),
          crs=4326)
      #   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

###########setup intersecting lines#######
####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r

r <- rbind(c(-180, -90), c(-181, -90), c(-181, 90), c(-180, 90), c(-180, -90)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc %>%
  st_set_crs(.,4326)

i <- st_intersection(poly_sra_0, r)
########sra resolution 0#########
cells_sra_0 <- point_to_cell(sample_st, res=0, simple=F) #data frame
#write.csv(cells_sra_0, "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_0_full.csv")
#rm(list = ls())

#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_sra_0 <- count(cells_sra_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_sra_0<- count(cells_sra_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_0 <- cell_to_polygon(cells_w_count_sra_0, simple =F)
i <- st_intersection(poly_sra_0, r)
#plot(i, add = F, lty = 2, col = "red")

h3_to_drop <- i$h3_address
cropped_poly_sra_0<- poly_sra_0[!poly_sra_0$h3_address %in% h3_to_drop,]
plot(poly_sra_0)
plot(cropped_poly_sra_0)
#in cases where the grid cells cross the 180th meridian, this cropping removes image artifacts

########resolution 1#########
cells_sra_1 <- point_to_cell(sample_st, res=1, simple=F) #data frame
#write.csv(cells_sra_1, "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_1_full.csv")
#rm(cells_sra_1)
cells_sra_1 <- count(cells_sra_1, scrubbed_species_binomial, h3_resolution_1, sort = TRUE) 
cells_w_count_sra_1<- count(cells_sra_1, h3_resolution_1, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_1 <- cell_to_polygon(cells_w_count_sra_1, simple =F)
#plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_sra_1, r)
h3_to_drop <- i$h3_address
cropped_poly_sra_1<- poly_sra_1[!poly_sra_1$h3_address %in% h3_to_drop,]
plot(poly_sra_1)
plot(cropped_poly_sra_1)
########resolution 2########
cells_sra_2 <- point_to_cell(sample_st, res=2, simple=F) #data frame
#write.csv(cells_sra_2, "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_2_full.csv")
#rm(list = ls())

cells_sra_2 <- count(cells_sra_2, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_sra_2<- count(cells_sra_2, h3_resolution_2, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_2 <- cell_to_polygon(cells_w_count_sra, simple =F)

#########plot_rich from full step#########
plot_rich_hexes <- function(rich, name){ #richness points to plot, name to save as
  ggplot() +
    scale_fill_viridis(option="mako", direction=-1,na.value = 'white')+ #figure out later
    geom_spatvector(data = wrld, fill=NA)+
    geom_sf(data = rich, aes(fill=n)) +
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          
    )+
    geom_spatvector(data = wrld, fill=NA)+
    labs(
      fill = "Number of species"
    )
  
  ggsave(paste(name, ".pdf", sep=""), width = 20, height = 10, units = "in")
  ggsave(paste(name, ".svg", sep=""), width = 20, height = 10, units = "in")
  
  #https://stackoverflow.com/questions/60990276/why-does-my-plot-of-a-raster-in-r-blur-in-saved-file
  st_write(rich, paste(name, ".shp", sep=""))
 # writeRaster(rich, paste(name, ".shp", sep=""))
}
setwd("Documents/GitHub/preservation-review-2024/05_image_creation_hex/output_oryza_example/")
plot_rich_hexes(poly_sra_0, "sra_res_0")
#plot_rich_hexes(cropped_poly_sra_0, "sra_res_0_cropped")
plot_rich_hexes(poly_sra_1, "sra_res_1")
#plot_rich_hexes(cropped_poly_sra_1, "sra_res_1_cropped")
plot_rich_hexes(poly_sra_2, "sra_res_2")
#plot_rich_hexes(cropped_poly_sra_2, "sra_res_2_cropped")


##########whole setup#########
setwd("~")
whole <- read.csv(file ="../../scratch/alexa/pres_rev_figs/01_data/03_cleaned_coords_for_maps/coords_wholedb.csv")
just_coords_whole <- whole %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$latitude),]
just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$longitude),]
wrld <- world(path=".")
sample <- head(just_coords_whole, 100000)
#https://r-spatial.github.io/sf/reference/sf.html
sample_st_whole<- st_as_sf(sample, coords = c("longitude", "latitude"),
                     crs=4326)
#   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r
###########whole 0#######
cells_whole_0 <- point_to_cell(sample_st_whole, res=0, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_whole_0 <- count(cells_whole_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_whole_0<- count(cells_whole_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_whole_0 <- cell_to_polygon(cells_w_count_whole_0, simple =F)
#plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_whole_0, r)
h3_to_drop <- i$h3_address
cropped_poly_whole_0<- poly_whole_0[!poly_whole_0$h3_address %in% h3_to_drop,]
plot(poly_whole_0)
plot(cropped_poly_whole_0)

###########whole 3#######
cells_whole_3 <- point_to_cell(sample_st_whole, res=3, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_whole_3 <- count(cells_whole_3, scrubbed_species_binomial, h3_resolution_3, sort = TRUE) 
cells_w_count_whole_3<- count(cells_whole_3, h3_resolution_3, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_whole_3 <- cell_to_polygon(cells_w_count_whole_3, simple =F)
#plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_whole_3, r)
h3_to_drop <- i$h3_address
cropped_poly_whole_3<- poly_whole_3[!poly_whole_3$h3_address %in% h3_to_drop,]
plot(poly_whole_3)
plot(cropped_poly_whole_3)
