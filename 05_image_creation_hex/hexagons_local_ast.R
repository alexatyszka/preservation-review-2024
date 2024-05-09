#run in R 4.3.2
#using https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html as reference
#https://observablehq.com/@nrabinowitz/h3-tutorial-heatmap-rendering
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
#wholedb <-read.csv(file ="atyszka2@uic.edu - Google Drive/Shared drives/walker-lab-shared-drive/preservation_review_ast/01_data/03_cleaned_coords_for_maps/coords_wholedb.csv")
sra <- read.csv(file ="atyszka2@uic.edu - Google Drive/Shared drives/walker-lab-shared-drive/preservation_review_ast/01_data/03_cleaned_coords_for_maps/coords_sradb.csv")
just_coords_sra <- sra %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
just_coords_sra <- just_coords_sra[!is.na(just_coords_sra$latitude),]
just_coords_sra <- just_coords_sra[!is.na(just_coords_sra$longitude),]
wrld <- world(path=".")
#sample <- head(just_coords_sra, 1000)
#https://r-spatial.github.io/sf/reference/sf.html
sample_st<- st_as_sf(just_coords_sra, coords = c("longitude", "latitude"),
          crs=4326)
      #   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

###########setup interesecting lines#######

####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r

r <- rbind(c(-180, -90), c(-181, -90), c(-181, 90), c(-180, 90), c(-180, -90)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc %>%
  st_set_crs(.,4326)

#i <- st_intersection(poly_sra_0, r)

cells_sra_3 <- point_to_cell(sample_st, res=3, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_sra_3 <- count(cells_sra_3, scrubbed_species_binomial, h3_resolution_3, sort = TRUE) 
cells_w_count_sra_3<- count(cells_sra_3, h3_resolution_3, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_3 <- cell_to_polygon(cells_w_count_sra_3, simple =F)
#plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_sra_3, r)
h3_to_drop <- i$h3_address
cropped_poly_sra_3<- poly_sra_3[!poly_sra_3$h3_address %in% h3_to_drop,]
plot(poly_sra_3)
plot(cropped_poly_sra_3)
########sra resolution 0#########
cells_sra_0 <- point_to_cell(sample_st, res=0, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_sra_0 <- count(cells_sra_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_sra_0<- count(cells_sra_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_0 <- cell_to_polygon(cells_w_count_sra_0, simple =F)
i <- st_intersection(poly_sra_0, r)
plot(i, add = F, lty = 2, col = "red")

h3_to_drop <- i$h3_address
cropped_poly_sra_0<- poly_sra_0[!poly_sra_0$h3_address %in% h3_to_drop,]
plot(poly_sra_0)
plot(cropped_poly_sra_0)

########sra resolution 2#########
cells_sra_2 <- point_to_cell(sample_st, res=2, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_sra_2 <- count(cells_sra_2, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_sra_2<- count(cells_sra_2, h3_resolution_2, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra_2 <- cell_to_polygon(cells_w_count_sra_2, simple =F)
i <- st_intersection(poly_sra_2, r)
plot(i, add = F, lty = 2, col = "red")

h3_to_drop <- i$h3_address
cropped_poly_sra_2<- poly_sra_2[!poly_sra_2$h3_address %in% h3_to_drop,]
plot(poly_sra_2)
plot(cropped_poly_sra_2)

########resolution 1#########
cells_sra_1 <- point_to_cell(sample_st, res=1, simple=F) #data frame
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
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
######example below#######
#cells_sra %>% count(scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_sra <- count(cells_sra, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_sra<- count(cells_sra, h3_resolution_2, sort = TRUE) #get count for each hexagon
#poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_sra <- cell_to_polygon(cells_w_count_sra, simple =F)


#plot(poly_sra_0[1])



plot.new()
ggplot() +
  geom_spatvector(data = wrld, fill=NA)+
  geom_sf(data=cropped_poly_sra_1, aes(fill=n, color=NA)) +
  geom_sf_text(data = cropped_poly_sra_1, aes(label = n))+
  theme(axis.title = element_text(),
        axis.text.x = element_text())+
  theme_classic() +
  coord_sf()

#########plot_rich from previous pipeline#########
setwd("atyszka2@uic.edu - Google Drive/Shared drives/walker-lab-shared-drive/preservation_review_ast/03_figures/03_geoloc/hex/")
plot_rich_hexes <- function(rich, name){ #richness points to plot, name to save as
  ggplot() +
    scale_fill_viridis(option="mako", direction=-1,na.value = 'white')+ #figure out later
    geom_spatvector(data = wrld, fill=NA)+
    geom_sf(data = rich, aes(fill=n), color=NA) +
    geom_sf_text(data = cropped_poly_sra_1, aes(label = n))+
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
plot_rich_hexes(poly_sra_0, "sra_res_0")
plot_rich_hexes(cropped_poly_sra_0, "sra_res_0_cropped")
plot_rich_hexes(poly_sra_1, "sra_res_1")
plot_rich_hexes(cropped_poly_sra_1, "sra_res_1_cropped_test")
plot_rich_hexes(poly_sra_3, "sra_res_3")
plot_rich_hexes(cropped_poly_sra_3, "sra_res_3_cropped")
plot_rich_hexes(poly_sra_2, "sra_res_2")
plot_rich_hexes(cropped_poly_sra_2, "sra_res_2_cropped")


setwd("~")
whole <- read.csv(file ="~/atyszka2@uic.edu - Google Drive/Shared drives/walker-lab-shared-drive/preservation_review_ast/01_data/03_cleaned_coords_for_maps/coords_wholedb.csv")
just_coords_whole <- whole %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$latitude),]
just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$longitude),]
just_coords_whole <- unique(just_coords_whole)
wrld <- world(path=".")
#sample <- head(just_coords_sra, 1000)
#https://r-spatial.github.io/sf/reference/sf.html
sample_st_whole<- st_as_sf(just_coords_whole, coords = c("longitude", "latitude"),
                     crs=4326)
#   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r
###########whole 3#######
cells_whole_0 <- point_to_cell(sample_st_whole, res=0, simple=F) #data frame
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
