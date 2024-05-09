#run in R 4.3.2
#using https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html as reference
#https://observablehq.com/@nrabinowitz/h3-tutorial-heatmap-rendering
########setup########
rm(list = ls())
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

options(stringsAsFactors = FALSE)
setwd("~")
###########setup interesecting lines#######

####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r

r <- rbind(c(-180, -90), c(-181, -90), c(-181, 90), c(-180, 90), c(-180, -90)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc %>%
  st_set_crs(.,4326)
##########whole setup#########
#whole <- read.csv(file ="../../scratch/alexa/pres_rev_figs/01_data/03_cleaned_coords_for_maps/coords_wholedb.csv")
#just_coords_whole <- whole %>% dplyr::select(latitude, longitude, scrubbed_species_binomial)
###remove NA values from lat/long data.
#just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$latitude),]
#just_coords_whole <- just_coords_whole[!is.na(just_coords_whole$longitude),]
#rm(whole)
#length(just_coords_whole$latitude)
#23395931
#length(unique(just_coords_whole)$latitude)
#12900337 #half of them are exact duplicates, from same collection location. this will reduce computation load!
#just_coords_whole <- unique(just_coords_whole) #only keep unique rows here since range matters only
#sample_a <- just_coords_whole[1:4300112,]
#sample_b <- just_coords_whole[4300113:8600224,]
#sample_c <- just_coords_whole[8600225:12900337,]
write.csv(sample_a, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_sample_a.csv")
write.csv(sample_b, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_sample_b.csv")
write.csv(sample_c, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_sample_c.csv")

#https://r-spatial.github.io/sf/reference/sf.html
#sample_st_whole<- st_as_sf(just_coords_whole, coords = c("longitude", "latitude"),
#                             crs=4326)

sample_st_whole_b<- st_as_sf(sample_b, coords = c("longitude", "latitude"),
                           crs=4326)
sample_st_whole_c<- st_as_sf(sample_c, coords = c("longitude", "latitude"),
                            crs=4326)
####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r
##########res 0############
#cells_whole_0_a <- point_to_cell(sample_st_whole_a, res=0, simple=F) #data frame, error here
#write.csv(cells_whole_0_a, "../../scratch/alexa/pres_rev_figs/01_data/whole_0_a.csv")
#rm(cells_whole_0_a)
#cells_whole_0_b <- point_to_cell(sample_st_whole_b, res=0, simple=F) #data frame, error here
#write.csv(cells_whole_0_b, "../../scratch/alexa/pres_rev_figs/01_data/whole_0_b.csv")
#cells_whole_0_c <- point_to_cell(sample_st_whole_c, res=0, simple=F) #data frame, error here
#write.csv(cells_whole_0_c, "../../scratch/alexa/pres_rev_figs/01_data/whole_0_c.csv")
#a_0<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_0_a.csv")
#a_0[c("X")] <- list(NULL)
#b_0<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_0_b.csv")
#b_0[c("X")] <- list(NULL)
#c_0<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_0_c.csv")
#c_0[c("X")] <- list(NULL)
#
#whole_0 <- rbind(a_0, b_0, c_0)
#write.csv(whole_0, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_0_full.csv")


##whole_0 %>% count(scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 

cells_whole_0 <- count(whole_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_whole_0<- count(cells_whole_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
##poly_sra <- unlist(cells_w_count_sra, use.names = T) #not sure if this is needed?
poly_whole_0 <- cell_to_polygon(cells_w_count_whole_0, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_whole_0, r)
h3_to_drop <- i$h3_address
cropped_poly_whole_0<- poly_whole_0[!poly_whole_0$h3_address %in% h3_to_drop,]
#plot(poly_whole_0)
#plot(cropped_poly_whole_0)


########res 1#######
#cells_whole_1 <- point_to_cell(sample_st_whole, res=1, simple=F) #data frame, error here
#cells_whole_1_a <- point_to_cell(sample_st_whole_a, res=1, simple=F) #data frame, error here
#write.csv(cells_whole_1_a, "../../scratch/alexa/pres_rev_figs/01_data/whole_1_a.csv")
#rm(cells_whole_1_a)

#cells_whole_1_b <- point_to_cell(sample_st_whole_b, res=1, simple=F) #data frame, error here
#write.csv(cells_whole_1_b, "../../scratch/alexa/pres_rev_figs/01_data/whole_1_b.csv")


#cells_whole_1_c <- point_to_cell(sample_st_whole_c, res=1, simple=F) #data frame, error here
#write.csv(cells_whole_1_c, "../../scratch/alexa/pres_rev_figs/01_data/whole_1_c.csv")
a_1<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_1_a.csv")
a_1[c("X")] <- list(NULL)
b_1<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_1_b.csv")
b_1[c("X")] <- list(NULL)
c_1<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_1_c.csv")
c_1[c("X")] <- list(NULL)
#
whole_1 <- rbind(a_1, b_1, c_1)
write.csv(whole_1, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_1_full.csv")


##whole_0 %>% count(scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 

cells_whole_1 <- count(whole_1, scrubbed_species_binomial, h3_resolution_1, sort = TRUE) 
cells_w_count_whole_1<- count(cells_whole_1, h3_resolution_1, sort = TRUE) #get count for each hexagon
poly_whole_1 <- cell_to_polygon(cells_w_count_whole_1, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_whole_1, r)
h3_to_drop <- i$h3_address
cropped_poly_whole_1<- poly_whole_1[!poly_whole_1$h3_address %in% h3_to_drop,]

########res 2#######
cells_whole_2_a <- point_to_cell(st_as_sf(sample_a, coords = c("longitude", "latitude"), crs=4326), res=2, simple=F)
write.csv(cells_whole_2_a, "../../scratch/alexa/pres_rev_figs/01_data/whole_2_a.csv")
rm(list = ls())

cells_whole_2_b <- point_to_cell(st_as_sf(read.csv("~/../../scratch/alexa/pres_rev_figs/01_data/whole_sample_b.csv"), coords = c("longitude", "latitude"), crs=4326), res=2, simple=F) #data frame, error here
write.csv(cells_whole_2_b, "../../scratch/alexa/pres_rev_figs/01_data/whole_2_b.csv")
rm(list = ls())

cells_whole_2_c <- point_to_cell(st_as_sf(read.csv("~/../../scratch/alexa/pres_rev_figs/01_data/whole_sample_c.csv"), coords = c("longitude", "latitude"), crs=4326), res=2, simple=F) 
write.csv(cells_whole_2_c, "../../scratch/alexa/pres_rev_figs/01_data/whole_2_c.csv")
rm(list = ls())

a_2<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_2_a.csv")
a_2[c("X")] <- list(NULL)
b_2<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_2_b.csv")
b_2[c("X.1")] <- list(NULL)
c_2<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/01_data/whole_2_c.csv")
c_2[c("X.1")] <- list(NULL)
#
whole_2 <- rbind(a_2, b_2, c_2) #if any have been calculated at wrong resolution this will not work
write.csv(whole_2, "~/../../scratch/alexa/pres_rev_figs/01_data/whole_2_full.csv")

cells_whole_2 <- count(whole_2, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_whole_2<- count(cells_whole_2, h3_resolution_2, sort = TRUE) #get count for each hexagon
poly_whole_2 <- cell_to_polygon(cells_w_count_whole_2, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_whole_2, r)
h3_to_drop <- i$h3_address
cropped_poly_whole_2<- poly_whole_2[!poly_whole_2$h3_address %in% h3_to_drop,]
##########plot_rich from previous pipeline#########
#setwd("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/")
plot_rich_hexes <- function(rich, name){ #richness points to plot, name to save as
  ggplot() +
    scale_fill_viridis(option="mako", direction=-1,na.value = 'white')+ #figure out later
    geom_spatvector(data = wrld, fill=NA)+
    geom_sf(data = rich, aes(fill=n), color=NA) +
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          
    )+
    geom_spatvector(data = wrld, fill=NA)+
    labs(
      fill = "Number of species"
    )
  
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".pdf", sep=""), width = 20, height = 10, units = "in")
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".svg", sep=""), width = 20, height = 10, units = "in")
  
  #https://stackoverflow.com/questions/60990276/why-does-my-plot-of-a-raster-in-r-blur-in-saved-file
  st_write(rich, paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".shp", sep=""))
  # writeRaster(rich, paste(name, ".shp", sep=""))
}
#######
wrld <- world(path=".")
plot_rich_hexes(poly_whole_0, "whole_res_0")
plot_rich_hexes(cropped_poly_whole_0, "whole_res_0_cropped")
plot_rich_hexes(poly_whole_1, "whole_res_1")
plot_rich_hexes(cropped_poly_whole_1, "whole_res_1_cropped")
plot_rich_hexes(poly_whole_2, "whole_res_2")
plot_rich_hexes(cropped_poly_whole_2, "whole_res_2_cropped")
