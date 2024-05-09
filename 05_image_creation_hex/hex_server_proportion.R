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
###########setup intersecting lines#######

####https://gis.stackexchange.com/questions/463253/clipping-lines-from-intersecting-polygons-with-sf-in-r

r <- rbind(c(-180, -90), c(-181, -90), c(-181, 90), c(-180, 90), c(-180, -90)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc %>%
  st_set_crs(.,4326)

##########plot_rich from previous pipeline#########
#setwd("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/")
plot_rich_hexes <- function(rich, name, colorscheme){ #richness points to plot, name to save as
  ggplot() +
    scale_fill_viridis(option=colorscheme, direction=-1,na.value = 'white')+ #figure out later
    geom_spatvector(data = wrld, fill=NA)+
    geom_sf(data = rich, aes(fill=percent), color=NA) +
    #geom_sf_text(data = rich, aes(label = percent))+
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          
    )+
    geom_spatvector(data = wrld, fill=NA)+
    labs(
      fill = "Percent of species sequenced"
    )
  
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".pdf", sep=""), width = 20, height = 10, units = "in")
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".svg", sep=""), width = 20, height = 10, units = "in")
  
  #https://stackoverflow.com/questions/60990276/why-does-my-plot-of-a-raster-in-r-blur-in-saved-file
  st_write(rich, paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".shp", sep=""))
  # writeRaster(rich, paste(name, ".shp", sep=""))
}
plot_rich_hexes_nums <- function(rich, name, colorscheme){ #richness points to plot, name to save as
  ggplot() +
    scale_fill_viridis(option=colorscheme, direction=-1,na.value = 'white')+ #figure out later
    geom_spatvector(data = wrld, fill=NA)+
    geom_sf(data = rich, aes(fill=percent), color=NA) +
    geom_sf_text(data = rich, aes(label = percent))+
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          
    )+
    geom_spatvector(data = wrld, fill=NA)+
    labs(
      fill = "Percent of species sequenced"
    )
  
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".pdf", sep=""), width = 20, height = 10, units = "in")
  ggsave(paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".svg", sep=""), width = 20, height = 10, units = "in")
  
  #https://stackoverflow.com/questions/60990276/why-does-my-plot-of-a-raster-in-r-blur-in-saved-file
  st_write(rich, paste("~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/", name, ".shp", sep=""))
  # writeRaster(rich, paste(name, ".shp", sep=""))
}
##########whole setup#########
whole_0<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/whole_0_full.csv")
whole_1<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/whole_1_full.csv")
whole_2<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/whole_2_full.csv")
sra_0<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_0_full.csv")
sra_1<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_1_full.csv")
sra_2<-read.csv(file = "~/../../scratch/alexa/pres_rev_figs/03_figures/03_geoloc/hex/sra_2_full.csv")

##########res 0############
cells_whole_0 <- count(whole_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_whole_0<- count(cells_whole_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_whole_0)[colnames(cells_w_count_whole_0)== "n"] <- "whole"

cells_sra_0 <- count(sra_0, scrubbed_species_binomial, h3_resolution_0, sort = TRUE) 
cells_w_count_sra_0<- count(cells_sra_0, h3_resolution_0, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_sra_0)[colnames(cells_w_count_sra_0)== "n"] <- "sra"

cells_prop_0 <- left_join(cells_w_count_whole_0, cells_w_count_sra_0, by=c("h3_resolution_0"))
cells_prop_0$percent <- round((cells_prop_0$sra / cells_prop_0$whole) *100, digits=2)
cells_prop_0$percent[is.na(cells_prop_0$percent)] <- 0


poly_prop_0 <- cell_to_polygon(cells_prop_0, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_prop_0, r)
h3_to_drop <- i$h3_address
cropped_poly_prop_0<- poly_prop_0[!poly_prop_0$h3_address %in% h3_to_drop,]
wrld <- world(path=".")
plot_rich_hexes(colorscheme="mako", poly_prop_0, "prop_res_0_mako")
plot_rich_hexes(colorscheme="mako", cropped_poly_prop_0, "prop_cropped_res_0_mako")
plot_rich_hexes_nums(colorscheme="mako", poly_prop_0, "prop_res_0_nums_mako")
plot_rich_hexes_nums(colorscheme="mako", cropped_poly_prop_0, "prop_cropped_res_0_nums_mako")

plot_rich_hexes(colorscheme="magma", poly_prop_0, "prop_res_0_magma")
plot_rich_hexes(colorscheme="magma", cropped_poly_prop_0, "prop_cropped_res_0_magma")
plot_rich_hexes_nums(colorscheme="magma", poly_prop_0, "prop_res_0_nums_magma")
plot_rich_hexes_nums(colorscheme="magma", cropped_poly_prop_0, "prop_cropped_res_0_nums_magma")


########res 1#######
cells_whole_1 <- count(whole_1, scrubbed_species_binomial, h3_resolution_1, sort = TRUE) 
cells_w_count_whole_1<- count(cells_whole_1, h3_resolution_1, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_whole_1)[colnames(cells_w_count_whole_1)== "n"] <- "whole"

cells_sra_1 <- count(sra_1, scrubbed_species_binomial, h3_resolution_1, sort = TRUE) 
cells_w_count_sra_1<- count(cells_sra_1, h3_resolution_1, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_sra_1)[colnames(cells_w_count_sra_1)== "n"] <- "sra"

cells_prop_1 <- left_join(cells_w_count_whole_1, cells_w_count_sra_1, by=c("h3_resolution_1"))
cells_prop_1$percent <- round((cells_prop_1$sra / cells_prop_1$whole) *100, digits=2)
cells_prop_1$percent[is.na(cells_prop_1$percent)] <- 0


poly_prop_1 <- cell_to_polygon(cells_prop_1, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_prop_1, r)
h3_to_drop <- i$h3_address
cropped_poly_prop_1<- poly_prop_1[!poly_prop_1$h3_address %in% h3_to_drop,]
wrld <- world(path=".")
plot_rich_hexes(colorscheme="mako", poly_prop_1, "prop_res_1_mako")
plot_rich_hexes(colorscheme="mako", cropped_poly_prop_1, "prop_cropped_res_1_mako")
plot_rich_hexes_nums(colorscheme="mako", poly_prop_1, "prop_res_1_nums_mako")
plot_rich_hexes_nums(colorscheme="mako", cropped_poly_prop_1, "prop_cropped_res_1_nums_mako")

plot_rich_hexes(colorscheme="magma", poly_prop_1, "prop_res_1_magma")
plot_rich_hexes(colorscheme="magma", cropped_poly_prop_1, "prop_cropped_res_1_magma")
plot_rich_hexes_nums(colorscheme="magma", poly_prop_1, "prop_res_1_nums_magma")
plot_rich_hexes_nums(colorscheme="magma", cropped_poly_prop_1, "prop_cropped_res_1_nums_magma")

#######res 2#####
cells_whole_2 <- count(whole_2, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_whole_2<- count(cells_whole_2, h3_resolution_2, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_whole_2)[colnames(cells_w_count_whole_2)== "n"] <- "whole"

cells_sra_2 <- count(sra_2, scrubbed_species_binomial, h3_resolution_2, sort = TRUE) 
cells_w_count_sra_2<- count(cells_sra_2, h3_resolution_2, sort = TRUE) #get count for each hexagon
colnames(cells_w_count_sra_2)[colnames(cells_w_count_sra_2)== "n"] <- "sra"

cells_prop_2 <- left_join(cells_w_count_whole_2, cells_w_count_sra_2, by=c("h3_resolution_2"))
cells_prop_2$percent <- round((cells_prop_2$sra / cells_prop_2$whole) *100, digits=2)
cells_prop_2$percent[is.na(cells_prop_2$percent)] <- 0


poly_prop_2 <- cell_to_polygon(cells_prop_2, simple =F)
##plot(i, add = F, lty = 2, col = "red")
i <- st_intersection(poly_prop_2, r)
h3_to_drop <- i$h3_address
cropped_poly_prop_2<- poly_prop_2[!poly_prop_2$h3_address %in% h3_to_drop,]
wrld <- world(path=".")
plot_rich_hexes(colorscheme="mako", poly_prop_2, "prop_res_2_mako")
plot_rich_hexes(colorscheme="mako", cropped_poly_prop_2, "prop_cropped_res_2_mako")
plot_rich_hexes_nums(colorscheme="mako", poly_prop_2, "prop_res_2_nums_mako")
plot_rich_hexes_nums(colorscheme="mako", cropped_poly_prop_2, "prop_cropped_res_2_nums_mako")

plot_rich_hexes(colorscheme="magma", poly_prop_2, "prop_res_2_magma")
plot_rich_hexes(colorscheme="magma", cropped_poly_prop_2, "prop_cropped_res_2_magma")
plot_rich_hexes_nums(colorscheme="magma", poly_prop_2, "prop_res_2_nums_magma")
plot_rich_hexes_nums(colorscheme="magma", cropped_poly_prop_2, "prop_cropped_res_2_nums_magma")

