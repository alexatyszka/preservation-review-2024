#Created by AST, edited 8 May 2024
#This file takes names as uploaded to the SRA and compares them to the rgbif backbone taxonomy. 
#Only the coordinates associated with a valid species should be returned in the final file.
library("BIEN")
library("rgbif")
library("dplyr")
setwd("~")
setwd("~/Documents/GitHub/preservation-review-2024/") #where your larger, rbien database is
sras <- read.csv("01_download_sra_list/Output_orzya_example_unique_species.csv") #where your SRA downloaded species list is
rbien_coords <- read.csv(file="02_download_rbien_sp_list/example_output_Oryza.csv")

u_all <- unique(rbien_coords$scrubbed_species_binomial)
rbien_coords_checklist <- rgbif::name_backbone_checklist(u_all)
colnames(rbien_coords)[3] <- c("name")
rbien_coords_full <- rgbif::name_backbone_checklist(rbien_coords)
#u_sp <- unique(sras$x) #column should be species names
#sum(u_sp %in% u_all)
name_checklist <- rgbif::name_backbone_checklist(u_sp)
checked <- name_checklist[name_checklist$rank == "SPECIES",]
checked <- checked[checked$status == "ACCEPTED",]
checked <- distinct(checked, canonicalName)
#length(checked$canonicalName)
checked_in_all <- checked$canonicalName[checked$canonicalName %in% rbien_coords_checklist$canonicalName]
checked_in_all <- checked_in_all[!is.na(checked_in_all)]
#rbien_coords_checklist$canonicalName %in% u_all
joined_coords_with_canonical <- left_join(rbien_coords, rbien_coords_full, by = c("name"="verbatim_name"), relationship ="many-to-many")

coords_with_species_in_sra <- joined_coords_with_canonical[joined_coords_with_canonical$canonicalName %in% checked_in_all,]
#you may not need all these columns
write.csv(coords_with_species_in_sra, "output_orzya_example_checked_sra_names_in_results.csv")
