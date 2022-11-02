#idea: make a map of the world and plot NCBI data on it

#load libraries -------
library(rgdal)      
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)


#load files for world map --------
rm(list=ls())
gc()

load("working_files/earth_map_objects.RData")

## This will load 5 R objects:
##   xbl.X & lbl.Y are two data.frames that contain labels for graticule lines
##   NE_box is a SpatialPolygonsDataFrame object and represents a bounding box for Earth 
##   NE_countries is a SpatialPolygonsDataFrame object representing countries 
##   NE_graticules is a SpatialLinesDataFrame object that represents 10 dg latitude lines and 20 dg longitude lines
##   NOTE: data downloaded from http://www.naturalearthdata.com/

#also get coastline outline
NE_coastlines <- rnaturalearth::ne_coastline(scale = "medium", returnclass = 'sp')
NE_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = 'sp')

#read in lat/long of points we want to plot onto map ---------
sites.raw <- read.delim("working_files/paper2_all_biosample_list.tsv")
sites <- sites.raw %>% dplyr::select(project_index,biosample_acc_sra,project_acc_bioprj,specificEpithet,decimalLatitude,decimalLongitude,QC)

#filter out -
#filter out biosamps without lat/long
sites <- sites %>% filter(is.na(decimalLatitude)==F) %>% filter(is.na(decimalLongitude)==F)
#filter out domestic etc. species that shouldn't have been in datathon in first place
# NOTE - there are some biosamps that don't have a specificEpithet, so don't filter on species names again
# here bc we'll loose those NAs
#and drop marine projects that are also in full datathon 
#(Eric made sure both structured author comments and paper_avail cols matched btwn these "duplicate" records)
sites <- sites %>% filter(!project_index %in% c("M0012","M0022","M0037","M0050","M0062","M0063"))
#and drop 5 prjs that are missing from 2020 master NCBI list (bc they have e.g. "exome" or "phenotype" in bioprj data type field)
sites <- sites %>% filter(!project_acc_bioprj %in% c("PRJNA315895", "PRJNA324830", "PRJNA386149", "PRJNA429104", "PRJNA453553", "PRJNA377812"))

#check that all biosamp IDs only have one species name - should return 0 if so
sites %>% group_by(biosample_acc_sra,specificEpithet) %>% summarise(n=n()) %>% 
  group_by(biosample_acc_sra) %>% summarise(n=n()) %>% filter(n>1) %>% nrow()

#drop any points with nonsensical lat/long (shouldn't be any)
sites <- sites %>% filter(decimalLatitude >= -90 & decimalLatitude <= 90 & decimalLongitude >= -180 & decimalLongitude <= 180)
sites %>% group_by(QC) %>% summarise(n=n())


#get taxonomic info ------------

#get unique biosamples
taxinfo <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-11-3-2021-greplnamematching.csv", header = T) %>%
  dplyr::select(biosample_acc_sra,organism_biosamp,division_taxonomy,contains("taxize")) %>% distinct()
#keep just biosamp IDs that are in datathon list
taxinfo <- taxinfo %>% filter(biosample_acc_sra %in% sites$biosample_acc_sra)
#check that all biosamp IDs only have one species name - should return 0 if so
taxinfo %>% group_by(biosample_acc_sra,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(biosample_acc_sra) %>% summarise(n=n()) %>% filter(n>1) %>% nrow()

#assign tax categories
taxinfo <- taxinfo %>% mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Fungi", "Fungi", ".")) %>% 
  mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Viridiplantae", "Other Plants", tax_category)) %>%
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Cycadopsida", "Gymnosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Gnetopsida", "Gymnosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Pinopsida", "Gymnosperms", tax_category)) %>%
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Magnoliopsida", "Angiosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Aves", "Birds", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Mammalia", "Mammals", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize %in% c("Actinopteri","Chondrichthyes","Coelacanthimorpha"), "Fish", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize %in% c("Lepidosauria","Amphibia"), "Amphibians and Reptiles", tax_category)) %>%
  mutate(tax_category = ifelse(phylum_ncbitaxonomy.via.taxize == "Arthropoda", "Arthropods", tax_category)) %>% 
  mutate(tax_category = ifelse(phylum_ncbitaxonomy.via.taxize == "Mollusca", "Mollusks", tax_category)) %>%
  mutate(tax_category = ifelse(division_taxonomy == "Mammals" & is.na(tax_category)==T, "Mammals", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Vertebrates" & tax_category==".", "Other Vertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Invertebrates" & tax_category==".", "Invertebrates", tax_category)) %>% mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Fungi" & is.na(tax_category)==T, "Fungi", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Vertebrates" & is.na(tax_category)==T, "Other Vertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Invertebrates" & is.na(tax_category)==T, "Invertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Plants and Fungi" & is.na(tax_category)==T, "Other", tax_category)) %>% 
  mutate(tax_category = ifelse(tax_category == "Invertebrates", "Other Invertebrates", tax_category))

#make broader tax categories
key <- data.frame(tax_category = c("Other Plants", "Angiosperms", "Other", 
                                   "Arthropods", "Gymnosperms", "Fish", "Mammals", "Amphibians and Reptiles", 
                                   "Birds", "Other Invertebrates", "Fungi", "Mollusks", "Other Vertebrates", 
                                   "Domesticated Plant", "Domesticated Animal", "Domesticated Aquaculture", 
                                   "Domesticated Fungus"),
                  broad_tax_category = c("Plants", "Plants", "Other", 
                                         "Invertebrates", "Plants", "Vertebrates", "Vertebrates", "Vertebrates", 
                                         "Vertebrates", "Invertebrates", "Fungi", "Invertebrates", "Vertebrates", 
                                         "Domesticated", "Domesticated", "Domesticated","Domesticated"))

#tack broader categories on
taxinfo <- merge(taxinfo,key,by="tax_category",all.x=T)

taxinfo %>% group_by(broad_tax_category,tax_category) %>% summarise(n=n())

#add onto sites
sites <- merge(sites,taxinfo, by = "biosample_acc_sra", all.x = T)

#code expects lat and long to be first two cols and to be called lat and lon
sites <- sites %>% rename("lat" = "decimalLatitude") %>% rename("lon" = "decimalLongitude") %>% 
  dplyr::select(lon,lat,everything())
sites.df <- sites



# do projecting and heat/raster layer making -----------
#set up projections for map and our points
## give the PORJ.4 string for Eckert IV projection
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
## or use the short form "+proj=eck4"

## re-project the shapefiles and points
NE_countries.prj  <- spTransform(NE_countries, CRSobj = PROJ)
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)
NE_box.prj        <- spTransform(NE_box, CRSobj = PROJ)
NE_coastlines.prj        <- spTransform(NE_coastlines, CRSobj = PROJ)

sp::coordinates(sites) <- ~lon+lat
sp::proj4string(sites) <- sp::CRS("+proj=longlat +datum=WGS84")
sites.prj <- sp::spTransform(sites, CRSobj = PROJ)
#ggplot can only handle df not sp so save one here for plotting
sites.df.prj <- sites.prj %>% as.data.frame()

#make heatmap raster - N number of points in each box (large res = fewer/bigger squares)
rast <- raster::raster(res = 3)
raster::extent(rast) <- raster::extent(sites.prj)
rast
raster.Nindivs <- raster::rasterize(sites.prj, rast, fun = "count")
raster.Nindivs <- raster::rasterToPoints(raster.Nindivs) %>% as.data.frame()
#and heatmap of N number of unique species per grid cell
raster.Nspecies <- raster::rasterize(sites.prj, rast, sites.prj$specificEpithet, fun = function(x, ...) {length(unique(na.omit(x)))})
raster.Nspecies <- raster::rasterToPoints(raster.Nspecies) %>% as.data.frame()

# make point map colored by taxonomic cluster - white ocean and gray land ----------
map = ggplot() +
  coord_fixed(ratio = 1) +
  ## add projected bounding box, white for ocean
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "white", size = .25) +
  ## add projected countries, a bit darker grey for terrestrial polygons
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray70", fill = "gray70", size = .25) +
  
  #add land outlines
  geom_path(data = NE_coastlines.prj, 
            aes(long, lat, group = group), 
            colour = "gray70", size = .2) +
  
  ## add graticules
  geom_path(data = NE_graticules.prj,
            aes(long, lat, group = group),
            linetype = "dotted", colour = "grey60", size = 0.2) +
  
  ## add locations (points)
  geom_point(data = sites.df.prj,aes(x = lon, y = lat, fill = broad_tax_category), size = 0.8, pch = 21, colour = "black", alpha = 1, stroke = 0.1) +
  scale_fill_manual(values = c("#B79F00","#A0B2DA","#00BFC4","#53BB69", "#F564E3"),
                    #labels = c("Fungi", "Mammals", "Non-mammal verts.", "Plant and fungi"),
                    name = "Taxonomic \ndivision") +
  #guides(fill = "none") + 
  ## Set empty theme
  theme_void() + # remove the default background, gridlines & default gray color around legend's symbols
  theme(legend.title = element_text(colour="black", size=10, face="bold"), # adjust legend title
        legend.position = c(1.035, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=3, b=0, l=0), unit="cm"), # adjust margins
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

print(map)
ggsave(paste("figures/paper2/worldmap_39841samps-385bioprjs-taxonomic-theme2.pdf",sep="") , width = 174, height = 100, dpi = 600, units = c("mm"))

sites.df.prj %>% distinct(project_acc_bioprj) %>% nrow()
sites.df.prj %>% distinct(biosample_acc_sra) %>% nrow()


# make heatmap - cells colored by N individuals - white ocean --------------
map = ggplot() +
  
  coord_fixed(ratio = 1) +
  
  ## add projected bounding box, white for ocean
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "white", size = .25) +
  
  ## add projected countries, a bit darker grey for terrestrial polygons
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray70", fill = "gray70", size = .25) +
  
  ## add graticules
  geom_path(data = NE_graticules.prj,
            aes(long, lat, group = group),
            linetype = "dotted", colour = "grey60", size = 0.2) +
  
  ## add N indivs raster
  geom_tile(data = raster.Nindivs, aes(x = x, y = y, fill = ID)) +
  
  viridis::scale_fill_viridis(name = "N. samples", trans = "log",
                              breaks = c(0,1,10,100,1000), labels = c(0,1,10,100,1000),
                              option="magma") +
  
  #add land outlines
  geom_path(data = NE_coastlines.prj, 
            aes(long, lat, group = group), 
            colour = "gray70", size = .2) +
  
  ## Set empty theme
  theme_void() + # remove the default background, gridlines & default gray color around legend's symbols
  theme(legend.title = element_text(colour="black", size=10, face="bold"), # adjust legend title
        legend.position = c(1.035, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=3, b=0, l=0), unit="cm"), # adjust margins
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

print(map)
ggsave(paste("figures/paper2/worldmap_39841samps-385bioprjs-Nindivs-theme2.pdf",sep="") , width = 174, height = 100, dpi = 600, units = c("mm"))



# make heatmap - cells colored by N species --------------
map = ggplot() +
  
  coord_fixed(ratio = 1) +
  
  ## add projected bounding box, white for ocean
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "white", size = .25) +
  
  ## add projected countries, a bit darker grey for terrestrial polygons
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray70", fill = "gray70", size = .25) +
  
  ## add graticules
  geom_path(data = NE_graticules.prj,
            aes(long, lat, group = group),
            linetype = "dotted", colour = "grey60", size = .2) +
  
  ## add N indivs raster
  geom_tile(data = raster.Nspecies, aes(x = x, y = y, fill = layer)) +
  
  viridis::scale_fill_viridis(name = "N. species", trans = "log",
                              breaks = c(0,1,5,10,20), labels = c(0,1,5,10,20),
                              option="magma") +
  #add land outlines
  geom_path(data = NE_coastlines.prj, 
            aes(long, lat, group = group), 
            colour = "gray70", size = .2) +
  
  ## Set empty theme
  theme_void() + # remove the default background, gridlines & default gray color around legend's symbols
  theme(legend.title = element_text(colour="black", size=10, face="bold"), # adjust legend title
        legend.position = c(1.035, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=3, b=0, l=0), unit="cm"), # adjust margins
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

print(map)
ggsave(paste("figures/paper2/worldmap_39841samps-385bioprjs-Nspecies-theme2.pdf",sep="") , width = 174, height = 100, dpi = 600, units = c("mm"))




#graveyard ---------

# make point map - points colored by taxonomic cluster ------------
map = ggplot() +
  
  coord_fixed(ratio = 1) +
  
  ## add projected bounding box, blue for ocean
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "#abd9e9", size = .25) +
  
  ## add projected countries, a bit darker grey for terrestrial polygons
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray50", fill = "gray90", size = .25) +
  
  ## add graticules
  geom_path(data = NE_graticules.prj,
            aes(long, lat, group = group),
            linetype = "dotted", colour = "grey50", size = .25) +
  
  ## add locations (points)
  geom_point(data = sites.df.prj,aes(x = lon, y = lat, fill = broad_tax_category), size = 1.5, pch = 21, colour = "black", alpha = 0.75) +
  scale_fill_manual(values = c("#B79F00","#A0B2DA","#00BFC4","#53BB69", "#F564E3"),
                    #labels = c("Fungi", "Mammals", "Non-mammal verts.", "Plant and fungi"),
                    name = "Taxonomic division") +
  
  ## Set empty theme
  theme_void() + # remove the default background, gridlines & default gray color around legend's symbols
  theme(legend.title = element_text(colour="black", size=10, face="bold"), # adjust legend title
        legend.position = c(1.035, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=3, b=0, l=0), unit="cm"), # adjust margins
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

print(map)

sites.df.prj %>% dplyr::distinct(project_acc_bioprj) %>% nrow() #number of datasets
sites.df.prj %>% nrow() #number of samples

ggsave(paste("figures/paper2/worldmap_39641samps-385bioprjs-taxonomic.pdf",sep="") , width = 10, height = 5, dpi = 600, units = c("in"))



# make heatmap - cells colored by N individuals --------------
map = ggplot() +
  
  coord_fixed(ratio = 1) +
  
  ## add projected bounding box, blue for ocean
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "#abd9e9", size = .25) +
  
  ## add projected countries, a bit darker grey for terrestrial polygons
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray80", fill = "gray80", size = .25) +
  
  ## add graticules
  geom_path(data = NE_graticules.prj,
            aes(long, lat, group = group),
            linetype = "dotted", colour = "grey50", size = .25) +
  
  ## add N indivs raster
  geom_tile(data = raster.Nindivs, aes(x = x, y = y, fill = ID)) +
  
  viridis::scale_fill_viridis(name = "N. samples", trans = "log",
                              breaks = c(0,1,10,100,1000), labels = c(0,1,10,100,1000),
                              option="magma") +
  
  #add white outlines
  geom_path(data = NE_coastlines.prj, 
            aes(long, lat, group = group), 
            colour = "white", size = .25) +
  
  ## Set empty theme
  theme_void() + # remove the default background, gridlines & default gray color around legend's symbols
  theme(legend.title = element_text(colour="black", size=10, face="bold"), # adjust legend title
        legend.position = c(1.035, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=3, b=0, l=0), unit="cm"), # adjust margins
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

print(map)
ggsave(paste("figures/paper2/worldmap_39641samps-385bioprjs-Nindivs.pdf",sep="") , width = 10, height = 5, dpi = 600, units = c("in"))









pts.raw <- read.csv("/Users/rachel/Desktop/test.csv")
pts <- pts.raw
sp::coordinates(pts) <- ~lon+lat
rast <- raster::raster(res = 50)
raster::extent(rast) <- raster::extent(pts)
out <- raster::rasterize(pts, rast, fun = "count")
out <- raster::rasterToPoints(out) %>% as.data.frame()

ggplot() + 
  geom_tile(data = out, aes(x=x, y=y, fill=ID)) +
  geom_point(data = pts.raw, aes(x = lon, y = lat), colour = "red")

