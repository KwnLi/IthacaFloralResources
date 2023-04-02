library(tidyverse)
library(terra)
library(sf)
library(exactextractr)

pts <- read.csv("Strawberry_SiteCoordinates_2013_2015.csv")
rastfolder <-"/Users/kevinl/Documents/mac_sync_pcloud/Cornell/Output/Output/w_cont/"

pts.sf <- pts %>% st_as_sf(coords=c("X","Y"), crs = 4326) %>% 
  st_transform(st_crs(fr))

pts.buf <- st_buffer(pts.sf, 750)

# function for summarizing raster values
sum_cats <- function(values, coverage_fractions){
  cats_sum <- data.frame(values, coverage_fractions) %>% group_by(values) %>%
    summarize(total_area = sum(coverage_fractions)) %>%
    pivot_wider(names_from = values, values_from = total_area)
  return(cats_sum)
}

# pts.ext <- exact_extract(fr, pts.buf, fun=sum_cats)

# loop over all rasters
# files in continuous raster folder
cont_files <- list.files(rastfolder, pattern="\\.tif$")

for(i in length(cont_files)){
  rast.i <- terra::rast(paste(rastfolder,cont_files[i],sep=""))
  
  extr.i <- exact_extract(rast.i, pts.buf, fun=sum_cats) %>% 
    mutate(across(everything(), .fns = ~replace_na(.,0))) 
  
  extr.i <- bind_rows(Name=pts.buf$Name, extr.i)
}