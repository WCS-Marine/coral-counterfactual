# load libraries for coral counterfactual

library(here)
library(lubridate)
library(RColorBrewer)
library(viridis) # plot colors
library(raster)  # raster manipulation
#library(rgdal)   # geospatial analysis
library(sf)      # geospatial analysis
library(knitr)   # tables
#library(velox)   # geospatial analysis (not using?)
library(conflicted)
library(tidyverse)


conflict_prefer("select", "dplyr") # (need to use raster::select for rasters)
conflict_prefer("filter", "dplyr")

# load "extract_values" function 
# source(here("globalprep","analysis","coral-counterfactual","00-lp-extract-values.R"))

# Plot themes and colors ----
# Color Palette mapped to region
region_palette = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#f7d54d')

# Plot themes
theme_partials <- function(){
  theme_light() %+replace%    #replace elements we want to change
  theme(
    # legend
    legend.position="none")
}

theme_rotate <- function(){
  theme_light() %+replace%    #replace elements we want to change
    theme(
      # rotate x axis labels
      axis.text.x = element_text(angle = 90))
}
