## Vector Data

library(sf)

setwd("~/data")
shp <- 'cb_2016_us_county_5m'
counties <- st_read(shp)

sesync <- st_sfc(
    st_point(c(-76.503394, 38.976546)),
    crs = st_crs(counties))

## Bounding box

library(dplyr)
counties_md <- filter(
    counties,
    STATEFP == '24'
)

## Grid

grid_md <- st_make_grid(counties_md, n=4)


## Plot Layers

plot(grid_md)
plot(counties_md['ALAND'], add = TRUE)
plot(sesync, col = "green", pch = 20, add = TRUE)

## Plotting with ggplot2

library(ggplot2)

ggplot() +
    geom_sf(data = counties_md, aes(fill = ALAND)) +
    geom_sf(data = sesync, size = 3, color = 'red')  

theme_set(theme_bw())

ggplot() +
    geom_sf(data = counties_md, aes(fill = ALAND/1e6), color = NA) +
    geom_sf(data = sesync, size = 3, color = 'red') +
    scale_fill_viridis_c(name = 'Land area (sq. km)') +
    theme(legend.position = c(0.3, 0.3))

## Coordinate Transforms

shp <- 'huc250k'
huc <- st_read(shp)

prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
        +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 \
        +datum=WGS84 +units=m +no_defs'

counties_md <- st_transform(
    counties_md,
    crs = prj)
huc <- st_transform(huc, crs = prj)
sesync <- st_transform(sesync, crs = prj)

plot(st_geometry(counties_md))
plot(st_geometry(huc),
     border = 'blue', add = TRUE)
plot(sesync, col = 'green',
     pch = 20, add = TRUE)

## Geometric Operations

state_md <- st_union(counties_md)
plot(state_md)

huc_md <- st_intersection(huc, state_md)

plot(state_md)
plot(st_geometry(huc_md), border = 'blue',
     col = NA, add = TRUE)

## Raster Data

library(stars)
nlcd <- read_stars("nlcd_agg.tif", proxy = FALSE)
nlcd <- droplevels(nlcd)

## Crop

md_bbox <- st_bbox(huc_md)
nlcd <- st_crop(nlcd, md_bbox)

ggplot() +
    geom_stars(data = nlcd) +
    geom_sf(data = huc_md, fill = NA) +
    scale_fill_manual(values = attr(nlcd[[1]], 'colors'))

## Raster math

forest_types <- c('Evergreen Forest', 'Deciduous Forest', 'Mixed Forest')
forest <- nlcd
forest[!(forest %in% forest_types)] <- NA
plot(forest)

## Downsampling a raster

nlcd_agg <- st_warp(nlcd,
                cellsize = 1500, 
                method = 'mode', 
                use_gdal = TRUE)

nlcd_agg <- droplevels(nlcd_agg) 
levels(nlcd[[1]]) <- levels(nlcd[[1]]) 

plot(nlcd_agg)

## Mixing rasters and vectors

plot(nlcd, reset = FALSE)
plot(sesync, col = 'green',
     pch = 16, cex = 2, add = TRUE)

sesync_lc <- st_extract(nlcd, sesync)

baltimore <- nlcd[counties_md[1, ]]
plot(baltimore)

nlcd %>%
    st_crop(counties_md[1, ]) %>%
    pull %>%
    table

mymode <- function(x) names(which.max(table(x)))

modal_lc <- aggregate(nlcd_agg, huc_md, FUN = mymode) 

huc_md <- huc_md %>% 
    mutate(modal_lc = modal_lc[[1]])

ggplot(huc_md, aes(fill = modal_lc)) + 
    geom_sf() #getting numbers instead of labels on legend

## Mapview

library(mapview)
mapview(huc_md)

mapview(nlcd_agg, legend = FALSE, alpha = 0.5, 
        map.types = 'OpenStreetMap') +
    mapview(huc_md, legend = FALSE, alpha = 0.2)


##exercise 1
#Produce a map of Maryland counties with the county that contains SESYNC colored in red, 
#using either plot() or ggplot(). Hint: use st_filter() to find the county that contains SESYNC.

counties_sesync <- st_filter(counties_md, sesync)
plot(st_geometry(counties_md))
plot(counties_sesync, col = 'red', add = TRUE)
plot(sesync, col = 'green', pch = 20, add = TRUE)

ggplot() +
    geom_sf(data = counties_md, fill = NA) +
    geom_sf(data = counties_sesync, fill = 'red') +
    geom_sf(data = sesync, color = 'green')

#Exercise 2
bubble_md <- st_buffer(state_md, 5000)

ggplot() +
    geom_sf(data = st_geometry(state_md)) +
    geom_sf(data = bubble_md, linetype = 'dotted', fill = NA)

#Exercise 3
library(purrr)
map(nlcd == 'Deciduous Forest', mean)



