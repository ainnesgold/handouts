# PART I: EXPLORE DATA READ AND DISPLAY INPUTS

## Simple Features

library(sf)
setwd("~/data")
lead <- read.csv('SYR_soil_PB.csv')

lead <- st_as_sf(lead,
  coords = c('x', 'y'),
  crs = 32618)

plot(lead['ppm'])

library(ggplot2)
ggplot(data = lead,
       mapping = aes(color=ppm)) +
  geom_sf()

## Feature Collections

blockgroups <- st_read('bg_00')

## Table Operations

ggplot(blockgroups,
       aes(fill = Shape_Area)) +
   geom_sf()

library(dplyr)

census <- read.csv('SYR_census.csv')
census <- mutate(census, 
  BKG_KEY = as.character(BKG_KEY)
)

census_blockgroups <- inner_join(
  blockgroups, census,
  by = c('BKG_KEY'))

ggplot(census_blockgroups,
       aes(fill = POP2000)) +
  geom_sf()

census_tracts <- census_blockgroups %>%
  group_by(TRACT) %>%
  summarize(
    POP2000 = sum(POP2000),
    perc_hispa = sum(HISPANIC) / POP2000)

tracts <- st_read('ct_00')
ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=tracts,
          color = 'red', fill = NA)

# PART II: SPATIAL QUERY AND AGGREGATION 

## Spatial Join

ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=lead, color = 'red',
          fill = NA, size = 0.1)

lead_tracts <- lead %>%
    st_join(census_tracts) %>%
    st_drop_geometry()

lead_tracts <- lead %>%
    st_join(census_tracts) %>%
    st_drop_geometry() %>%
    group_by(TRACT) %>%
    summarise(avg_ppm = mean(ppm))

census_lead_tracts <- census_tracts %>%
  inner_join(lead_tracts)

ggplot(census_lead_tracts,
       aes(fill = avg_ppm)) +
  geom_sf() +
  scale_fill_gradientn(
    colors = heat.colors(7))

library(mapview)
mapview(lead['ppm'],
        map.types = 'OpenStreetMap',
        viewer.suppress = TRUE) +
  mapview(census_tracts,
          label = census_tracts$TRACT,
          legend = FALSE)

# PART III :  KRIGING SURFACES 

## The Semivariogram

library(gstat)

lead_xy <- read.csv('SYR_soil_PB.csv')

v_ppm <- variogram(
  ppm ~ 1,
  locations = ~ x + y,
  data = lead_xy)
plot(v_ppm)

v_ppm_fit <- fit.variogram(
  v_ppm,
  model = vgm(model='Sph', range=900, nugget=1))
plot(v_ppm, v_ppm_fit)

## Kriging

pred_ppm <- st_make_grid(
  lead, cellsize = 400,
  what = 'centers')

pred_ppm <- pred_ppm[census_tracts]

ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=pred_ppm, color = 'red', fill = NA)

pred_ppm <- krige(
  formula = ppm~1,
  locations = lead,
  newdata = pred_ppm,
  model = v_ppm_fit)

ggplot() + 
  geom_sf(data = census_tracts,
          fill = NA) +
  geom_sf(data = pred_ppm,
          aes(color = var1.pred))

pred_ppm_tracts <-
  pred_ppm %>%
  st_join(census_tracts) %>%
  st_drop_geometry() %>%
  group_by(TRACT) %>%
  summarise(pred_ppm = mean(var1.pred))
census_lead_tracts <- 
  census_lead_tracts %>%
  inner_join(pred_ppm_tracts)

ggplot(census_lead_tracts,
       aes(x = pred_ppm, y = avg_ppm)) +
  geom_point() +
  geom_abline(slope = 1)

# PART IV : SPATIAL AUTOCORRELATION AND REGRESSION 

ppm.lm <- lm(pred_ppm ~ perc_hispa,
  census_lead_tracts)

census_lead_tracts <- census_lead_tracts %>%
  mutate(lm.resid = resid(ppm.lm))
plot(census_lead_tracts['lm.resid'])

library(sp)
library(spdep)
library(spatialreg)

tracts <- as(
  st_geometry(census_tracts), 'Spatial')
tracts_nb <- poly2nb(tracts)

plot(census_lead_tracts['lm.resid'],
     reset = FALSE)
plot.nb(tracts_nb, coordinates(tracts),
        add = TRUE)

tracts_weight <- nb2listw(tracts_nb)

moran.plot(
  census_lead_tracts[['lm.resid']],
  tracts_weight,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)

ppm.sarlm <- lagsarlm(
  pred_ppm ~ perc_hispa,
  data = census_lead_tracts,
  tracts_weight,
  tol.solve = 1.0e-30)

dev.off() # uncomment to try this if moran.plot below fails

moran.plot(
  resid(ppm.sarlm),
  tracts_weight,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)
