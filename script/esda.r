#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~     Exploratory Spatial Data Analysis    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-07    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(sf)
library(terra)
library(tmap)

npp = terra::rast('./data/npp.tif')

# prediction sample points used in the case
strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5)
set.seed(2004)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE)

tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

bbox = sf::st_bbox(tibetbio) |> 
  sf::st_as_sfc() |> 
  sf::st_as_sf()

elev = elevatr::get_elev_raster(bbox, z = 4, clip = "bbox") |> 
  terra::rast()
names(elev) = 'elevation'
plot(elev)

map1 = tm_shape(elev)+
  tm_raster("elevation", 
            col.scale = tm_scale_continuous(values = terrain.colors(9)),
            col_alpha = 0.85, col.legend = tm_legend_hide()) +
  tm_style("classic",
           frame.doule.line = FALSE,
           earth.boundary = FALSE) +
  tm_shape(tibetbio) +
  tm_dots(col = "#4F767F", fill = "#4F767F",size = "sm",
          size.scale = tm_scale_continuous(values.scale = 0.5,
                                           values = 1:5),
          size.legend = tm_legend_hide())
tmap_save(map1,'./figure/map1.jpg',dpi = 300)

map2 = tm_shape(elev)+
  tm_raster("elevation", 
            col.scale = tm_scale_continuous(values = terrain.colors(9)),
            col_alpha = 0.85, col.legend = tm_legend_hide()) +
  tm_style("classic",
           frame.doule.line = FALSE,
           earth.boundary = FALSE) +
  tm_shape(tibetbio) +
  tm_symbols(col = "#A75F65", fill = "#A75F65", size = "bio",
             size.scale = tm_scale_continuous(values.scale = 0.5,
                                              values = 1:5,
                                              midpoint = NA),
             size.legend = tm_legend_hide())
tmap_save(map2,'./figure/map2.jpg',dpi = 300)
