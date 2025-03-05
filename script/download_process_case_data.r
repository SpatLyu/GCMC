library(rgee)
# authorize GEE permissions
ee_clean_user_credentials()
ee_Initialize()
ee_Initialize(drive = T, gcs = T)

library(sf)
library(terra)

roi = read_sf('./data/tibet.gpkg') |> 
  st_bbox() |> 
  st_as_sfc() |> 
  sf_as_ee()

# download net primary productivity
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A3HGF?hl=en

npp = ee$ImageCollection('MODIS/061/MOD17A3HGF')$
         filter(ee$Filter$calendarRange(2021, 2021, 'year'))$
         select("Npp")$
         sum()$
         clip(roi)$
         multiply(1e-4)$
         rename("npp")
ee_as_rast(
  image = npp,
  region = roi,
  scale = 10000,
  crs = 'EPSG:4326',
  dsn = "./data/npp21.tif"
)

# download precipitation accumulation and soil moisture
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE

terraclimate = ee$ImageCollection('IDAHO_EPSCOR/TERRACLIMATE')$
  filter(ee$Filter$calendarRange(2021, 2021, 'year'))

pre = terraclimate$
  select("pr")$
  sum()$
  toDouble()$
  clip(roi)$
  rename("pre")
ee_as_rast(
  image = pre,
  region = roi,
  scale = 10000,
  crs = 'EPSG:4326',
  dsn = "./data/pre21.tif"
)

sm = terraclimate$
  select("soil")$
  sum()$
  multiply(0.1)$
  clip(roi)$
  rename("sm")
ee_as_rast(
  image = sm,
  region = roi,
  scale = 10000,
  crs = 'EPSG:4326',
  dsn = "./data/sm21.tif"
)
  
# download elevation
# https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003?hl=en

elev = ee$Image('USGS/SRTMGL1_003')$
  select('elevation')$
  clip(roi)$
  rename("elev")
ee_as_rast(
  image = elev,
  region = roi,
  scale = 10000,
  crs = 'EPSG:4326',
  dsn = "./data/elev.tif"
)

# process temperature
# https://www.geodata.cn/data/datadetails.html?dataguid=164304785536614&docId=2297

tibet = vect('./data/tibet.gpkg')
npp = rast(paste0("./data/",c("pre21","sm21","elev","npp21"),".tif"))
tem = rast('./data/tmp_2021.nc') |> 
  app("mean",na.rm = TRUE, cores = 8) |> 
  app(\(.x) .x * 0.1) |> 
  aggregate(fact = 10, fun = "mean", na.rm = TRUE) |> 
  crop(tibet) |> 
  resample(npp, method = "bilinear", threads = TRUE)
names(tem) = "tem"
writeRaster(tem,'./data/tem21.tif',overwrite = TRUE)

# clip data with the tibet polygon
npp = c(tem,npp) |> 
  crop(tibet, mask = TRUE)
writeRaster(npp,'./data/npp.tif')
