library(terra)

npp = terra::rast('./data/npp.tif')
plot(npp)

terra::global(npp[["npp"]],"notNA")

strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5,
                               plot = TRUE, map = TRUE)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE) |> 
  sdsfun::sf_coordinates()
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))
