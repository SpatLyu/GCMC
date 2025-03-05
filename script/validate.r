library(terra)
npp = terra::rast('./npp.tif')

strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5,
                               plot = TRUE, map = TRUE)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE) |> 
  sdsfun::sf_coordinates()
nnamat = terra::as.matrix(!is.na(npp[["npp"]]), wide = TRUE)
nnaindice = terra::rowColFromCell(npp,which(nnamat))
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))

source('./.internal_funs.r')

# select the dimensions of embdedding
simplex4grid(npp,lib = nnaindice, pred = predindice, trend.rm = TRUE)

k = 500
npp_gcmc = gcmc4grid(npp,E = c(3,3,3,3,3),k = 600,r = 0,pred = predindice,trend.rm = TRUE)
readr::write_csv(npp_gcmc,'./npp_gcmc.csv')

npp_gcmc = gcmc4grid(npp,E = c(3,9,3,4,10),k = k,r = 0,pred = predindice,trend.rm = FALSE)
readr::write_csv(npp_gcmc,'./npp_gcmc.csv')

g = spEDM::gcmc(data = npp,
            cause = "pre",
            effect = "elev",
            E = 3,
            k = 450,
            r = 0,
            pred = predindice,
            trend.rm = TRUE,
            progressbar = TRUE)
g1 = spEDM::gcmc(data = npp,
                cause = "pre",
                effect = "elev",
                E = 3,
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = TRUE,
                progressbar = TRUE)
g2 = spEDM::gcmc(data = npp,
                cause = "tem",
                effect = "sm",
                E = 3,
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = TRUE,
                progressbar = TRUE)
g3 = spEDM::gcmc(data = npp,
                cause = "npp",
                effect = "elev",
                E = c(5,3),
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = FALSE,
                progressbar = TRUE)


source('./.internal_funs.r')
tibetbio = readr::read_csv('./tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)


bio_gcmc = gcmc4lattice(tibetbio,E = c(7,7,9,8,10,7),k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./bio_gcmc2.csv')

bio_gcmc = gcmc4lattice(tibetbio,E = c(7,7,8,3,3,3),k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./bio_gcmc.csv')

bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = c(7,7,9,8,10,7),k = 12,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./bio_gccm2.csv')

bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = c(7,7,8,3,3,3),k = 4,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./bio_gccm2.csv')