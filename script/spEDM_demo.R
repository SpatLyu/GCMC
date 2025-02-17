library(spEDM)

popd_nb = spdep::read.gal(system.file("extdata/popdensity_nb.gal",
                                      package = "spEDM"))
popd_nb

popdensity = readr::read_csv(system.file("extdata/popdensity.csv",
                                         package = "spEDM"))
popdensity

popd_sf = sf::st_as_sf(popdensity, coords = c("x","y"), crs = 4326)
popd_sf

set.seed(42)
pred = sample(nrow(popd_sf), size = 1000, replace = FALSE)


startTime = Sys.time()
pd_res = gcmc(data = popd_sf,
              cause = "Pre",
              effect = "popDensity",
              E = c(1,6),
              k = 6,
              r = 20,
             #pred = pred,
              nb = popd_nb)
endTime = Sys.time()
print(difftime(endTime,startTime, units ="mins"))
pd_res


columbus = sf::read_sf(system.file("shapes/columbus.gpkg", package="spData"))
g = spEDM::gcmc(columbus,"HOVAL","CRIME",E = c(6,5),k = 8)
g

cu = terra::rast(system.file("extdata/cu.tif", package = "spEDM"))

spEDM::simplex(cu,"industry", k = 5,
        lib = as.matrix(expand.grid(1:terra::nrow(cu),1:terra::ncol(cu))),
        pred = as.matrix(expand.grid(seq(5,131,5),seq(5,125,5))))

spEDM::simplex(cu,"cu", k = 5,
               lib = as.matrix(expand.grid(1:terra::nrow(cu),1:terra::ncol(cu))),
               pred = as.matrix(expand.grid(seq(5,131,5),seq(5,125,5))))

spEDM::simplex(cu,"ntl", k = 5,
        lib = as.matrix(expand.grid(1:terra::nrow(cu),1:terra::ncol(cu))),
        pred = as.matrix(expand.grid(seq(5,131,5),seq(5,125,5))))

tictoc::tic()
g1 = spEDM::gccm(cu,"ntl","cu",libsizes = seq(10,120,20),E = c(8,2),k = 5,
                 pred = as.matrix(expand.grid(seq(5,131,5),seq(5,125,5))),trend.rm = F)
g1
tictoc::toc()

tictoc::tic()
g2 = gcmc(cu,"industry","cu",E = 2,k = 6, r = 20, pred = as.matrix(expand.grid(seq(5,125,5),seq(5,125,5))))
g2
tictoc::toc()

tictoc::tic()
g3 = scpcm(cu,"industry","cu","ntl",E = c(2,2,8),libsizes = seq(10,120,20),k = 5,
           pred = as.matrix(expand.grid(seq(5,131,5),seq(5,125,5))))
g3
tictoc::toc()