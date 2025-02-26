source('./script/.internal_funs.r')

tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

k = floor(nrow(tibetbio) / 4)

# bio_res = spEDM::gcmc(data = tibetbio,
#                       cause = "ndvi",
#                       effect = "bio",
#                       E = 3,
#                       k = k,
#                       r = 0,
#                       trend.rm = FALSE)

# g = spEDM::gccm(data = tibetbio,
#                 cause = "ndvi",
#                 effect = "bio",
#                 libsizes = seq(100,1600,100),
#                 E = 3)

bio_gcmc = gcmc4lattice(tibetbio,E = 3,k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./result/bio_gcmc.csv')

bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = 3,k = 4,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./result/bio_gccm.csv')