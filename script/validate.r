source('./.internal_funs.r')
tibetbio = readr::read_csv('./tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

k = 500

bio_gcmc = gcmc4lattice(tibetbio,E = c(7,7,8,3,3,3),k = k,r = 0,trend.rm = FALSE)
readr::write_csv(bio_gcmc,'./bio_gcmc.csv')

bio_gccm = gccm4lattice(tibetbio,libsizes = seq(100,1600,100),E = c(7,7,9,8,10,7),k = 12,trend.rm = TRUE)
readr::write_csv(bio_gccm,'./bio_gccm.csv')