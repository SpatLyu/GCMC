source('./script/.internal_funs.r')

tibetnpp = readr::read_csv('./data/tibet_npp.csv') |> 
  sf::st_as_sf(coords = c("X","Y"), crs = 4326)

# select the dimensions of embdedding
simplex4lattice(tibetnpp,lib = 1:nrow(tibetnpp), pred = 1:nrow(tibetnpp))