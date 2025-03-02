tibetbio = readr::read_csv('./data/tibet_bio.csv') |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326)

# embeddings:
m1 = spEDM::embedded(tibetbio,"sm",E = 3,tau = 5,trend.rm = FALSE)
m2 = spEDM::embedded(tibetbio,"bio",E = 3,tau = 6,trend.rm = FALSE)
# colnames(m1) = colnames(m2) = c("x","y","z")
# readr::write_csv(as.data.frame(m1),'./result/phase_space1.csv')
# readr::write_csv(as.data.frame(m2),'./result/phase_space2.csv')

H1 = spEDM:::RcppIntersectionCardinality(m1,m2,1:nrow(tibetbio),450,0,8,FALSE)
