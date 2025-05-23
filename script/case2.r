#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~  Case: County Level Population Density In China  ~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~         Author: Wenbo Lv; Date: 2025-03-15       ~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)

popd_nb = spdep::read.gal(system.file("case/popd_nb.gal",package = "spEDM"))
popd = readr::read_csv(system.file("case/popd.csv",package = "spEDM"))
popd_sf = popd |> 
  sf::st_as_sf(coords = c("x","y"), crs = 4326) |> 
  dplyr::select(popd,elev,tem)
popd_sf

#------------------------------------------------------------------------------#
#------    Causality by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

# temperature and population density
g1 = gcmc(data = popd_sf, cause = "tem", effect = "popd",
          E = c(2,5), k = 210, nb = popd_nb, detrend = TRUE)
g1

# elevation and population density
g2 = gcmc(data = popd_sf, cause = "elev", effect = "popd",
          E = c(1,5), k = 210, nb = popd_nb, detrend = TRUE)
g2

# elevation and temperature
g3 = gcmc(data = popd_sf, cause = "elev", effect = "tem",
          E = c(1,2), k = 210, nb = popd_nb, detrend = TRUE)
g3 # When there are insignificant results, we set spEDM to suppress output. This is not a bug.
g3$xmap

gcmc_case2 = list(g1,g2,g3)
readr::write_rds(gcmc_case2,'./result/case/gcmc_case2.rds')

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# temperature and population density
g1 = gccm(data = popd_sf, cause = "tem", effect = "popd",
          libsizes = seq(10, 2800, by = 100),E = c(2,5),k = 6,nb = popd_nb)
g1

# elevation and population density
g2 = gccm(data = popd_sf, cause = "elev", effect = "popd",
          libsizes = seq(10, 2800, by = 100),E = c(1,5),k = 6,nb = popd_nb)
g2

# elevation and temperature
g3 = gccm(data = popd_sf, cause = "elev", effect = "tem",
          libsizes = seq(10, 2800, by = 100),E = c(1,2),k = 6,nb = popd_nb)
g3

gccm_case2 = list(g1,g2,g3)
readr::write_rds(gccm_case2,'./result/case/gccm_case2.rds')

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

popdf = sf::st_drop_geometry(popd_sf)
pcc = psych::corr.test(popdf)
pcc
readr::write_rds(pcc,'./result/case/pcc_case2.rds')

#------------------------------------------------------------------------------#
#------             Association by Geographical Detector(GD)             ------#
#------------------------------------------------------------------------------#

source('./script/ssh_q.r')
q1 = ssh_q(data = popdf, cause = "tem", effect = "popd")
q2 = ssh_q(data = popdf, cause = "elev", effect = "popd")
q3 = ssh_q(data = popdf, cause = "elev", effect = "tem")
qv = do.call(rbind,list(q1,q2,q3))
qv
readr::write_rds(qv,'./result/case/gd_case2.rds')