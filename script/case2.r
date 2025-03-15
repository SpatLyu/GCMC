#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~  Case: County Level Population Density In China  ~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~         Author: Wenbo Lv; Date: 2025-03-15       ~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)

popd_nb = spdep::read.gal(system.file("case/popdensity_nb.gal",package = "spEDM"))
popdensity = readr::read_csv(system.file("case/popdensity.csv",package = "spEDM"))
popd_sf = sf::st_as_sf(popdensity, coords = c("x","y"), crs = 4326)
popd_sf

#------------------------------------------------------------------------------#
#------    Causality by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

# temperature and population density
g1 = gcmc(data = popd_sf, cause = "tem", effect = "popdensity",
          E = c(2,5), k = 160, nb = popd_nb, trend.rm = FALSE)
g1

# elevation and population density
g2 = gcmc(data = popd_sf, cause = "elev", effect = "popdensity",
          E = c(1,5), k = 150, nb = popd_nb, trend.rm = FALSE)
g2

# elevation and temperature
g3 = gcmc(data = popd_sf, cause = "elev", effect = "tem",
          E = c(1,2), k = 150, nb = popd_nb, trend.rm = FALSE)
g3

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# temperature and population density
g1 = gccm(data = popd_sf, cause = "tem", effect = "popdensity",
          libsizes = seq(10, 2800, by = 100),E = c(2,5),k = 6,nb = popd_nb)
g1

# elevation and population density
g2 = gccm(data = popd_sf, cause = "elev", effect = "popdensity",
          libsizes = seq(10, 2800, by = 100),E = c(1,5),k = 6,nb = popd_nb)
g2

# elevation and temperature
g3 = gccm(data = popd_sf, cause = "elev", effect = "tem",
          libsizes = seq(10, 2800, by = 100),E = c(1,2),k = 6,nb = popd_nb)
g3

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

popdf = sf::st_drop_geometry(dplyr::select(popd_sf,popdensity,elev,tem,pre))
pcc = psych::corr.test(popdf)
pcc

#------------------------------------------------------------------------------#
#------             Association by Geographical Detector(GD)             ------#
#------------------------------------------------------------------------------#

source('./script/ssh_q.r')
ssh_q(data = popdf, cause = "tem", effect = "popdensity")
ssh_q(data = popdf, cause = "elev", effect = "popdensity")
ssh_q(data = popdf, cause = "elev", effect = "tem")
