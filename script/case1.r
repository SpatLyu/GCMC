#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~      Case: Columbus,OH Housing Value     ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-15    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)
columbus = sf::read_sf(system.file("case/columbus.gpkg", package="spEDM"))

#------------------------------------------------------------------------------#
#------    Causality by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

# housing value and crime (residential burglaries and vehicle thefts)
g1 = gcmc(data = columbus,cause = "hoval",effect = "crime",E = c(6,8),k = 20)
g1

# household income and crime (residential burglaries and vehicle thefts)
g2 = gcmc(data = columbus,cause = "inc",effect = "crime",E = c(5,8),k = 20)
g2

# housing value and household income
g3 = gcmc(data = columbus,cause = "hoval",effect = "inc",E = c(6,5),k = 20)
g3

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# housing value and crime (residential burglaries and vehicle thefts)
g1 = gccm(data = columbus,cause = "hoval",effect = "crime",
          libsizes = seq(5,45,5), E = c(6,8), k = c(6,10))
g1

# household income and crime (residential burglaries and vehicle thefts)
g2 = gccm(data = columbus,cause = "inc",effect = "crime",
          libsizes = seq(5,45,5), E = c(5,8), k = c(3,10))
g2

# housing value and household income
g3 = gccm(data = columbus,cause = "hoval",effect = "inc",
          libsizes = seq(5,45,5), E = c(6,5), k = c(6,10))
g3