#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~        Case: Farmland NPP In China       ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-15    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)
npp = terra::rast(system.file("case/npp.tif", package = "spEDM"))
# To save the computation time, we will aggregate the data by 3 times and 
# select 1500 non-NA pixels to predict:
npp = terra::aggregate(npp, fact = 3, na.rm = TRUE)
npp

terra::global(npp,"isNA")
terra::ncell(npp)

nnamat = terra::as.matrix(npp[[1]], wide = TRUE)
nnaindice = which(!is.na(nnamat), arr.ind = TRUE)
dim(nnaindice)

set.seed(2025)
indices = sample(nrow(nnaindice), size = 1500, replace = FALSE)
libindice = nnaindice[-indices,]
predindice = nnaindice[indices,]

#------------------------------------------------------------------------------#
#------    Causality by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

# precipitation and npp
g1 = gcmc(data = npp,cause = "pre",effect = "npp",E = 2,k = 320,
          lib = nnaindice, pred = predindice)
g1

# temperature and npp
g2 = gcmc(data = npp,cause = "tem",effect = "npp",E = 2,k = 320,
          lib = nnaindice, pred = predindice)
g2

# precipitation and temperature
g3 = gcmc(data = npp,cause = "pre",effect = "tem",E = 2,k = 320,
          lib = nnaindice, pred = predindice)
g3

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# precipitation and npp
g1 = gccm(data = npp,cause = "pre",effect = "npp",
          libsizes = as.matrix(expand.grid(seq(10,130,10),seq(10,160,10))),
          E = 2, k = 6, lib = nnaindice, pred = predindice)
g1

# temperature and npp
g2 = gccm(data = npp,cause = "tem",effect = "npp",
          libsizes = as.matrix(expand.grid(seq(10,130,10),seq(10,160,10))),
          E = 2, k = 6, lib = nnaindice, pred = predindice)
g2

# precipitation and temperature
g3 = gccm(data = npp,cause = "pre",effect = "tem",
          libsizes = as.matrix(expand.grid(seq(10,130,10),seq(10,160,10))),
          E = 2, k = 6, lib = nnaindice, pred = predindice)
g3

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

npp.df = npp[terra::cellFromRowCol(npp,predindice[,1],predindice[,2])]
pcc = psych::corr.test(npp.df)
pcc

#------------------------------------------------------------------------------#
#------             Association by Geographical Detector(GD)             ------#
#------------------------------------------------------------------------------#

source('./script/ssh_q.r')
ssh_q(data = npp.df,cause = "pre",effect = "npp")
ssh_q(data = npp.df,cause = "tem",effect = "npp")
ssh_q(data = npp.df,cause = "pre",effect = "tem")
