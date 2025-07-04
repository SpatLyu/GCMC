#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~        Case: Farmland NPP In China       ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-07-03    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)
npp = terra::rast(system.file("case/npp.tif", package = "spEDM"))
# To save the computation time, we will aggregate the data by 3 times and 
# select 1500 non-NA pixels to predict:
npp = terra::aggregate(npp, fact = 3, na.rm = TRUE)
npp = npp[[c("npp","pre","tem")]]
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
g1 = gcmc(npp, "pre", "npp", E = 18, k = 165, lib = predindice, pred = predindice)
g1

# temperature and npp
g2 = gcmc(npp, "tem", "npp", E = 18, k = 165, lib = predindice, pred = predindice)
g2

# precipitation and temperature
g3 = gcmc(npp, "pre", "tem", E = 18, k = 165, lib = predindice, pred = predindice)
g3

gcmc_case3 = list(g1,g2,g3)
readr::write_rds(gcmc_case3,'./result/case/gcmc_case3.rds')

#------------------------------------------------------------------------------#
#------    Causality by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# precipitation and npp
g1 = gccm(npp, "pre", "npp", E = 18, k = 20, lib = predindice, pred = predindice)
g1

# temperature and npp
g2 = gccm(npp, "tem", "npp", E = 18, k = 20, lib = predindice, pred = predindice)
g2

# precipitation and temperature
g3 = gccm(npp, "pre", "tem", E = 18, k = 20, lib = predindice, pred = predindice)
g3

gccm_case3 = list(g1,g2,g3)
readr::write_rds(gccm_case3,'./result/case/gccm_case3.rds')

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

npp.df = dplyr::filter(npp[terra::cellFromRowCol(npp,predindice[,1],predindice[,2])],
                       dplyr::if_all(dplyr::everything(),
                                     \(.x) !is.na(.x)))
pcc = psych::corr.test(npp.df)
pcc
readr::write_rds(pcc,'./result/case/pcc_case3.rds')

#------------------------------------------------------------------------------#
#------             Association by Geographical Detector(GD)             ------#
#------------------------------------------------------------------------------#

source('./script/ssh_q.r')
q1 = ssh_q(data = npp.df,cause = "pre",effect = "npp")
q2 = ssh_q(data = npp.df,cause = "tem",effect = "npp")
q3 = ssh_q(data = npp.df,cause = "pre",effect = "tem")
qv = do.call(rbind,list(q1,q2,q3))
qv
readr::write_rds(qv,'./result/case/gd_case3.rds')