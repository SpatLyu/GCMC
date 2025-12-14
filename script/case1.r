#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~      Case: Columbus,OH Housing Value     ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-12-15    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(spEDM)
columbus = sf::read_sf(system.file("case/columbus.gpkg", package="spEDM"))
columbus

#-----------------------------------------------------------------------------#
#------            Determining minimum embedding dimension              ------#
#-----------------------------------------------------------------------------#

spEDM::fnn(columbus, "hoval", E = 1:10, eps = stats::sd(columbus$hoval) / 10)

#------------------------------------------------------------------------------#
#------    Causation by Geographical Cross Mapping Cardinality (GCMC)    ------#
#------------------------------------------------------------------------------#

ceiling(sqrt(7 * nrow(columbus)))

# housing value and crime (residential burglaries and vehicle thefts)
g1 = gcmc(columbus, "hoval", "crime", E = 7, k = 19)
g1

# household income and crime (residential burglaries and vehicle thefts)
g2 = gcmc(columbus, "inc", "crime", E = 7, k = 19)
g2

# housing value and household income
g3 = gcmc(columbus, "hoval", "inc", E = 7, k = 19)
g3

gcmc_case1 = list(g1,g2,g3)
readr::write_rds(gcmc_case1,'./result/case/gcmc_case1.rds')

#------------------------------------------------------------------------------#
#------    Causation by Geographical Convergent Cross Mapping (GCCM)     ------#
#------------------------------------------------------------------------------#

# housing value and crime (residential burglaries and vehicle thefts)
g1 = gccm(columbus, "hoval", "crime",  E = 7, k = 9)
g1

# household income and crime (residential burglaries and vehicle thefts)
g2 = gccm(columbus, "inc", "crime", E = 7, k = 9)
g2

# housing value and household income
g3 = gccm(columbus, "hoval", "inc", E = 7, k = 9)
g3

gccm_case1 = list(g1,g2,g3)
readr::write_rds(gccm_case1,'./result/case/gccm_case1.rds')

#------------------------------------------------------------------------------#
#------        Correlation by Pearson Correlation Coefficient(PCC)       ------#
#------------------------------------------------------------------------------#

columdf = sf::st_drop_geometry(dplyr::select(columbus,c(hoval,inc,crime)))
pcc = psych::corr.test(columdf)
pcc
readr::write_rds(pcc,'./result/case/pcc_case1.rds')

#------------------------------------------------------------------------------#
#------             Association by Geographical Detector(GD)             ------#
#------------------------------------------------------------------------------#

source('./script/ssh_q.r')
q1 = ssh_q(data = columdf, cause = "hoval", effect = "crime")
q2 = ssh_q(data = columdf, cause = "inc", effect = "crime")
q3 = ssh_q(data = columdf, cause = "hoval", effect = "inc")
qv = do.call(rbind,list(q1,q2,q3))
qv
readr::write_rds(qv,'./result/case/gd_case1.rds')
