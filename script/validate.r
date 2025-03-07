library(terra)

# load data
npp = terra::rast('./npp.tif')
terra::global(npp[["npp"]],"notNA")

# sample 2000 points to balance computational accuracy and processing time.
strata = sgsR::strat_quantiles(npp[["npp"]],nStrata = 5,
                               plot = TRUE, map = TRUE)
set.seed(2004)
sam = sgsR::sample_strat(strata,nSamp = 2000,force = TRUE) |> 
  sdsfun::sf_coordinates()
predindice = terra::rowColFromCell(npp,terra::cellFromXY(npp,sam))

# construct the parameter sets for running GCMC
Es = c(3,3,3,3,5)
names(Es) = names(npp)
vars = utils::combn(names(npp),2,simplify = TRUE)
params = data.frame(
  cause = vars[1,],
  effect = vars[2,],
  Ex = Es[vars[1,]],
  Ey = Es[vars[2,]],
  k = 450,
  r = 0,
  trend.rm = c(rep(FALSE,length.out = length(vars[1,]) - 1),TRUE) # remove the linear trend when running for NPP and elevation.
)

npp_gcmc = data.frame()
for (v in 1:nrow(params)) {
  g = spEDM::gcmc(data = npp,
                  cause = params[v,"cause",drop = TRUE],
                  effect = params[v,"effect",drop = TRUE],
                  E = c(params[v,"Ex",drop = TRUE],params[v,"Ey",drop = TRUE]),
                  k = params[v,"k",drop = TRUE],
                  r = params[v,"r",drop = TRUE],
                  pred = predindice,
                  trend.rm = params[v,"trend.rm",drop = TRUE],
                  progressbar = TRUE)
  tempdf = g$xmap
  tempdf$x = params[v,"cause",drop = TRUE]
  tempdf$y = params[v,"effect",drop = TRUE]
  npp_gcmc = rbind(npp_gcmc,tempdf)
}
readr::write_csv(npp_gcmc,'./npp_gcmc.csv')

g = spEDM::gccm(data = npp,
            cause = "pre",
            effect = "tem",
            libsizes = as.matrix(expand.grid(seq(10,150,10),seq(20,340,20))),
            E = 3,
            k = 5,
            pred = predindice,
            trend.rm = TRUE,
            progressbar = TRUE)
g1 = spEDM::gcmc(data = npp,
                cause = "pre",
                effect = "elev",
                E = 3,
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = FALSE,
                progressbar = TRUE)
g2 = spEDM::gcmc(data = npp,
                cause = "tem",
                effect = "sm",
                E = 3,
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = FALSE,
                progressbar = TRUE)
g3 = spEDM::gcmc(data = npp,
                cause = "npp",
                effect = "elev",
                E = c(5,3),
                k = 450,
                r = 0,
                pred = predindice,
                trend.rm = FALSE,
                progressbar = TRUE)